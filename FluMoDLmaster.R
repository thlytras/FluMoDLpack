# Script to run a standard FluMoDL analysis
# See the associated manual for details


# Read in options to run the script
source("options.R")


# Load the deaths file. 
# Adapt as needed if you have some other way of providing the deaths file.
# See manual for details.
load(sprintf("%s/deaths.RData", input_dir))


# Load the weekly ILI & virological file. 
# Adapt as needed if you have some other way of providing these data.
# See manual for details.
load(sprintf("%s/weekly.RData", input_dir))


# **** NO MODIFICATION BELOW THIS LINE ****

cat("Loading packages...\n")
library(FluMoDL)
library(plotrix)
cat("\n")
load(sprintf("%s/mdTemp.RData", input_dir))

# Setting up input data

daily <- mdTemp; rm(mdTemp)
daily <- subset(daily, date>=start_date)
ageGroups <- c("All","65","1564","0514","04")
ageGroupsLab <- c("All" = "All ages", "65" = ">=65 years", "1564" = "15-64 years", "0514" = "5-14 years", "04" = "0-4 years")

load(sprintf("%s/weekly.RData", input_dir))
if (!("proxyH1" %in% names(weekly))) weekly$proxyH1 <- with(weekly, ILI*ppH1)
if (!("proxyH3" %in% names(weekly))) weekly$proxyH3 <- with(weekly, ILI*ppH3)
if (!("proxyB" %in% names(weekly))) weekly$proxyB <- with(weekly, ILI*ppB)
if (sum(paste0("proxy",c("H1","H3","B")) %in% names(weekly))!=3)
  stop("Did not find expected columns in the `weekly` data.frame, sorry...\nPlease check documentation and format it accordingly.")
weekly[,paste0("proxy", c("H1","H3","B"))][is.na(weekly[,paste0("proxy", c("H1","H3","B"))])] <- 0

# Aggregating deaths if needed
if (sum(c("age","DoD") %in% names(deaths))==2) {
  cat("Aggregating deaths by date...\n")
  daily$diedAll <- with(deaths, 
    tapply(DoD, DoD, length))[as.character(daily$date)]
  daily$died04 <- with(subset(deaths, age<=4), 
    tapply(DoD, DoD, length))[as.character(daily$date)]
  daily$died0514 <- with(subset(deaths, age>=5 & age <=14), 
    tapply(DoD, DoD, length))[as.character(daily$date)]
  daily$died1564 <- with(subset(deaths, age>=15 & age<=64), 
    tapply(DoD, DoD, length))[as.character(daily$date)]
  daily$died65 <- with(subset(deaths, age>=65), 
    tapply(DoD, DoD, length))[as.character(daily$date)]
} else if (sum(c("date", paste("died",ageGroups)) %in% names(deaths))==6) {
  daily <- merge(daily, deaths[,c("date",paste("died",ageGroups))], by="date", all.x=TRUE)
} else {
  stop("Did not find expected columns in the `deaths` data.frame, sorry...\nPlease check documentation and format it accordingly.")
}
daily[,paste0("died",ageGroups)][is.na(daily[,paste0("died",ageGroups)])] <- 0


# Now doing the actual fitting of the models, by age group

cat("Fitting FluMoDLs...\n")
periodic <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
names(periodic) <- ageGroups
models <- lapply(ageGroups, function(a)
  fitFluMoDL(deaths = daily[[paste0("died",a)]],
             temp = daily$temp, dates = daily$date,
             proxyH1 = weekly$proxyH1, proxyH3 = weekly$proxyH3,
             proxyB = weekly$proxyB, yearweek = weekly$yearweek,
             periodic = periodic[a]))
names(models) <- ageGroups


# Setting up the output directories
cat("Setting up output directories...\n")
outdir <- sprintf("%s/FluMoDL-%s-0.1-%s", output_dir,
    country_code, format(Sys.time(), "%Y%m%d"))
suppressWarnings(dir.create(outdir))
suppressWarnings(dir.create(sprintf("%s/figures", outdir)))
suppressWarnings(dir.create(sprintf("%s/figures/pdf", outdir)))
suppressWarnings(dir.create(sprintf("%s/figures/png", outdir)))


# Summarizing the FluMoDLs, and saving both the models and summaries
summaries <- lapply(models, summary)
for (i in 1:length(summaries)) {
  summaries[[i]]$description <- sprintf("First-stage summary: %s, %s", 
    country_code, format(Sys.Date(), "%Y%m%d"))
}
cat("Saving models and summaries...\n")
save(models, file=sprintf("%s/models.RData", outdir))
save(summaries, file=sprintf("%s/summaries.RData", outdir))


# Set up some color palettes
colsFlu <- c("H1" = "steelblue", "H3" = "lawngreen", "B" = "orange", "temp"="orchid")
addalpha <- function(col, alpha) {
  rgbn <- col2rgb(col)
  rgb(red=rgbn["red",], green=rgbn["green",], blue=rgbn["blue",], alpha=alpha, max=255)
}
colsFluTr <- addalpha(colsFlu, 70)
names(colsFluTr) <- names(colsFlu)


# Helper function to plot influenza-/temperature-mortality associations
plotAssoc <- function(obj, group, proxy, title=TRUE, ylim=NULL) {
  proxyT <- c("H1" = "Influenza A(H1N1)pdm09", "H3" = "Influenza A(H3N2)", "B" = "Influenza B", "temp"="Mean daily temperature")
  args <- list(x=obj[[group]]$pred[[paste0(ifelse(proxy!="temp","proxy",""),proxy)]], 
    ptype="overall", ylab="Relative Risk", xlab="Incidence Proxy",
    lwd=2, col=colsFlu[proxy], ci.arg=list(col=colsFluTr[proxy]))
  if (!is.null(ylim)) args$ylim <- ylim
  do.call(plot.crosspred, args)
  if (title) mtext(sprintf("%s, reg: %s, group: %s", 
    proxyT[proxy], country_code, group), side=3, line=2, cex=1.1, font=2)
  return(par("yaxp")[1:2])
}


# Now plotting some output...

# Plot influenza-/temperature-mortality associations (individual plots)
ylims <- rep(list(list("H1"=numeric(), "H3"=numeric(), "B"=numeric(), "temp"=numeric())),5)
names(ylims) <- ageGroups
for (g in ageGroups) {
  for (p in c("H1","H3","B","temp")) {
    cairo_pdf(sprintf("%s/figures/pdf/assoc-%s-%s-%s.pdf", 
        outdir, country_code, g, p), width=8, height=6, pointsize=14)
    ylims[[g]][[p]] <- c(ylims[[g]][[p]], plotAssoc(models, g, p))
    dev.off()
    png(sprintf("%s/figures/png/assoc-%s-%s-%s.png", 
        outdir, country_code, g, p), width=800, height=600, res=120)
    plotAssoc(models, g, p)
    dev.off()
  }
}

# Plot PDF report on flu-mortality associations
cairo_pdf(sprintf("%s/assoc-flu.pdf", outdir), width=9, height=13)
par(mfrow=c(5,3), oma=c(2,4,8.5,0), mar=c(3,4,3,2))
for (g in ageGroups) {
  for (p in c("H1","H3","B")) {
    plotAssoc(models, g, p, title=FALSE, ylim=range(unlist(ylims[[g]][1:3])))
    if (g=="All") {
      mtext(c("H1" = "A(H1N1)pdm09", "H3" = "A(H3N2)", "B" = "B")[p], side=3, xpd=NA, cex=1.2, font=2, line=2)
    }
    if (p=="H1") {
      mtext(ageGroupsLab[g], side=2, xpd=NA, cex=1.2, font=2, line=5, las=3)
    }
  }
}
mtext(sprintf("Influenza-mortality associations. Region: %s", country_code), side=3, outer=TRUE, font=2, line=4.5, cex=1.4)
mtext(sprintf("Data period: from %s to %s", min(models$All$data$dates), max(models$All$data$dates)), side=3, outer=TRUE, line=2.5, cex=1.2)
dev.off()

# Plot PDF report on temperature-mortality associations
cairo_pdf(sprintf("%s/assoc-temp.pdf", outdir), width=7, height=8, pointsize=14)
par(mfrow=c(3,2), oma=c(1,1.5,4,0), mar=c(4.5,4,3.5,2))
for (g in ageGroups[c(1,10,2:5)]) {
  if (is.na(g)) {
    plot(0, type="n", axes=FALSE, ylab=NA, xlab=NA)
    mmpstr <- paste(rbind(ageGroupsLab, ": ", sapply(models, function(x) x$MMP), "\n"), collapse="")
    text(1,0,mmpstr, cex=1.1)
    mtext("Minimum Mortality Temperatures", font=3, side=3, cex=0.8)
  } else {
    yl <- ylims[[g]]$temp
    yl <- c(max(0.5,min(yl)), min(20,max(yl)))
    plotAssoc(models, g, "temp", title=FALSE, ylim=yl)
    abline(v=models[[g]]$MMP, lty="dotted")
    mtext(ageGroupsLab[g], side=3, line=0.5, font=2, cex=0.85)
  }
}
mtext(sprintf("Temperature-mortality associations. Region: %s", country_code), side=3, outer=TRUE, font=2, line=1, cex=1.1)
mtext(sprintf("Data period: from %s to %s", min(models$All$data$dates), max(models$All$data$dates)), side=3, outer=TRUE, line=-0.5, cex=0.95)
dev.off()



# Now calculating attributable mortalities
# This is the part that takes a lot of time (unless you've suppressed Monte-Carlo 
# 95%CIs by setting `mcIter_seasonal` and/or `mcIter_weekly` to zero in `options.R`)


# Calculate attributable mortalities per season
cat("\nCalculating attributable mortality estimates for all seasons...\n")
attrMort_seasonal <- lapply(ageGroups, function(a) {
  cat(sprintf("Calculating for group %s...\n", a))
  attrMort(models[[a]], sel="season", ci=(mcIter_seasonal!=0), nsim=mcIter_seasonal)
})
names(attrMort_seasonal) <- ageGroups


# Calculate attributable mortalities per week 
# (can take a long time, especially if week_period == "all")
if (!is.null(week_period)) {
cat("\nCalculating weekly attributable mortality estimates as requested...\n")
  args <- list(sel="week", ci=(mcIter_weekly!=0), nsim=mcIter_weekly, mcsamples=TRUE)
  if (length(week_period)==2 && is.numeric(week_period)) {
    args$from <- week_period[1]; args$to <- week_period[2]
  } else if (length(week_period)==1 && week_period=="all") {
    # Nothing to add to `args`
  } else if (length(week_period)==1 && week_period=="current") {
    curw <- isoweek(rev(models$All$data$date)[1])
    curw <- ((curw%/%100)-(curw%%100<40))*100+c(40,120)
    args$from <- curw[1]; args$to <- curw[2]  
  } else {
    stop("Invalid input in option `week_period`, don't know what to do, sorry...\n")
  }
  # We've set mcsamples=TRUE, to *keep* the Monte-Carlo samples...
  mcsamples_weekly <- lapply(ageGroups, function(a) {
    cat(sprintf("Calculating for group %s...\n", a))
    args$m <- models[[a]]
    do.call(attrMort, args)
  })
  names(mcsamples_weekly) <- ageGroups
  # Manually calculate 95% CIs, based on the Monte-Carlo samples
  # (would be simpler to do automatically, by setting ci=TRUE in attrMort(),
  #  BUT we want to keep the Monte-Carlo samples)
  attrMort_weekly <- lapply(mcsamples_weekly, function(x) x$result)
  for (g in names(attrMort_weekly)) {
    for (p in names(mcsamples_weekly[[1]][[1]])) {
      attrMort_weekly[[g]][[paste0(p, ".lo")]] <- as.integer(sapply(mcsamples_weekly[[g]]$mcsamples, function(x) quantile(x[[p]], 0.025, na.rm=TRUE)))
      attrMort_weekly[[g]][[paste0(p, ".hi")]] <- as.integer(sapply(mcsamples_weekly[[g]]$mcsamples, function(x) quantile(x[[p]], 0.975, na.rm=TRUE)))
    }
    attrMort_weekly[[g]] <- attrMort_weekly[[g]][,names(attrMort_seasonal$All)]
  }

  # Save the attributable mortality estimates (including the seasonal estimates)
  save(attrMort_weekly, attrMort_seasonal, file=sprintf("%s/attrMort.RData", outdir))
  if (mcsamples_save) {
    save(mcsamples_weekly, file=sprintf("%s/mcsamples_weekly.RData", outdir))
  }
} else {
  save(attrMort_seasonal, file=sprintf("%s/attrMort.RData", outdir))
} 


# Helper function to plot influenza-attributable mortalities per season
plotInflAttrSeasonal <- function(x) {
  if ("AllFlu.hi" %in% names(x)) {
    ylim <- range(pretty(c(min(0, x$AllFlu.lo), x$AllFlu.hi)))
  } else {
    ylim <- range(pretty(c(min(0, x$AllFlu), x$AllFlu)))
  }
  bp <- barplot(t(x[,c("FluH1","FluH3","FluB")]), col=colsFlu[1:3], border=NA, ylim=ylim)
  if ("AllFlu.hi" %in% names(x)) {
    plotCI(x=bp, y=x$AllFlu, li=x$AllFlu.lo, ui=x$AllFlu.hi, lwd=2, sfrac=0.005, cex=0.01, add=TRUE)
  }
}

# Plot influenza-attributable mortalities by season (individual plots)
lapply(ageGroups, function(a) {
    x <- tail(attrMort_seasonal[[a]], 7)
    rownames(x) <- paste(rownames(x), as.integer(rownames(x))+1, sep="-")
    cairo_pdf(sprintf("%s/figures/pdf/inflAttrSeasonal-%s-%s.pdf", 
        outdir, country_code, a), width=8, height=6, pointsize=13)
    plotInflAttrSeasonal(x)
    mtext(sprintf("Influenza-attributable mortality. Region: %s, age group: %s", 
        country_code, ageGroupsLab[[a]]), side=3, line=1, cex=1.1, font=2)
    dev.off()
    png(sprintf("%s/figures/png/inflAttrSeasonal-%s-%s.png", 
        outdir, country_code, ageGroupsLab[[a]]), width=800, height=600, res=100)
    plotInflAttrSeasonal(x)
    mtext(sprintf("Influenza-attributable mortality. Region: %s, age group: %s", 
        country_code, a), side=3, line=1, cex=1.1, font=2)
    dev.off()
})

# Plot PDF report on flu-attributable mortalities by season
cairo_pdf(sprintf("%s/attrMort-seasonal.pdf", outdir), width=9, height=14, pointsize=14)
par(mfrow=c(5,1), oma=c(2,2,10,0), mar=c(3,5,3,2))
lapply(ageGroups, function(a) {
    x <- tail(attrMort_seasonal[[a]], 7)
    rownames(x) <- paste(rownames(x), as.integer(rownames(x))+1, sep="-")
    plotInflAttrSeasonal(x)
    mtext(sprintf("Age group: %s", ageGroupsLab[[a]]), side=3, line=1, cex=0.9, font=2)
    mtext("Number of deaths", side=2, line=3, cex=0.8)
  if (a=="All") 
    legend("top", c("A(H1N1)pdm09", "A(H3N2)", "B"), col=colsFlu, 
        pch=15, pt.cex=2.5, horiz=TRUE, xpd=NA, inset=c(0,-0.6), bty="n", cex=1.3)
})
mtext(sprintf("Influenza-attributable mortality by season. Region: %s", country_code), side=3, outer=TRUE, font=2, line=6, cex=1.4)
mtext(sprintf("Data period: from %s to %s", min(models$All$data$dates), max(models$All$data$dates)), side=3, outer=TRUE, line=4, cex=1.2)
dev.off()


# Helper function to plot influenza-attributable mortalities per week
plotInflAttrWeekly <- function(a) {
    x <- tail(attrMort_weekly[[a]][,c("AllFlu","Temp")], 350)
    x$deaths <- with(models[[a]]$data, 
        tapply(deaths, yearweek, sum, na.rm=TRUE))[rownames(x)]
    x$baseline <- predict(models[[a]], temp="MMP", 
        proxyH1=0, proxyH3=0, proxyB=0, byWeek=TRUE)[rownames(x)]
    x$weekL <- as.integer(rownames(x))
    x$AllFlu[is.nan(x$AllFlu) | is.na(x$AllFlu)] <- 0
    x$Temp[is.nan(x$Temp) | is.na(x$Temp)] <- 0
    if (nrow(x)>50) {
      x$weekL[!((x$weekL %% 100) %in% c(1,20,40))] <- NA
    }
    plot(x$deaths, type="l", ylab="Number of deaths", bty="l", xlab=NA, xaxt="n",
      col="mediumpurple4", lwd=1)
    axis(1, at=which(!is.na(x$weekL)), labels=x$weekL[!is.na(x$weekL)], las=3)
    polygon(x=c(1:nrow(x),nrow(x):1),
        y=c(x$deaths-x$AllFlu-x$Temp, rev(x$deaths-x$AllFlu)),
        border=NA, col="skyblue1")
    polygon(x=c(1:nrow(x),nrow(x):1),
        y=c(x$deaths-x$AllFlu, rev(x$deaths)),
        border=NA, col="violetred1")
    points(x$baseline, type="l", col="darkorange", lwd=1)
    points(x$deaths, type="l", col="mediumpurple4", lwd=1)
}

# Plot influenza-attributable mortalities by season (individual plots)
lapply(ageGroups, function(a) {
    cairo_pdf(sprintf("%s/figures/pdf/inflAttrWeekly-%s-%s.pdf", 
        outdir, country_code, a), width=13, height=6, pointsize=13)
    plotInflAttrWeekly(a)
    mtext(sprintf("Influenza-attributable mortality. Region: %s, age group: %s", 
        country_code, ageGroupsLab[[a]]), side=3, line=1, cex=1.1, font=2)
    dev.off()
    png(sprintf("%s/figures/png/inflAttrWeekly-%s-%s.png", 
        outdir, country_code, ageGroupsLab[[a]]), width=1300, height=600, res=100)
    plotInflAttrWeekly(a)
    mtext(sprintf("Influenza-attributable mortality. Region: %s, age group: %s", 
        country_code, a), side=3, line=1, cex=1.1, font=2)
    dev.off()
})

# Plot PDF report on flu-attributable mortalities by week
cairo_pdf(sprintf("%s/attrMort-weekly.pdf", outdir), width=9, height=14, pointsize=12)
par(mfrow=c(5,1), oma=c(4,2,10,0), mar=c(5,5,3,2))
lapply(ageGroups, function(a) {
    plotInflAttrWeekly(a)
    mtext(sprintf("Age group: %s", ageGroupsLab[[a]]), side=3, line=1, cex=0.9, font=2)
  if (a=="All") 
    legend("top", c("Influenza (all types)", "Cold temperatures (<MMP)"), 
        col=c("violetred1", "skyblue1"), 
        pch=15, pt.cex=2.5, horiz=TRUE, xpd=NA, inset=c(0,-0.55), bty="n", cex=1.3)
})
mtext(sprintf("Influenza-attributable mortality by week. Region: %s", country_code), side=3, outer=TRUE, font=2, line=6, cex=1.4)
mtext(sprintf("Data period: from %s to %s", min(models$All$data$dates), max(models$All$data$dates)), side=3, outer=TRUE, line=4, cex=1.2)
dev.off()

