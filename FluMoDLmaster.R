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
cat("\n")
load(sprintf("%s/mdTemp.RData", input_dir))

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


cat("Setting up output directories...\n")
outdir <- sprintf("%s/FluMoDL-%s-0.1-%s", output_dir,
    country_code, format(Sys.time(), "%Y%m%d"))
suppressWarnings(dir.create(outdir))
suppressWarnings(dir.create(sprintf("%s/figures", outdir)))
suppressWarnings(dir.create(sprintf("%s/figures/pdf", outdir)))
suppressWarnings(dir.create(sprintf("%s/figures/png", outdir)))


summaries <- lapply(models, summary)
for (i in 1:length(summaries)) {
  summaries[[i]]$description <- sprintf("First-stage summary: %s, %s", 
    country_code, format(Sys.Date(), "%Y%m%d"))
}
cat("Saving models and summaries...\n")
save(models, file=sprintf("%s/models.RData", outdir))
save(summaries, file=sprintf("%s/summaries.RData", outdir))



colsFlu <- c("H1" = "steelblue", "H3" = "lawngreen", "B" = "orange", "temp"="orchid")
addalpha <- function(col, alpha) {
  rgbn <- col2rgb(col)
  rgb(red=rgbn["red",], green=rgbn["green",], blue=rgbn["blue",], alpha=alpha, max=255)
}
colsFluTr <- addalpha(colsFlu, 70)
names(colsFluTr) <- names(colsFlu)

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



cat("\nCalculating seasonal estimates for all seasons...\n")
attrMort_seasonal <- lapply(ageGroups, function(a) {
  cat(sprintf("Calculating for group %s...\n", a))
  attrMort(models[[a]], sel="season", ci=(mcIter_seasonal!=0), nsim=mcIter_seasonal)
})
names(attrMort_seasonal) <- ageGroups



if (!is.null(week_period)) {
cat("\nCalculating weekly estimates as requested...\n")
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
  mcsamples_weekly <- lapply(ageGroups, function(a) {
    cat(sprintf("Calculating for group %s...\n", a))
    args$m <- models[[a]]
    do.call(attrMort, args)
  })
  names(mcsamples_weekly) <- ageGroups
  
  
  attrMort_weekly <- lapply(mcsamples_weekly, function(x) x$result)
  for (g in names(attrMort_weekly)) {
    for (p in names(mcsamples_weekly[[1]][[1]])) {
      attrMort_weekly[[g]][[paste0(p, ".lo")]] <- as.integer(sapply(mcsamples_weekly[[g]]$mcsamples, function(x) quantile(x[[p]], 0.025, na.rm=TRUE)))
      attrMort_weekly[[g]][[paste0(p, ".hi")]] <- as.integer(sapply(mcsamples_weekly[[g]]$mcsamples, function(x) quantile(x[[p]], 0.975, na.rm=TRUE)))
    }
    attrMort_weekly[[g]] <- attrMort_weekly[[g]][,names(attrMort_seasonal$All)]
  }

  save(attrMort_weekly, attrMort_seasonal, file=sprintf("%s/attrMort.RData", outdir))
  if (mcsamples_save) {
    save(mcsamples_weekly, file=sprintf("%s/mcsamples_weekly.RData", outdir))
  }
} else {
  save(attrMort_seasonal, file=sprintf("%s/attrMort.RData", outdir))
} 



