# Script to update the weather data used for FluMoDL


# Read in options to run the script
source("options.R")

# **** NO MODIFICATION BELOW THIS LINE ****

if (course_of_action==4) {
  cat(sprintf("Running your custom script %s% to create/update temperatures data...\n\n", custom_script))
  source(custom_script)

} else {  

  # Error checking
  if (is.na(end_date)) end_date <- Sys.Date()
  start_date <- as.Date(start_date)
  if (is.na(start_date)) stop("Invalid start date, sorry...\n\n")
  if (is.na(end_date)) stop("Invalid end date, sorry...\n\n")
  if (start_date>=end_date) stop("Start date should be earlier than end date, sorry...\n\n")
  
  library(FluMoDL)
  
  if (course_of_action==1) {
    if (!file.exists(paste0("input/wdata_",country_code,".txt")))
        stop("FluMOMO temperatures file '%s' not found in /input, sorry.\n\n")
      # Load temperatures file and aggregate
    wdata <- read.csv2(paste0("input/wdata_",country_code,".txt"))
    wdata$date <- as.Date(wdata$date)
    wdata <- subset(wdata, date>=start_date & date<=end_date)
    if (nrow(wdata)==0) stop("No data found in file for the requested period! Sorry...\n\n")
    wdata$NUTS3 <- as.character(wdata$NUTS3)
    wdata$temp <- as.numeric(as.character(wdata$temp))
    wdata <- subset(wdata, !is.na(temp))
    temp_all <- wdata[,c("temp","pop3","date","NUTS3")]
    names(temp_all)[c(2,4)] <- c("pop","nuts3")


  } else if (course_of_action %in% 2:3) {
    years <- as.integer(format.Date(start_date, "%Y")):as.integer(format.Date(end_date, "%Y"))
    if (course_of_action==3) {
      if (file.exists("input/temp_all.RData")) {
        load("input/temp_all.RData")
        temp_all_prev <- temp_all
        existing_years <- sort(as.integer(unique(format.Date(temp_all$date, "%Y"))))
        mY <- as.integer(format.Date(Sys.Date(), "%Y"))
        existing_years <- existing_years[existing_years != mY]
        if (as.integer(format.Date(Sys.Date(), "%m"))<4)
          existing_years <- existing_years[existing_years != mY-1]
        years <- years[!(years %in% existing_years)]
      } else {
        cat("No file 'input/temp_all.RData' found! Downloading all data from NOAA...\n")
        course_of_action <- 2
      }
    }
    if (course_of_action==2) {
      load("input/europe_map.RData")
      map <- subset(europe, level==3 & nuts0==substr(country_code, 1, 2))
      if (nchar(country_code)==3) {
        map <- subset(map, nuts1==country_code)
      }
        # Get weather stations from NOAA
      sites <- NOAA_countryStations(europe$fips[match(substr(country_code,1,2), europe$nuts0)])
      coordinates(sites) <- ~lon+lat
      sites$nuts3 <- map$nuts3[over(sites, SpatialPolygons(map@polygons))]
      sites <- subset(sites, !is.na(nuts3))
      if (nrow(sites)==0) stop("No weather stations found for this region, sorry...")
      sites$pop <- europe$pop[match(sites$nuts3, europe$nuts3)]
    }
      # Get the weather data
    cat("Downloading weather data (please be patient)...\n")
    temp_all <- list()
    for(y in years) {
      cat(sprintf("Year: %s\n", y))
        temp_all[[as.character(y)]] <- NOAA_getGSOD(sites@data, y, match.columns=c("station.name","nuts3", "pop"))
    }
    temp_all <- do.call(rbind, temp_all)
    if (course_of_action==3) {
      temp_all <- rbind(temp_all_prev, temp_all)
      temp_all <- temp_all[!duplicated(temp_all),]
      temp_all <- temp_all[!rev(duplicated(temp_all[nrow(temp_all):1,c("date","station.name")])),]
    }
    save(temp_all, sites, file="input/temp_all.RData")
  }



  cat("Aggregating (please wait)... ")
  aggr <- aggregate(temp_all[,c("temp","pop")], temp_all[,c("date","nuts3")], mean, na.rm=TRUE) 
  aggr <- c(by(aggr, aggr[,"date", drop=FALSE], function(x) 
      with(x, sum(temp*pop)/sum(pop))))
  cat("OK\n")
  aggr <- data.frame(date=as.Date(names(aggr)), temp=unname(aggr))
  mdTemp <- data.frame(date=seq.Date(min(aggr$date), max(aggr$date), by="days"))
  mdTemp$temp <- aggr$temp[match(mdTemp$date, aggr$date)]
    # Check length of missing segments
  posNA <- which(is.na(mdTemp$temp)) # Find all NAs
  startNA <- posNA[c(2,diff(posNA))>1] # Find the start of any consecutive NA segments
  endNA <- rev(rev(posNA)[c(-2, diff(rev(posNA)))<(-1)]) # ...and the end
  dist <- endNA - startNA + 1
  if (length(dist)>1 && max(dist)>3)
      cat(sprintf("Found segment(s) with consecutive missing temperatures up to %s days.\nInterpolating, but note that this is not good...\n\n", max(dist)))
  mdTemp$temp <- linterp(mdTemp$temp, max_allow=NULL)
  file.copy("input/mdTemp.RData", "input/mdTemp.RData.bak", overwrite=TRUE)
  save(mdTemp, file="input/mdTemp.RData")
  cat("Finished!\n\n")

}




