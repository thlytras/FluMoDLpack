# **** Basic common options ****

# Insert your 2-letter (or 3-letter) NUTS code 
country_code <- "EL"

# Start date for analysis
start_date <- "2013-5-27"

# End date for analysis: if NA, the current date will be used
end_date <- NA

# Input/output directories (should you need to change them)
input_dir <- "./input"
output_dir <- "./output"


# **** FluMoDL-specific options ****

# For which period to calculate WEEKLY influenza-attributable mortality estimates?
# Can be one of the following:
# -- a vector of start and end weeks, YYYYWW format (e.g. c(201501, 201510))
# -- a year indicating the influenza season, e.g. 2014 for the season 2014-15
# -- the string "current", to calculate for the current influenza season
# -- the string "all", to calculate for ALL available weeks in the data (can take a long time)
# -- NULL, to not calculate any weekly influenza-attributable mortality estimates
week_period <- "current"

# Number of Monte-Carlo iterations to calculate attributable mortality 95%CIs,
# for weekly and seasonal mortality estimates.
# Increase if you have a faster computer (or if you like waiting). 
# Set to 0 if you're not interested in 95%CIs.
mcIter_weekly <- 5000
mcIter_seasonal <- 5000

# Save Monte-Carlo samples for weekly attributable mortalities?
# This is useful to combine attributable mortality estimates (with other countries,
# or in different periods for the same country) and calculate new 95% CIs
# This file can get very large if there are many weeks or lots of iterations,
# thus default is FALSE.
mcsamples_save <- FALSE



# *** update_weather.R specific options ****

# Select what to do (1-4)
# Options are:
# 1 - Use FluMOMO temperatures data (file should be in /input)
# 2 - Download from NOAA, for the entire period
# 3 - Use existing /input/temp_all.RData file, and update it from NOAA
# 4 - Run a fully custom script to create/update temperatures file
course_of_action <- 3

# (Full) pathname for a custom script (used if course_of_action == 4)
custom_script <- ""
 
