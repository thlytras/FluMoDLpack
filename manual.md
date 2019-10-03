---
output:
  html_document: default
  pdf_document: default
---
## FluMoDLpack version 0.1


## Overview

This manual explains how to use FluMoDLpack in order to analyze 
influenza-attributable mortality using 
[FluMoDL](https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.14.1800118),
within the context of the [EuroMOMO network](http://www.euromomo.eu/).



### What FluMoDLpack is

FluMoDLpack is a set of R scripts that implements a standard set of analyses
of influenza-attributable mortality using FluMoDL. It relies on the 
[FluMoDL R package](https://cran.r-project.org/package=FluMoDL) to do the 
actual model fitting and analysis. Therefore, you need to have R packages 
`FluMoDL` and `plotrix` installed. 

The objective of FluMoDLpack is to facilitate the analyses and standardize the
output, so that EuroMOMO network members have a common frame of reference and 
can readily share information.



### How to run FluMoDLpack

First adapt the file `options.R`. This contains all configuration options, so
that you won't normally need to modify any other FluMoDLpack script. Each 
option has explanatory comments in the file; also see below for a full 
reference.

Then, format and put all your input files in `/input`; see below exactly what 
kind of input files FluMoDLpack expects to find, and how exactly to format it
(a couple of different options are possible). Specifically for the temperature
data, a configurable script `update_weather.R` is included to help you; run it
to update your temperature data.

Finally, run the script `FluMoDLpack.R` to do the actual analysis and create
all the output. Note that the script should be very quick, _except_ for the
Monte-Carlo calculation of 95% Confidence Intervals (CIs) for attributable 
mortality. If you have a slow computer, or you don't want to wait at all, 
turn off 95% CI calculation or reduce the number of Monte-Carlo iterations
(see `options.R`).



### What input does FluMoDLpack expect to find?

FluMoDLpack expects to find the following files in `/input`:

* A file named `mdTemp.RData` containing the mean daily temperatures. 
This file should contain a data.frame named `mdTemp` with two columns, 
named `date`(of class `Date`) and `temp` (numeric). 
The dates should be continuous, i.e. no missing dates, and also no missing 
temperatures. 
This file can be conveniently created by running the `update_weather.R` 
script, see details below.

* A deaths file named `deaths.RData`, containing _either_ individual or 
aggregated deaths data. This file should contain a data.frame named `deaths`.
If using individual data, `deaths` should contain two columns named `DoD` (of
class `Date`) and `age` (numeric), containing the date of death and age; 
this practically means that you can re-use your EuroMOMO deaths data file. 
If using aggregate data, `deaths` should contain columns named `date` (of 
class `Date`), `diedAll`, `died04`, `died0514`, `died1564` and `died65`, 
referring to the date, the total number of deaths for that date, and the
number of deaths for the 0-4, 5-14, 15-64 and >=65 years age groups.

* A file named `weekly.RData`, containing your weekly ILI and virological
data. This file should contain a data.frame named `weekly`; this should 
include a column named `yearweek` (integer, in YYYYWW format), and _either_
columns named `ILI`, `ppH1`, `ppH3` and `ppB` (with the ILI rate and 
percentages positive to H1N1, H3N2 and B) _or_ columns named `proxyH1`, 
`proxyH3` and ` proxyB` (if you want to specify your proxies yourself).



### How exactly do you set up and update the weather data?

You set the required options inall `options.R`, and then you run the 
`update_weather.R` script. The most important option is `course_of_action`,
which can take values from 1 to 4. There are essentially three options:

1. You can directly use the EuroMOMO weather data available in 
http://www.euromomo.eu/methods/weather/weather.php. 
Download your respective .txt file, place it in `/input`, and set 
`course_of_action <- 1` in `options.R`
The 2-letter NUTS code in the file name must match what you've set in option
`country_code` in the `options.R` file.

2. You can have the script automatically download the appropriate NOAA data for
your region. By setting `course_of_action <- 2`, the script downloads all the 
weather station data from NOAA for your region and for the appropriate years, 
and saves those to `/input/temp_all.RData`. If you set `course_of_action <- 3`, 
then any weather data previously saved in `/input/temp_all.RData` are kept, and 
only the data for the last year are downloaded from NOAA (or last two years, 
if we're between January and March); this saves time, and keeps any previously
downloaded data from being deleted if NOAA drops some past records due to QA
issues (has happened in the past).

   In both cases (EuroMOMO data or freshly-downloaded NOAA data), aggregation 
is automatically done by NUTS3 region, and the information in 
`/input/europe_map.RData` (see below for details) is used for that purpose.
The NUTS3 region that corresponds to each weather station is identified, the 
mean daily temperature for each NUTS3 region is calculated, and then a 
population-weighted mean over all NUTS3 regions is calculated. In the end, 
the mean daily temperatures are written in the file `/input/mdTemp.RData`, 
which is the input file used by the main script (`FluMoDLmaster.R`). This file
contains a data.frame named `mdTemp`, with two columns: `date` (of class 
`Date`) and `temp` (numeric).

3. If you want to use another data source (not NOAA) or some other way to 
aggregate or otherwise handle your weather data, you can write your own custom
script and have it called via `update_weather.R`. Go to `options.R`, set 
`course_of_action <- 4` and give the full pathname of your script to the 
`custom_script` option. Your script should produce as output the file
`/input/mdTemp.RData` exactly as described above.



### What does the `FluMoDLmaster.R` script do?

It reads in all the input, and fits the FluMoDL both overall (for all ages) 
and by age group: >=65 years, 15-64 years, 5-14 years and 0-4 years. The age
groups are the same as in EuroMOMO, and are not configurable in this version
of FluMoDLpack.

Subsequently the script calculates influenza- and temperature- attributable 
mortality aggregated by season, and optionally by week. This depends on the
option `week_period` in `options.R`. This can take one of the following values:

* A vector of start and end weeks, in YYYWW format, to specifically select a
range for which weekly attributable mortalities should be calculated.

* A year indicating a single influenza season, e.g. 2014, which will calculate
weekly mortalities between week 201440 and 201520.

* The string "current", to select the current influenza season

* The string "all", to calculate weekly attributable mortalities for ALL weeks
included in the data. Note that this can take a long time.

* `NULL` to not calculate any weekly attributable mortalities.



### What output does FluMoDLpack create?

The script writes in `/output`, creating a sub-directory named `FluMoDL-X-Y-Z` 
where X is your NUTS code, Y is the FluMoDLpack version and Z is the current 
date (in YYYYMMDD format).

Inside this sub-directory, the following output is created:

* A file `models.RData` holding the fitted FluMoDLs. This contains a list 
(named `models`) of `FluMoDL`-class objects, overall and by age group. 
Note that objects of class `FluMoDL` include all your input data, and thus 
*should NOT be shared* with other parties if your data are confidential.

* A file `summaries.RData` containing a list of FluMoDL summaries (of class
`summary.FluMoDL`), overall and by age group. Note that these objects do not
contain any of your data, just regression coefficients for the influenza
activity proxies, and thus *can be freely shared*, in order e.g. to run a 
multivariate meta-analysis and calculate BLUP estimates.

* A file `attrMort.RData` containing attributable mortalities (to influenza
and cold temperatures) both per season (data.frame `attrMort_seasonal`) and,
if requested, per week (data.frame `attrMort_weekly`). 

* A PDF file `assoc-flu.pdf` illustrating the overall influenza-mortality 
associations by activity proxy, overall and by age groups. Note that the 
individual plots from this small report can also be found inside a `figures`
directory in the output sub-directory, in both PNG and PDF format. Also note
that the information to build these exact plots is precisely what the file 
`summaries.RData` contains (and nothing else).

* A PDF file `assoc-temp.pdf` illustrating the temperature-mortality 
associations, overall and by age groups. The individual plots from this 
report can also be found in the `figures` sub-directory

* A PDF file `attrMort-seasonal.pdf` containing influenza-attributable mortality
estimates by season, overall and by age groups. The individual plots from this 
report can also be found in the `figures` sub-directory

* A PDF file `attrMort-weekly.pdf` containing influenza-attributable mortality
estimates by week, overall and by age groups. The individual plots from this 
report can also be found in the `figures` sub-directory

* A file named `mcsamples_weekly.RData`, created only if option `mcsamples_save`
in `options.R` is `TRUE`, and containing the Monte-Carlo samples for the weekly
attributable mortality estimates. The primary use for this file is for 
pooling influenza-attributable mortality estimates from different countries
into Europe-wide estimates, with 95% CIs included. 
Therefore, the default for `mcsamples_save` is currently `FALSE`.



### TODO

* Add capability to handle multiple sub-regions and pool the results via 
multivariate meta-analysis. 

* Maybe expand the produced output? (Ideas very welcome).

* ...

