---
output:
  html_document: default
  pdf_document: default
---
## FluMoDLpack version 0.0.1


## Overview

This manual explains how to use FluMoDLpack in order to analyze 
influenza-attributable mortality using 
[FluMoDL](https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.14.1800118),
within the context of the [EuroMOMO network](http://www.euromomo.eu/).



### What FluMoDLpack is

FluMoDLpack is a set of R scripts that implements a standard set of analyses
of influenza-attributable mortality using FluMoDL. It relies on the 
[FluMoDL R package](https://cran.r-project.org/package=FluMoDL) to do the 
actual model fitting and analysis.

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



### What output does FluMoDLpack create?

[To be added]

