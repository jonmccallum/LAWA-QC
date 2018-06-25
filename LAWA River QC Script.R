# 25/6/18 Jonathan McCallum (Jonathan.McCallum@tasman.govt.nz)

# QC Script for River Water Quality data

# Used for checking LAWA river water quality data for the following:
# - < and > symbols
# - Last data collection date
# - Missing data
# - Data double-ups
# - Out of range data
# - Outliers
# - TP less than DRP
# - TN less than TON + NH4

# LAWA data accessed as csv files from Sharepoint links:
# 20170820 Datasets:  https://horizonsregionalcouncil-my.sharepoint.com/personal/sean_hodges_horizons_govt_nz/_layouts/15/guestaccess.aspx?folderid=0305cdb9b96d44b919a8a7e5c03212bd0&authkey=Aes8g14LFfyCVscYgPH54T4
# Contact Sean Hodges (Sean.Hodges@horizons.govt.nz) about data access issues.

##### Inputs #####

# Working directory (folder where LAWA data is saved, use forward slashes rather than back-slashes)
setwd("H:/R/R Input")

# LAWA data file
fname <- "Tasman 20170820 raw.csv"

# Period of interest (5 yrs from January 2012 to December 2016 in this case)
date_start <- as.Date("2012-01-01")
date_end <- as.Date("2016-12-31")

# Sampling frequency (Quarterly or Monthly)
sfrequency <- "Monthly"

# Lab detection limits for Total Nitrogen and Total Phosphorus (g/m3)
TN_detlimit <- 0.01
TP_detlimit <- 0.004


##### Setup #####

# Load packages
library(tidyverse)
library(lubridate)

# Load helper functions
source("H:/R/R Scripts/LAWA QC/LAWA River QC Helpers.R")

# Read LAWA data
d <- read_lawa(fname, sfrequency)


##### < and > symbols #####

# If the data have been pre-processed, there should be no < and > symbols.
# Instead, the Censored and CenType columns hold this information.

# If Value contains < and > symbols, print a warning
if(any(grepl("<|>", d$Value))) {
  warning("Value column contains < or > symbols.")
}


##### Last data collection date #####

# For each site, when was the last (most recent) data collection date?
d_last <- d %>%
  group_by(SiteName) %>% 
  summarise(LastDateDay = last(DateDay))


##### Missing data #####

# Missing combinations of site, parameter, year and season
# Ignores combinations that don't occur in the data
# For example, if TON was not collected during all of 2014, if won't be listed as missing
# Note that for Quarterly data, the definition of 'Season' affects the output (this is set in the season_fun helper function)
d_missing <- d %>%
  # 'nesting' limits combinations to those that occur in the data
  complete(nesting(SiteName, parameter, DateYear), Season) %>%
  filter(is.na(Value)) %>%
  # reorder columns
  select(DateYear, Season, SiteName, Date, parameter, Value) %>%
  arrange(DateYear, Season)

# Export csv of missing combinations
write_csv(d_missing, sub(".csv", " missing.csv", fname))


##### Data double-ups #####

# Multiple records from the same site on the same day
d_multiple <- d %>%
  group_by(SiteName, parameter, DateDay) %>%
  filter(n() > 1) %>%
  ungroup()

# Export csv of data double-ups
write_csv(d_multiple, sub(".csv", " multiple.csv", fname))


##### Out of Range data #####

# Identify potential out of range data
# Low and high thresholds are set in range_fun helper function
d_range <- d %>%
  select(SiteName, Date, parameter, Value) %>%
  mutate(RangeWarning = range_fun(parameter, Value)) %>%
  filter(!is.na(RangeWarning))

# Export csv of range warnings
write_csv(d_range, sub(".csv", " range warnings.csv", fname))


##### Outliers #####

# Identify potential outliers for each site-parameter combination (using the default 1.5x IQR)
d_outlier <- d %>%
  group_by(SiteName, parameter) %>%
  do(outlier_fun(.$Value, .$Date, coef = 1.5)) %>%
  ungroup() %>%
  filter(!is.na(OutlierWarning))


##### TP less than DRP #####

# Identify samples where TP is less than DRP
# Differences within the TP detection limit are ignored
d_TP <- d %>%
  select(SiteName, Date, Value, parameter) %>%
  spread(parameter, Value) %>%
  mutate(TPWarning = ifelse((DRP - TP) > TP_detlimit, "TP less than DRP", NA)) %>%
  filter(!is.na(TPWarning))


##### TN less than TON + NH4 #####

# Identify samples where TN is less than the sum of TON and NH4
# Differences within the TN detection limit are ignored
# Only run if dataset contains TON (Total Oxidised Nitrogen)
if ("TON" %in% unique(d$parameter)){
  d_TN <- d %>%
    select(SiteName, Date, Value, parameter) %>%
    spread(parameter, Value) %>%
    mutate(TONplusNH4 = TON + NH4,
           TNWarning = ifelse((TONplusNH4 - TN) > TN_detlimit, "TN less than TON + NH4", NA)) %>%
    filter(!is.na(TNWarning))
}


