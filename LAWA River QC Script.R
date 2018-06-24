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

# Load packages
library(tidyverse)
library(lubridate)

# Working directory (folder where LAWA data is saved, use forward slashes rather than back-slashes)
setwd("H:/R/R Input")

# File name
fname <- "Tasman 20170820 raw.csv"

# Period of interest (5 yrs from January 2012 to December 2016 in this case)
date_start <- as.Date("2012-01-01")
date_end <- as.Date("2016-12-31")

# Sampling frequency (Quarterly or Monthly)
sfrequency <- "Monthly"

# Lab detection limits for Total Nitrogen and Total Phosphorus (g/m3)
TN_detlimit <- 0.01
TP_detlimit <- 0.004



##### Helper Functions #####

season_fun <- function(DateMonth){
  # Function for classifying the months from quarterly data into seasons
  # DateMonth is an integer from 1 to 12
  # The default definition of summer is Jan, Feb & Mar but can be changed
  
  Season <- rep(NA, length(DateMonth))
  Season[which(DateMonth %in% c(1,2,3))] <- "Summer"
  Season[which(DateMonth %in% c(4,5,6))] <- "Autumn"
  Season[which(DateMonth %in% c(7,8,9))] <- "Winter"
  Season[which(DateMonth %in% c(10,11,12))] <- "Spring"
  return(Season)
}

range_fun <- function(parameter, Value){
  # Function for classifying out of range data
  # Relies on the the following low and high thresholds
  # Customise to your needs:
  LowThresholds = c(BDISC = 0.01, DRP = 0.0001,TN = 0.0001, TP = 0.0001,
                    NH4 = 0.0001, TURB = 0.1, ECOLI = 1)
  HighThresholds = c(BDISC = 20, DRP = 10, TN = 10, TP = 10,
                     NH4 = 10, TURB = 50, ECOLI = 500000) 
  RangeWarning <- rep(NA, length(Value))
  RangeWarning[which(Value < LowThresholds[parameter])] <- "low"
  RangeWarning[which(Value > HighThresholds[parameter])] <- "high"
  return(RangeWarning)
}

outlier_fun <- function(Value, Date, coef = 1.5){
  # Function for identifying outliers using 'boxplot.stats'
  # Outliers are more extreme than 1.5x the inter-quartile range from the first or third quartile
  BoxStats <- boxplot.stats(Value, coef = coef)
  OutlierWarning <- rep(NA, length(Value))
  OutlierWarning[which(Value < BoxStats$stats[1])] <- "low"
  OutlierWarning[which(Value > BoxStats$stats[5])] <- "high"
  # Convert output to data frame
  Results <- data.frame(Value = Value, Date = Date,
                        OutlierWarning = OutlierWarning, stringsAsFactors = F)
  return(Results)
}


##### Load Data #####

# Read LAWA data for one region
d <- read_csv(fname) %>%
  # select columns of interest
  select(SiteName, Date, parameter, Value, Frequency, Censored, CenType) %>%
  # pull out full date component, the month and the year
  mutate(DateDay = date(Date),
         DateMonth = month(Date),
         DateYear = year(Date),
         # classify months into seasons
         # Uses season_fun() for quarterly or month() for monthly
         Season = if(sfrequency == "Quarterly") season_fun(DateMonth) else DateMonth) %>%
  # filter to period of interest
  filter(between(DateDay, date_start, date_end)) %>%
  # sort by site, parameter and date
  arrange(SiteName, parameter, DateDay)


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
# The definition of 'season' is important here (set in season_fun helper function)
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


