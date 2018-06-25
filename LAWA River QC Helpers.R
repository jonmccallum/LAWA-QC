# 25/6/18 Jonathan McCallum (Jonathan.McCallum@tasman.govt.nz)

# LAWA QC Helper Functions

season_fun <- function(DateMonth){
  # Function for classifying the months from quarterly data into seasons
  # DateMonth is an integer from 1 to 12
  # The default definition of Summer is Jan, Feb & Mar (modify if needed)
  
  Season <- rep(NA, length(DateMonth))
  Season[which(DateMonth %in% c(1,2,3))] <- "Summer"
  Season[which(DateMonth %in% c(4,5,6))] <- "Autumn"
  Season[which(DateMonth %in% c(7,8,9))] <- "Winter"
  Season[which(DateMonth %in% c(10,11,12))] <- "Spring"
  return(Season)
}

range_fun <- function(parameter, Value){
  # Function for classifying out of range data
  # Relies on the the following default low and high thresholds (modify if needed)
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

read_lawa <- function(fname, sfrequency){
  # Function for reading LAWA data for one region
  # fname is the file name of the csv file to read
  # sfrequency is the sampling frequency (Quarterly or Monthly)
  require(tidyverse)
  require(lubridate)
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
  return(d)
}

