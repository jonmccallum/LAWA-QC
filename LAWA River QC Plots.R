# 25/6/18 Jonathan McCallum (Jonathan.McCallum@tasman.govt.nz)

# Plotting script for LAWA River Water Quality data

# Produces the following plots:
# - Scatterplots of black disc water clarity vs turbidity
# - Timeseries graph for each site x parameter combination

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

# Sampling frequency over the period of interest (Quarterly or Monthly)
sfrequency <- "Quarterly"

# Output folder for plots
output_folder <- "H:/R/R Output/LAWA QC"

# Width and height of exported plots (inches)
out_wd = 5
out_ht = 4


##### Setup #####

# Load packages
library(tidyverse)
library(lubridate)

# Load helper functions
source("H:/R/R Scripts/LAWA QC/LAWA River QC Helpers.R")

# Read LAWA data
d <- read_lawa(fname, sfrequency)


##### Setup #####

# increase plot font size
theme_set(theme_gray(base_size = 13))

# site names
sites <- unique(d$SiteName)


##### Scatterplots of black disc water clarity vs turbidity #####

# for each site
for (site in sites) {
  print(paste("Plotting", site))
  # filter
  d_hold <- d %>%
    filter(SiteName == site, parameter %in% c("BDISC", "TURB")) %>%
    spread(parameter, Value)
  # if BDISC and TURB columns exist and there's at least one row of data
  if ("BDISC" %in% names(d_hold) & "TURB" %in% names(d_hold) & nrow(d_hold) >= 1) {
    # plot
    png(units = "in", res = 200, file = paste0(output_folder, "/BDISCvsTURB for ", site,".png"),
        width = out_wd, height = out_ht)
    print(ggplot(aes(x = BDISC, y = TURB), data = d_hold) + 
            geom_point(colour = "navyblue"))
    dev.off() # close plotting device
  }
}



##### Timeseries graph for each site x parameter combination #####

# site x parameter combinations
site_by_pars <- d %>%
  select(SiteName, parameter) %>% 
  distinct()

# for each site x parameter combination
for (ii in 1:nrow(site_by_pars)) {
  site_by_par = paste(site_by_pars$parameter[ii], "for", site_by_pars$SiteName[ii])
  print(paste("Plotting", site_by_par))
  # filter
  d_hold <- d %>%
    filter(SiteName == site_by_pars$SiteName[ii], parameter == site_by_pars$parameter[ii]) %>%
    spread(parameter, Value)
  # if there's at least one row of data
  if (nrow(d_hold) >= 1) {
    # plot
    png(units = "in", res = 200, file = paste0(output_folder, "/", site_by_par,".png"),
        width = out_wd, height = out_ht)
    print(ggplot(aes_string(x = "DateDay", y = site_by_pars$parameter[ii]), data = d_hold) +
            geom_point(colour = "navyblue"))
    dev.off() # close plotting device
  }
}

