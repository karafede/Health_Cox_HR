
# script to reorganise AQ data from MOCCAE.
# pre-processing to aggregate data in a data-base format has been done by dawit with Matlab

library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(openair)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data")


################################################################################

NCMS <- read_csv("database_NCMS_2015_daily.csv")
# replace NaN (not a Number with NA that is a missing value)
NCMS[sapply(NCMS,is.na)] = NA 

# DF[!is.na(DF$y),] # remove NA value in a specific column

# make hourly averages

NCMS_day <- NCMS %>%
  mutate(date = mdy(date, tz = "UTC")) %>%
         # day = str_sub(date, start = 1, end = -10), # read only 10 characters (the date)
         #  hour = hour(date)) %>%
  group_by(date,
           Pollutant,
           Site) %>%
  summarise(AVG = mean(Value, na.rm = TRUE)) %>%
  ungroup()


# check structure of yout data
str(NCMS_day)

# re-put right data-format
NCMS_day$day <- as.Date(NCMS_day$day)
str(NCMS_day)

# make plot in openair for each site
names(NCMS_day)[names(NCMS_day) == 'day'] <- 'date'
names(NCMS_day)[names(NCMS_day) == 'SiteName'] <- 'site'
names(NCMS_day)[names(NCMS_day) == 'variable'] <- 'pollutant'

# NCMS_day <- NCMS_day[!is.na(NCMS_day$AVG),] # remove nA value in a specific column


# search for one specific pollutant
NCMS_day_CO <- NCMS_day %>%
  filter(pollutant == "CO") 

# make plot in openair for each site
openair::timePlot(NCMS_day_CO, "AVG", type = "site")




