
# time series with hourly UAE pollution data

library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data")
setwd("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/Interactive_plots_R")

# NCMS <- read_csv("database_NCMS_2016_hourly.csv")
EAD <- read_csv("database_EAD_2015_hourly.csv")

# replace NaN (not a Number with NA that is a missing value)
EAD[sapply(EAD,is.na)] = NA 

 EAD <- EAD %>%
   mutate(date = DateTime)

 # shift time back of three hours
 EAD$date <- (EAD$date) - 4*60*60

# NCMS$AAA <- NCMS$date + 

data_time <- EAD %>%
  filter(Site == "Bain Al Jesrain") %>%
  select(date,
         Site,
         Pollutant,
         Value) 

# remove line with date == NA
# data_time <- data_time[!is.na(data_time$date),]

data_time <- data_time %>%
  spread(Pollutant, Value)

# Build timeseries for plots
time_series <- data_frame_to_timeseries(data_time)

# Return
time_series

# make interactive time-series plot
colour_vector <- threadr::ggplot2_colours(45)

plot <- dygraph(time_series$`Lower Ambient Temperature`) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "Lower Ambi. Temp") %>% 
  dyAxis("y", label = "Hourly Temp. <sup>o</sup>C") %>% 
  dyRangeSelector()
plot


plot <- dygraph(time_series$PM10) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "PM10") %>% 
  dyAxis("y", label = "Daily PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot

plot <- dygraph(time_series$PM10) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "PM10") %>% 
  dyAxis("y", label = "Daily PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot


plot <- dygraph(time_series$SO2) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "SO2") %>% 
  dyAxis("y", label = "Daily SO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot




