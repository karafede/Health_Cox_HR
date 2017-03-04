
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/")


# data capture --------------------------------------------------

 EAD_data_2013 <- read_csv("database_EAD_2013_daily.csv")
 EAD_data_2014 <- read_csv("database_EAD_2014_daily.csv")
 EAD_data_2015 <- read_csv("database_EAD_2015_daily.csv")
 EAD_data_2016 <- read_csv("database_EAD_2016_daily.csv")
 
 DM_data_2013 <- read_csv("database_DM_2013_daily.csv")
 DM_data_2014 <- read_csv("database_DM_2014_daily.csv")
 DM_data_2015 <- read_csv("database_DM_2015_daily.csv")
 DM_data_2016 <- read_csv("database_DM_2016_daily.csv")
 
 NCMS_data_2013 <- read_csv("database_NCMS_2013_daily.csv")
 NCMS_data_2014 <- read_csv("database_NCMS_2014_daily.csv")
 NCMS_data_2015 <- read_csv("database_NCMS_2015_daily.csv")
 NCMS_data_2016 <- read_csv("database_NCMS_2016_daily.csv")

AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
                 DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
                 NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)


# replace NaN (not a Number with NA that is a missing value)
AQ_data[sapply(AQ_data,is.na)] = NA


# get the name of the sites
names <- AQ_data %>% 
  select(Site,
         Value) %>%
  group_by(Site) %>%
  summarise(mean = mean(Value))

list_names <- as.list(names$Site)

# filter only PM2.5 by station and by year and calculate data capture by quarter


data <- AQ_data %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date),
         month = month(date)) %>%
  dplyr:: select(date,
                 Site,
                 year,
                 month,
                 Pollutant,
                 Value,
                 Cap) %>%
  filter(Pollutant == "PM2.5")


## get the months of observations
data$month <- factor(format(data$date, format = "%b"), levels = month.abb)

## Define the quarters the quarters
data$quarter <- character(length = nrow(data))
data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))


aaa <- data$Cap < 75
dawit <- which(aaa, arr.ind = TRUE, useNames = TRUE)
data$Value[dawit]<- NA

data$Cap_bin <- as.data.frame()
for (i in 1:nrow(data)){
  if (data$Cap[i] >= 75){
    data$Cap_bin[i] <- 1
  }
  if (data$Cap[i] < 75 ){
    data$Cap_bin[i]<-0
  }
}
data$one <-1


capture_quarter <- data %>%
  group_by(Site,
           quarter,
           year) %>%
  summarise(SUM = round(100*sum(Cap_bin)/sum(one), digits = 0))

# spread data
capture_quarter <- capture_quarter %>%
spread(quarter, SUM)

write_csv(capture_quarter, "Capture_PM25.csv")

###############################################################################
# function to all quarterly averages------------------------------------------

quarterly_averages <- function (data, site) {
  
  data <- data %>%
    mutate(date = mdy(date, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    dplyr:: select(date,
                   year,
                   month,
                   Pollutant,
                   Value,
                   Cap,
                   Site) %>%
    filter(Pollutant == "PM2.5"  & Site == site)
  
  ## get the months of observations
  
  data$month <- factor(format(data$date, format = "%b"), levels = month.abb)
  
  
  ## Format the quarters
  data$quarter <- character(length = nrow(data))
  data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
  data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
  data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
  data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
  data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))
  
  ## year variable
  data$year <- factor(format(data$date, format = "%Y"))
  
  # make averages by quarters------------------------------------------
  AVG_quarter <- data %>%
    group_by(Site,
             quarter,
             year) %>%
    summarise(mean = round(mean(Value, na.rm = TRUE), digits = 2))
  
  # return
  AVG_quarter <- as.data.frame(AVG_quarter)
  AVG_quarter
  
}


# loop all the stations and concatenate all the data to calcualte quarterly means
All_quarters <- data.frame()
for (i in 1:length(list_names)) {
  All_AVG <- quarterly_averages(AQ_data, unlist(list_names[6]))
  All_AVG <- quarterly_averages(AQ_data, unlist(list_names[i]))
  All_quarters <- rbind(All_quarters, All_AVG)
}


# spread data
All_quarters <- All_quarters %>%
  spread(quarter, mean)


All_quarters$annual_AVG <- round(rowMeans(All_quarters[ ,3:6], na.rm = TRUE), digit = 2)

# replace NaN (not a Number with NA that is a missing value)
All_quarters[sapply(All_quarters,is.na)] = NA

write_csv(All_quarters, "Annual_means_Quarters_PM2.5.csv")
