

##################################################################################
#####  look at long-trend data ###################################################

# use satellite PM25 data (2011 to 2013)
# use measurements PM25 data 2014 to 2016 - all UAE

library(readr)
library(dplyr)
library(lubridate)

# satellite data
PM25_sat <- read_csv("PM10_PM25_2011_2016_MODIS.csv")

# get only data over Abu Dhabi -----------------------------------------------

# load station info over Abu Dhabi ----------------------------
EAD_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info.csv")
DM_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_DM_info.csv")
NCMS_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_NCMS_info.csv")

stations_info <- rbind(EAD_info, DM_info, NCMS_info)

PM25_sat$Pollutant <- "PM2.5"

# attach infos to satellite PM2.5 data
PM25_sat <- stations_info %>%
  left_join(PM25_sat, c("Site", "Pollutant", "Latitude", "Longitude"))

# remove all lines with NA
PM25_sat <- na.omit(PM25_sat)

# remove outiers #######################################

#### remove outliers function---------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 4 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

########################################################

# remove outliers-----------------------------------------
PM25_sat_no_outliers <- remove_outliers(PM25_sat$AOD_PM25)

# new table with additional smooth column without outliers-----
PM25_sat_no_outliers <- cbind(PM25_sat, PM25_sat_no_outliers)

# select only data from 2011 to 2013
PM25_sat_no_outliers <- PM25_sat_no_outliers %>%
  select(Date,
         Site,
         PM25_sat_no_outliers)

PM25_sat_no_outliers <- PM25_sat_no_outliers %>%
  mutate(year = year(Date)) %>%
  filter(year <= 2013 & year >= 2011)
names(PM25_sat_no_outliers)[names(PM25_sat_no_outliers) == 'PM25_sat_no_outliers'] <- 'Daily_mean'


############################################################
#### load monitoring data from 2014 to 2016

dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"
# dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"


EAD_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2013 _daily_filtered.csv"))
EAD_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2014 _daily_filtered.csv"))
EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2015 _daily_filtered.csv"))
EAD_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2016 _daily_filtered.csv"))

DM_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2013 _daily_filtered.csv"))
DM_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2014 _daily_filtered.csv"))
DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2015 _daily_filtered.csv"))
DM_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2016 _daily_filtered.csv"))

NCMS_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2013 _daily_filtered.csv"))
NCMS_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2014 _daily_filtered.csv"))
NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2015 _daily_filtered.csv"))
NCMS_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2016 _daily_filtered.csv"))

# bind data together
UAE_AQ <- rbind(EAD_AQ_2013, EAD_AQ_2014, EAD_AQ_2015, EAD_AQ_2016,
                DM_AQ_2013, DM_AQ_2014, DM_AQ_2015, DM_AQ_2016,
                NCMS_AQ_2013, NCMS_AQ_2014, NCMS_AQ_2015, NCMS_AQ_2016)

UAE_AQ$Site <- as.character(UAE_AQ$Site)
UAE_AQ$Site_Type <- as.character(UAE_AQ$Site_Type)
UAE_AQ$Pollutant <- as.character(UAE_AQ$Pollutant)
str(UAE_AQ)


# filter only PM2.5 data after 2013------------------------------------------------

PM25_measures <- UAE_AQ %>%
  mutate(year = year(Date)) %>%
  filter(Pollutant == "PM2.5") %>%
  filter(year > 2013)


PM25_measures <- PM25_measures %>%
  select(Date,
         Site,
         Daily_mean,
         year)

# bind satellite and measurements data together -----------------------------

PM25_all <- rbind(PM25_sat_no_outliers,
                  PM25_measures)

# get daily means (6 years of data) ######################################
PM25_all_mean <- PM25_all %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(Daily_mean, na.rm = TRUE))


PM25_TREND_6_YEARS <- ggplot(PM25_all_mean, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25")) +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 450)  
PM25_TREND_6_YEARS

# remove outliers (4 times IQR)
PM25_all_mean$mean_PM25_no_outliers <- remove_outliers(PM25_all_mean$mean_PM25)

PM25_TREND_6_YEARS_no_outliers <- ggplot(PM25_all_mean, aes(Date, mean_PM25_no_outliers)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25_no_outliers, col = "mean_PM25_no_outliers")) +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 150)  
PM25_TREND_6_YEARS_no_outliers


# get data from smooth fit curve --------------------------------------------

AAA <- PM25_all_mean
AAA$Date <- as.numeric(AAA$Date)  # need Date to be numeric

smooth_PM25_no_outliers <- predict(loess(mean_PM25_no_outliers ~ Date ,AAA),
                                   AAA$Date)


# remove all lines with NA
smooth_PM25_no_outliers <- na.omit(smooth_PM25_no_outliers)
XXX <- as.data.frame(smooth_PM25_no_outliers)
MIN <- min(smooth_PM25_no_outliers)


# bind smooth data to main data
PM25_all_mean <- cbind(PM25_all_mean,
                       smooth_PM25_no_outliers)

PM25_all_mean <- PM25_all_mean %>%
  mutate(year = year(Date))

# calculate means by years from the smooth curve------------------------
PM25_all_mean_YEARS <- PM25_all_mean %>%
  group_by(year) %>%
  summarise(mean_years = mean(smooth_PM25_no_outliers, na.rm = TRUE))

# calculate the ANNUAl MEAN over 6 years ------------------------------

str(PM25_all_mean_YEARS)
MEAN_PM25 <- mean(PM25_all_mean_YEARS$mean_years)

str(PM25_all_mean)

PM25_TREND_6_YEARS_no_outliers <- ggplot(PM25_all_mean, aes(Date, mean_PM25_no_outliers)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25_no_outliers, col = "mean_PM25_no_outliers")) +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 150) + 
  geom_hline(yintercept=MEAN_PM25, col="black", size = 1) +
  geom_hline(yintercept=MIN, col="black", size = 1, linetype="dashed") 
  
#  geom_text(, label = "44.2 long term annual mean"), size = 7) 

  
PM25_TREND_6_YEARS_no_outliers





