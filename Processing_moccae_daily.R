
library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data")

wd <- getwd()
EAD_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2013_O3_daily.csv"))

# EAD_data_2013 <- read_csv("database_EAD_2013_daily.csv")
# EAD_data_2014 <- read_csv("database_EAD_2014_daily.csv")
#EAD_data_2015 <- read_csv("database_EAD_2015_daily.csv")
# EAD_data_2016 <- read_csv("database_EAD_2016_daily.csv")

# DM_data_2013 <- read_csv("database_DM_2013_daily.csv")
# DM_data_2014 <- read_csv("database_DM_2014_daily.csv")
# DM_data_2015 <- read_csv("database_DM_2015_daily.csv")
# DM_data_2016 <- read_csv("database_DM_2016_daily.csv")
# 
# NCMS_data_2013 <- read_csv("database_NCMS_2013_daily.csv")
# NCMS_data_2014 <- read_csv("database_NCMS_2014_daily.csv")
# NCMS_data_2015 <- read_csv("database_NCMS_2015_daily.csv")
# NCMS_data_2016 <- read_csv("database_NCMS_2016_daily.csv")

# AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
#                  DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
#                  NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)

AQ_data <- EAD_O3_2013 

# replace NaN (not a Number with NA that is a missing value)
AQ_data[sapply(AQ_data,is.na)] = NA 


AQ_data$Mean_8hour <- as.numeric(AQ_data$Mean_8hour)
AQ_data$MAX_8hour <- as.numeric(AQ_data$MAX_8hour)



AQ_data_O3 <- AQ_data %>%
  mutate(year = year(Date)) %>%
  dplyr:: select(Date,
                 Site,
                 Mean_8hour,
                 year) 

AQ_data_O3 <- AQ_data_O3 %>%
  group_by(Date) %>%
  summarise(mean = mean(Mean_8hour, na.rm = TRUE))
  

# # make a mean for all the sites in Abu Dhabi for the year 2013
# AQ_data_NO2 <- AQ_data_NO2 %>%
#   group_by(date) %>%
#   summarise(mean = mean(Value, na.rm = TRUE))
# 
# AQ_data_O3 <- AQ_data %>%
#  mutate(date = mdy(date, tz = "UTC"),
#        year = year(date)) %>%
#   dplyr:: select(date,
#                  Site,
#                  Value,
#                  year,
#                  Pollutant) %>%
#  filter(Pollutant == "NO2")
# #  filter(Site != "Zakher")
# 
# # make a mean for all the sites in Abu Dhabi for the year 2013
# AQ_data_NO2 <- AQ_data_NO2 %>%
#   group_by(date) %>%
#   summarise(mean = mean(Value, na.rm = TRUE))
  


# ggplot(AQ_data_PM10, aes(date, Value)) + 
#   theme_bw() +
#   geom_line() + geom_smooth() +
#   theme(legend.position="none") + 
#   facet_wrap("Site") + ylim(0, 5)   
# + geom_hline(yintercept=3, col="red") +
#  geom_hline(yintercept=0.5, col="red")

######################################################
### plot for one site---------------------------------

# AQ_data_PM25 <- AQ_data_PM25 %>%
#   filter(pollutant == "PM2.5", site == "Burairat") 

# ggplot(AQ_data_PM10, aes(date, mean)) + 
#   theme_bw() +
#   geom_line() + geom_smooth() +
#   theme(legend.position="none") + 
#   facet_wrap("Site") + ylim(0, 150)  


ggplot(AQ_data_O3, aes(Date, mean)) + 
  theme_bw() +
  geom_line() + geom_smooth() +
  theme(legend.position="none") + 
  ylim(0, 150)  

# density plot with counts -----------------------------

AQ_data_O3 %>%  
  ggplot(aes(mean, ..count..)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  theme_bw()


AQ_data_O3 %>%  
  ggplot(aes(mean)) + 
  geom_histogram(aes(fill=..count..),binwidth=5, stat = "bin") +
  theme_bw()


AQ_data_O3 %>%  
  ggplot(aes(mean)) + 
  geom_line(aes(fill=..count..), stat="bin", binwidth=5) +
  theme_bw() 

# remove NA values
# NCMS_day_Burairat_PM10 <- NCMS_day_Burairat_PM10[!is.na(NCMS_day_Burairat_PM10$value),] # remove NA value in a specific column

# AQ_data_PM25 %>%  
#   ggplot(aes(Value)) + 
#   geom_line(stat="bin", binwidth=10) +
#   scale_x_continuous("PM10 ug/m3") +
#   scale_y_continuous("number of days") +
#   theme_bw() 

AQ_data_O3 %>%  
  ggplot(aes(mean)) + 
  geom_point(stat="bin", binwidth=10) +
  scale_x_continuous("O3 ug/m3") +
  scale_y_continuous("number of days") +
  theme_bw() 
  
###################################################
# other examples-----------------------------

# Draw with black outline, white fill

AQ_data_O3 %>%  
ggplot(aes(mean)) +
  geom_histogram(binwidth= 5, colour="black", fill="white") +
  theme_bw() 

# Histogram overlaid with kernel density curve

NCMS_day_Burairat_PM10 %>% 
ggplot(aes(value)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth= 5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  theme_bw() 


NCMS_day_Burairat_PM10 %>% 
ggplot(aes(value)) +
  geom_histogram(binwidth= 5, colour="black", fill="white") +
  geom_vline(aes(xintercept = mean(value, na.rm=TRUE)),   
             color="red", linetype="dashed", size=1) +
  theme_bw() 


 # use FACETS------------------------------------------------------

NCMS_day_CO %>%
ggplot(aes(value)) +
  geom_histogram(binwidth = .2, colour="black", fill="white") + 
  facet_grid( site ~. ) +
  theme_bw()
#  facet_wrap("site") 

NCMS_day_CO %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = .2, colour="black", fill="white") + 
  facet_wrap("site") +
  theme_bw() 

# Find the mean of each site
library(plyr)
mean_value <- ddply(na.exclude(NCMS_day_CO), "site", summarise, mean=mean(value))


# With mean lines
NCMS_day_CO %>%
ggplot(aes(value)) +
  geom_histogram(binwidth = .2, colour="black", fill="white") + 
  facet_grid(site ~ .) +
  geom_vline(data= mean_value, aes(xintercept = mean),
             linetype="dashed", size=1, colour="red") +
  theme_bw()





