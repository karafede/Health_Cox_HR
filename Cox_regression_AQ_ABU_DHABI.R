
library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(grid)
library(rms)
library(survival)
library(tidyr)
library(stats)


setwd("D:/R_processing")
source("termplot2.R")


# load trial health data

health_data <- read_csv("D:/R_processing/Health data/Asthma_Raw_Data.csv")
# health_data[sapply(health_data,is.na)] = NA 
health_data <- na.omit(health_data)

# filter data

health_data <- health_data %>%
  mutate(Date = mdy_hm(Date, tz = "UTC"),
         Date = date(Date),
         year = year(Date))

 health_data <- health_data %>%
   filter(!ET == "Day Surgery/Surgical Short Stay") %>%
   filter(!ET == "Home Care") %>%
   filter(!ET == "Radiology Exam") %>%
   filter(!ET == "Weqaya Screening") %>%
   filter(!ET == "Recurring Outpatient") 
 
   # filter(!Nationality == "GCC National") %>%
   # filter(!Nationality == "Non-National") 
   


health_data <- health_data %>%
  filter(Gender %in% c("Male", "Female"))


health_data$Gender <- as.factor(health_data$Gender)
str(health_data)

# categorise gender with 1 == Female, 2 == Male

# health_data$sex <- as.data.frame()
# for (i in 1:nrow(health_data)){
#   if (health_data$Gender[i] == "Female"){
#     health_data$sex[i] <- 1
#   }
#   if (health_data$Gender[i] == "Male"){
#     health_data$sex[i] <- 2
#   }
# }



# make a column with bin to count patients
health_data$bin <- "1"
health_data$bin  <- as.numeric(health_data$bin)


DAILY_health_data <- health_data %>%
  group_by(Date) %>%
  summarise(sum = sum(bin))
mean_DAILY <- DAILY_health_data %>%
  summarise(mean = mean(sum))

######################################
### make class of Age bins -----------
######################################

## function to generate classes of age bins---


age_bins_fun <- function(federico){
  
  if (!is.na(federico) & federico == 0)
    AGE_BIN = 1

  
  if (!is.na(federico) & federico <= 4 & federico >=1)
    AGE_BIN = 2
  
  if (!is.na(federico) & federico <= 9 & federico >=5)
    AGE_BIN = 3
  
  if (!is.na(federico) & federico <= 14 & federico >= 10)
    AGE_BIN = 4
  
  if (!is.na(federico) & federico <= 19 & federico >= 15)
    AGE_BIN = 5
  
  if (!is.na(federico) & federico <= 24 & federico >= 20)
    AGE_BIN = 6
  
  if (!is.na(federico) & federico <= 29 & federico >= 25)
    AGE_BIN = 7
  
  if (!is.na(federico) & federico <= 34 & federico >= 30)
    AGE_BIN = 8
  
  if (!is.na(federico) & federico <= 39 & federico >= 35)
    AGE_BIN = 9
  
  if (!is.na(federico) & federico <= 44 & federico >= 40)
    AGE_BIN = 10
  
  if (!is.na(federico) & federico <= 49 & federico >= 45)
    AGE_BIN = 11
  
  if (!is.na(federico) & federico <= 54 & federico >= 50)
    AGE_BIN = 12
  
  if (!is.na(federico) & federico <= 59 & federico >= 55)
    AGE_BIN = 13
  
  if (!is.na(federico) & federico <= 64 & federico >= 60)
    AGE_BIN = 14
  
  if (!is.na(federico) & federico <= 69 & federico >= 65)
    AGE_BIN = 15
  
  if (!is.na(federico) & federico <= 74 & federico >= 70)
    AGE_BIN = 16
  
  if (!is.na(federico) & federico <= 79 & federico >= 75)
    AGE_BIN = 17
  
  if (!is.na(federico) & federico <= 84 & federico >= 80)
    AGE_BIN = 18
  
  if (!is.na(federico) & federico >= 85)
    AGE_BIN = 19
  

    # if(is.na(federico))
    #   AGE_BIN = NA
    # if(exists("AGE_BIN")){
    # 
    # } else {
    #   AGE_BIN=NA
    # }
  
  return(AGE_BIN)
  
}



#################
### AGE BINS ####
################# 

age_data <- as.vector(health_data$Age)

AGE_BIN <- lapply(age_data, age_bins_fun)
AGE_BIN <- as.numeric(AGE_BIN)

AGE_BIN <- as.data.frame(AGE_BIN)

health_data <- cbind(health_data, AGE_BIN)



   str(health_data)
   health_data$AGE_BIN <- as.factor(health_data$AGE_BIN)
   
   str(health_data)
  
# rename factor with names------------
  levels(health_data$AGE_BIN) <- gsub("^1$","00", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^2$","01-04", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^3$","05-09", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^4$","10-14", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^5$","15-19", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^6$","20-24", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^7$","25-29", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^8$","30-34", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^9$","35-39", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^10$","40-44", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^11$","45-49", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^12$","50-54", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^13$","55-59", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^14$","60-64", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^15$","65-69", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^16$","70-74", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^17$","75-79", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^18$","80-84", levels(health_data$AGE_BIN))
  levels(health_data$AGE_BIN) <- gsub("^19$","85-up", levels(health_data$AGE_BIN))
  
# bind age bins with health data again with age bins
 # health_data <- cbind(health_data, age_bin)
  str(health_data)
  
  # save health data------------------------------------------------
  
write_csv(health_data, "health_data_age_bins.csv")
  
  
#####################################################################

# load health data with age bins
health_data <- read_csv("health_data_age_bins.csv")
  str(health_data)
  
  health_data$AGE_BIN <- as.factor(health_data$AGE_BIN)
  health_data$Gender <- as.factor(health_data$Gender)
  
  str(health_data)
  
# count patients in each day (all genders all ages)
health_data <- health_data %>%
  group_by(Date,
           Gender,
           AGE_BIN) %>%
  summarise(sum_patients = sum(bin, na.rm = TRUE))



# count patients in each day (all genders all ages)
health_data_sum <- health_data %>%
  group_by(Date) %>%
  summarise(sum_patients = sum(sum_patients, na.rm = TRUE))


# take only the 8.5 of the total daily admissions (as for the Dubai & Northern Emirate data) #####################
##################################################################################################################

health_data_sum <- health_data %>%
  group_by(Date) %>%
  summarise(sum_patients = 0.085*sum(sum_patients, na.rm = TRUE))


# quick plot ######################################


jpeg('D:/R_processing/plots/Abu_Dhabi_Asthsma_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


min <- as.Date("2011-06-01") 
max <- as.Date("2013-05-31") 



plot <- ggplot(health_data_sum, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="blue") +
  ggtitle("counts admissions (Abu Dhabi - Asthma, 2013-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression("Sum Patients (counts) per day")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylim(0, 60) + 
  xlim(min, max) 
plot

par(oldpar)
dev.off()




################################################################################
################################################################################
############## DO NOT RUN THIS PART ############################################
################################################################################
################################################################################
################################################################################

# count patients in each day (all genders all ages)
health_data <- health_data %>%
  group_by(Date) %>%
  summarise(sum_patients = 0.085*sum(sum_patients, na.rm = TRUE))

health_data <- na.omit(health_data)


#### filter health data for years < 2016


# quick plot #################

min <- as.Date("2011-06-01") 
max <- as.Date("2013-05-31") 

plot <- ggplot(health_data, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="blue") +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression("Sum Patients (counts)")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylim(0, 60) + 
  xlim(min, max) 
plot



# calculate DAILY & ANNUAL MEAN of counts over the years of data


health_data_daily <- health_data %>%
  summarise(daily_counts = mean(sum_patients, na.rm = TRUE))


health_data_annual <- health_data %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(annual_daily_counts = mean(sum_patients, na.rm = TRUE))

ANNUAL_DAILY_COUNTS <- mean(health_data_annual$annual_daily_counts)




# make an average of all daily concentrations over the years (seasonality)

health_annual_mean <- health_data %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(month,
           day) %>%
  summarise(daily_counts_seasons = mean(sum_patients, na.rm = TRUE))


# bind the above averaged to the health data to create a typical seasonal trend
# these data MUST match the health data length date
counts_season <- rbind(health_annual_mean[151:365, ],    # 2011 (from June to December)
                       health_annual_mean[1:366, ],    # 2012 (leap year)
                       health_annual_mean[1:150, ])    # 2013

counts_season <- as.data.frame(counts_season)

health_data <- cbind(health_data, counts_season)





jpeg('D:/R_processing/plots/Seasonality_Abu_Dhabi_admissions_asthma.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(health_data, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="red") +
  geom_line(aes(y = daily_counts_seasons, col = "daily_counts_seasons"), alpha=1, col="blue") +
  theme(legend.position="none") + 
 # stat_smooth(method = "loess") +
  ylab(expression("admissions (counts)")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=30, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=30),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=30, colour = "black")) +
  ylim(0, 45) + 
  xlim(min, max) 
plot

par(oldpar)
dev.off()




# subtract the seasonal trend from the PM2.5 data and add ANNUAL MEAN
health_data$detrend_counts <- (health_data$sum_patients - health_data$daily_counts_seasons)  + ANNUAL_DAILY_COUNTS


jpeg('D:/R_processing/plots/NO_Seasonality_Abu_Dhabi_admissions_asthma.jpg',     
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(health_data, aes(Date, detrend_counts)) + 
  theme_bw() +
  # geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="red") +
  geom_line(aes(y = detrend_counts, col = "detrend_counts"), alpha=1, col="blue") +
  theme(legend.position="none") + 
  ylab(expression("admissions (counts)")) + 
  stat_smooth(method = "loess") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=30, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=30),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=30, colour = "black")) +
  ylim(0, 50) + 
  xlim(min, max) 
plot


par(oldpar)
dev.off()


########################################################################################
########################################################################################
########################################################################################



###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
## load satellite data for PM2.5 from MODIS------------------------------------
###############################################################################


# PM25_AOD <- read_csv("PM25_from_AOD_MODIS.csv")
PM25_AOD <- read_csv("PM10_PM25_2011_2016_MODIS.csv")

# get only data over Abu Dhabi -----------------------------------------------

# load station info over Abu Dhabi ----------------------------
EAD_info <- read.csv("D:/AQI/Stations_EAD_info.csv")
PM25_AOD$Pollutant <- "PM2.5"

# attach infos to satellite PM2.5 data
PM25_AOD <- EAD_info %>%
  left_join(PM25_AOD, c("Site", "Pollutant", "Latitude", "Longitude"))



PM25_AOD <- PM25_AOD %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(AOD_PM25, na.rm = TRUE))



# quick plot #################


jpeg('D:/R_processing/plots/Abu Dhabi_PM25_Daily_Average.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

plot <- ggplot(PM25_AOD, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="blue") +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ggtitle("Daily PM2.5 concentration (Abu Dhabi)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 250)  +
xlim(min, max) 
plot

par(oldpar)
dev.off()

# remove outliers #######################################

#### remove outliers function---------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 4.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

########################################################
# remove outliers-----------------------------------------
PM25_sat_no_outliers <- remove_outliers(PM25_AOD$mean_PM25)

# new table with additional smooth column without outliers-----
PM25_AOD <- cbind(PM25_AOD, PM25_sat_no_outliers)


plot <- ggplot(PM25_AOD, aes(Date, PM25_sat_no_outliers)) +
  theme_bw() +
  geom_line(aes(y = PM25_sat_no_outliers, col = "mean_PM25"), alpha=1, col="blue") +
  stat_smooth(method = "loess") +
  theme(legend.position="none") +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) +
  ylim(0, 420)
plot



# select only data from 2011 to 2013


PM25_AOD <- PM25_AOD %>%
  mutate(year = year(Date)) %>%
  filter(year <= 2013 & year >= 2011)

PM25_AOD <- PM25_AOD %>%
  mutate(year = year(Date)) %>%
  filter(year <= 2013 & year >= 2011) %>%
  select(- mean_PM25)
names(PM25_AOD)[names(PM25_AOD) == 'PM25_sat_no_outliers'] <- 'mean_PM25'


# 42.26717


#######################################################################################
################# DO NOT RUN THIS PART ################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
##### remove seasonality.......fit Curve with a sinusoidal function ###################
#######################################################################################

# calculate daily & ANNUAL MEAN over the years of data

PM25_annual <- PM25_AOD %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(annual_PM25 = mean(mean_PM25, na.rm = TRUE))
ANNUAL_MEAN <- mean(PM25_annual$annual_PM25)

PM25_DAILY <- PM25_AOD %>%
  summarise(annual_PM25 = median(mean_PM25, na.rm = TRUE))


# make an average of all daily concentrations over the years (seasonality)

PM25_season <- PM25_AOD %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(month,
           day) %>%
  summarise(season_PM25 = mean(mean_PM25, na.rm = TRUE))



# bind the above averaged to the PM2.5 data

PM25_AVG_Jan_Feb <- PM25_season[1:28 ,]
PM25_AVG_March_Dec <- PM25_season[30:366 ,]
PM25_AVG_no_leap <- rbind(PM25_AVG_Jan_Feb, PM25_AVG_March_Dec)
PM25_leap <- PM25_season

PM25_season <- rbind(PM25_AVG_no_leap,    # 2011
                     PM25_leap,           # 2012
                     PM25_AVG_no_leap)    # 2013


PM25_season <- as.data.frame(PM25_season)
PM25_all <- cbind(PM25_AOD, PM25_season)

# PM25_all <- PM25_all %>%
#   filter(mean_PM25 < 132)


jpeg('D:/R_processing/plots/Seasonality_Abu_Dhabi_PM25.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(PM25_all, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="red") +
  geom_line(aes(y = season_PM25, col = "season"), alpha=1, col="blue") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=32, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32, colour = "black")) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 150)  +
  xlim(min, max) 
plot

par(oldpar)
dev.off()






# subtract the seasonal trend from the PM2.5 data and add ANNUAL MEAN
PM25_all$detrend_PM25 <- (PM25_all$mean_PM25 - PM25_all$season_PM25)  + ANNUAL_MEAN





jpeg('D:/R_processing/plots/NO_Seasonality_Abu_Dhabi_PM25.jpg',     
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)



plot <- ggplot(PM25_all, aes(Date, detrend_PM25)) + 
  theme_bw() +
  #  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="red") +
  geom_line(aes(y = detrend_PM25, col = "season"), alpha=1, col="blue") +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=32, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32, colour = "black")) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(-10, 150)+
  xlim(min, max) 
plot

par(oldpar)
dev.off()


#################################################################################################
##### RUN THIS PART #############################################################################
#################################################################################################
############################################################################
############################################################################
############################################################################
# join PM2.5 data and health data-----


# introduce a LAG in the health data (shift admission to hosptitals by 1 days)

str(health_data)
health_data <- as.data.frame(health_data)
health_data <- health_data %>%
  mutate(Date = Date - 2)  # add x number of days

# AQ_HEALTH <- PM25_AOD %>%
#   left_join(health_data, "Date")
# AQ_HEALTH[sapply(AQ_HEALTH,is.na)] = NA

AQ_HEALTH <- PM25_all %>%
  left_join(health_data, "Date")
AQ_HEALTH[sapply(AQ_HEALTH,is.na)] = NA

# remove all lines with NA
AQ_HEALTH <- na.omit(AQ_HEALTH)
str(AQ_HEALTH)



############################################################
########## SAVE DATA #######################################
############################################################

write.csv(AQ_HEALTH, "HEALTH_DATA_PM25_COUNTS_ABU_DHABI.csv")
AQ_HEALTH <- read.csv("HEALTH_DATA_PM25_COUNTS_ABU_DHABI.csv")

AQ_Health_sum_admissions <- AQ_HEALTH %>%
 group_by(Gender) %>%
   summarise(sum_admissions = sum(sum_patients, na.rm = TRUE))




#############################################################
##### Summary STATISTICAL plots #############################
#############################################################


# average all variables by day

# AQ_HEALTH_SUMMARY_STATS <- AQ_HEALTH %>%
#   group_by(Date) %>%
#   summarise(mean_PM25 = mean(mean_PM25),
#             sum_patients = sum(sum_patients))


AQ_HEALTH_SUMMARY_STATS <- AQ_HEALTH %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(detrend_PM25),
            sum_patients = sum(detrend_counts))


jpeg('D:/R_processing/plots/Abu_Dhabi_PM25_distribution_asthma.jpg',   
     quality = 100, bg = "white", res = 200, width = 11, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_PM25 <- ggplot(AQ_HEALTH_SUMMARY_STATS,  aes(mean_PM25)) + 
  theme_bw() +
#  geom_point(stat="bin", binwidth=5) +
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  ggtitle("PM2.5 distribution (Abu Dhabi - asthma hospital admissions 2011-2013)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  ylab("number of days") + 
  ylim(0, 200) +
  xlab(expression(paste(PM[2.5], " (µg/",m^3, ")", " "))) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=32),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=32)) 
p_PM25

par(oldpar)
dev.off()





jpeg('D:/R_processing/plots/Abu_Dhabi_Sum_Patients_distribution_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_sum_patients <- ggplot(AQ_HEALTH_SUMMARY_STATS,  aes(sum_patients)) + 
  theme_bw() + 
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  ylim(0, 250) +
  ggtitle("number of patients (Abu Dhabi, 2011-2013)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  xlab("admissions per day") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=32),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=32)) +
  ylab("number of days") 
p_sum_patients


par(oldpar)
dev.off()





BBB <- NULL
xxx= seq(from = 10, to = 150, by =10)

for (i in 1:length(xxx)){
  #i=2
  if (i==1){
    AAA <- AQ_HEALTH_SUMMARY_STATS %>%
      filter(mean_PM25 <= xxx[i])  %>%
      summarise(sum_patients = sum(sum_patients))
    num_day <- AQ_HEALTH_SUMMARY_STATS %>%
      filter(mean_PM25 <= xxx[i]) 
    num_day<-nrow(num_day)
    AAA<- AAA/num_day
  
  }else{
  AAA <- AQ_HEALTH_SUMMARY_STATS %>%
    filter(mean_PM25 <= xxx[i] & mean_PM25 >= xxx[i-1]) %>%
    summarise(sum_patients = sum(sum_patients))
  num_day <- AQ_HEALTH_SUMMARY_STATS %>%
    filter(mean_PM25 <= xxx[i] & mean_PM25 >= xxx[i-1]) 
  num_day<-nrow(num_day)
  AAA<- AAA/num_day
  }
  BBB<- rbind(BBB, AAA)
  
}

SUM_PATIENTS_BINS <- as.data.frame(cbind(xxx, BBB))
SUM_PATIENTS_BINS <- na.omit(SUM_PATIENTS_BINS)



jpeg('D:/R_processing/plots/Abu_Dhabi_counts_vs_PM25.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_health <- ggplot(SUM_PATIENTS_BINS, aes(xxx, sum_patients)) + 
  theme_bw() +
  geom_point(size=5) +
  geom_smooth() +
  ylim(0, 50) +
  ggtitle("number of patients per day (Abu Dhabi, 2011-2013)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  xlab("sum patients per day") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=32),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=32)) +
  xlab(expression(paste(PM[2.5], " (µg/",m^3, ")", " "))) +
  theme(legend.position="none") + 
  ylab("average admissions per day") 
 # xlim(10, 110)
p_health


par(oldpar)
dev.off()


########################################################################################
# Generalized Linear Model #############################################################
########################################################################################

library(survival)

# rms_fit_PM25_glm <- glm(sum_patients ~ rcs(mean_PM25, 3)+ Gender + AGE_BIN,
#                         family = Gamma(),
#                         data = AQ_HEALTH)

# rms_fit_PM25_glm <- glm(sum_patients ~ rcs(mean_PM25, 3), 
#                         family = Gamma(),
#                         data = AQ_HEALTH)

AQ_HEALTH_pos <- AQ_HEALTH %>%
  filter(detrend_counts > 0) %>%
  filter(detrend_PM25 > 0)


AQ_HEALTH_pos <- AQ_HEALTH %>%
  filter(detrend_counts > 5) %>%
  filter(detrend_counts < 24) %>%
  filter(detrend_PM25 > 0)



rms_fit_PM25_glm <- glm(detrend_counts ~ rcs(detrend_PM25, 3), 
                        family = poisson(),
                        data = AQ_HEALTH_pos, x=T, y=T)

par(mar=c(6,10,3,5))


termplot2(rms_fit_PM25_glm, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep(" ", times=3),
          xlab = "",
          # ylab=rep("RR", times=3),
          cex.lab=2, cex.axis=2.5,  cex.main = 4, ylim = c(-0.4, 0.4), # ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4),# ,   
          cex.lab=2, cex.axis=2.5,  cex.main = 2, las = 1, font=2,
          #  xlab = c("conc", "Gender"),
           # xlab = c((expression(paste(PM[2.5], " (µg/",m^3, ")")))),
           # main=  ("Health Response Curve for PM2.5 (Generalised Linear Model)"),
          #   main=  ("Hazard Ratio for asthma by gender (Generalised Linear Model)"),
          # main=  ("Relative Risk for asthma by age bins (Generalised Linear Model)"),
          col.se=rgb(.2,.2,1,.4), col.term="black")



abline(h=1, col="red", lty=3, lwd=2)
abline(v= 45, col="red", lty=3, lwd=2)

abline(h = y_min, col="red", lty=1, lwd=2)
abline(v = 32, col ="black", lty=1, lwd=3)


ymin <- min(exp(0.4))
ymax <- max(exp(-0.4))
x_val <- 0:100

par(new=TRUE)
plot(x_val, line_eq + 0.09, ylim=range(c(ymin, ymax)), axes = F, xlab = "", ylab = "",
     col="black",lty=1, lwd=3, type = "l")

###################################################################
###################################################################


#### fine treshold value where RR == 1
se = TRUE   # standard error
which.terms <- terms

terms <- if (is.null(terms))
  predict(rms_fit_PM25_glm, type = "terms", se.fit = se)

tms <- as.matrix(if (se)
  terms$fit
  else terms)


# check values above RR = 1
tms <- exp(tms)  # make data exponential
tms <- as.data.frame(tms)
# colnames(tms) <- c("RR", "Gender", "AGE_BIN")
colnames(tms) <- c("RR")

# AAA <- cbind(AQ_HEALTH$mean_PM25, tms)
AAA <- cbind(AQ_HEALTH_pos$detrend_PM25, tms)


# look where RR is > 1
# filter only RR > 1
RR_1 <- AAA %>%
  filter(RR >= 1)


########################################################################
########################################################################
# calculate the minimum PM2.5 concentration for the lower Relative Risk
########################################################################

data_lm <- AAA%>%
  filter(RR >= 1)
y_min <- min(AAA$RR)


data_lm <- data_lm %>%
  arrange(RR)
data_lm <- data_lm[1:100,]
xxx <- lm(RR~`AQ_HEALTH_pos$detrend_PM25`, data=data_lm)
xxx$coefficients
x_val <- 0:100
line_eq <- xxx$coefficients[1]+ xxx$coefficients[2]*x_val
plot(line_eq)
ind_intersection <- which(abs(y_min-line_eq) ==min(abs(y_min-line_eq)))


limit_PM25 <- (line_eq [ind_intersection ]- xxx$coefficients[1])/xxx$coefficients[2] 

########################################################################
########################################################################



# summary(rms_fit_PM25_glm) # display results
# anova(rms_fit_PM25_glm)

# confint(rms_fit_PM25_glm) # 95% CI for the coefficients
# exp(coef(rms_fit_PM25_glm)) # exponentiated coefficients
# exp(confint(rms_fit_PM25_glm)) # 95% CI for exponentiated coefficients
# predict(rms_fit_PM25_glm, type="response") # predicted values
# residuals(rms_fit_PM25_glm, type="deviance") # residuals

####################################
#### conficence interval (95%) #####
####################################

predAll <- predict(rms_fit_PM25_glm, type = "terms", se.fit = se)
upper_CI =  exp(predAll$fit + 1.96 *  predAll$se.fit)
lower_CI = exp(predAll$fit - 1.96 * predAll$se.fit)

AAA <- cbind(AQ_HEALTH_pos$detrend_PM25, tms, lower_CI, upper_CI)
colnames(AAA) <- c("detrend_PM25", "RR", "Lower_CI", "Upper_CI")


# filter only RR > 1
RR_1 <- AAA %>%
  filter(RR >= 1)

write.csv(RR_1, "D:/R_processing/RR_Abu_Dhabi.csv")

# number of mean daily hospital admissions
AQ_HEALTH_mean <-  AQ_HEALTH %>%
  summarise(mean_admissions = mean(sum_patients, na.rm = T),
            mean_admiss_detrend = mean(detrend_counts, na.rm = T))

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################




















##################################################################################
##################----------------------------------##############################
##################################################################################

### COX sURVIVAL MODEL------TRIAL-----------------------------------------------

library(survival)


# PM2.5------------------------------------------------------------------------------

# create a survival object
SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(sum_patients))
SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(detrend_counts))

# AQ_HEALTH$Date <- as.numeric(AQ_HEALTH$Date)
# SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(Date, sum_patients))

ddist <- datadist(AQ_HEALTH)
options(datadist="ddist")

# RCS = restricted cubic spline----log
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_HEALTH, x=T, y=T)
rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH, x=T, y=T)
rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(detrend_PM25, 4), data = AQ_HEALTH, x=T, y=T)


rms_fit_PM25
summary(rms_fit_PM25)

##------------------------------------------------------------------------------------------
## Check for violation of proportional hazard (constant HR over time) ----------------------
# https://stat.ethz.ch/R-manual/R-devel/library/survival/html/cox.zph.html
res.cox1 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_HEALTH, x=T, y=T)

res.cox1
res.zph1 <- cox.zph(res.cox1)
res.zph1

## Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve.
plot(res.zph1, df=2)



## plot ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# jpeg('D:/R_processing/plots/response_curve_PM25.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)


termplot2(rms_fit_PM25, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep("Hazard Ratio", times=3),
          cex.lab=1.5, cex.axis=2.5,  cex.main = 2, ylim = c(-2, 1.5), # ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4),# ,   
          cex.lab=1.5, cex.axis=1.5,  cex.main = 2, las = 2, font=2,
      #  xlab = c("conc", "Gender"),
          xlab = c((expression(paste(PM[2.5], " daily concentration (µg/",m^3, ")")))),
          main=  ("Health Response Curve for PM2.5 (COX moodel)"),
        #  main=  ("Hazard Ratio for asthma by gender"),
       # main=  ("Hazard Ratio for asthma by age bins"),
          col.se=rgb(.2,.2,1,.4), col.term="black")


abline(h=1, col="red", lty=3, lwd=3)
abline(v= 34.67, col="red", lty=3, lwd=3)

# par(oldpar)
# dev.off()


summary(rms_fit_PM25) # display results
confint(rms_fit_PM25) # 95% CI for the coefficients
exp(coef(rms_fit_PM25)) # exponentiated coefficients
exp(confint(rms_fit_PM25)) # 95% CI for exponentiated coefficients
predict(rms_fit_PM25, type="response") # predicted values
residuals(rms_fit_PM25, type="deviance") # residuals

# # RCS = restricted cubic spline----no log
# termplot2(rms_fit_PM25, se=T, rug.type="density", rug=T, density.proportion=.05,
#           se.type="polygon",
#           ylab=rep("Hazard Ratio"),
#           main=rep("response curve"),
#           col.se=rgb(.2,.2,1,.4), col.term="black")
# abline(h=0, col="red", lty=3)


# fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_data_PM25, x=T, y=T)
# fit_PM25
# summary(fit_PM25)
# 
# termplot2(fit_PM25, se=T, rug.type="density", rug=T, density.proportion=.05,
#           se.type="polygon",  yscale="exponential", log="y",
#           xlab = c("conc", "Gender"),
#           ylab=rep("Hazard Ratio", times=2),
#           main=rep("response curve", times=2),
#           col.se=rgb(.2,.2,1,.4), col.term="black")
# abline(h=1, col="red", lty=3)


#################################################################
#### calculate fit line and Confidence Intervals ################
#################################################################

# Get the the terms that are of interest

se = TRUE   # confidence interval

which.terms <- terms

terms <- if (is.null(terms))
  predict(rms_fit_PM25, type = "terms", se.fit = se)


tms <- as.matrix(if (se)
  terms$fitted
  else terms)


# check values above HR = 1
tms <- exp(tms)  # make data exponential
tms <- as.data.frame(tms)
colnames(tms) <- c("HR", "Gender", "AGE_BIN")
AAA <- cbind(AQ_HEALTH$mean_PM25, tms)
# look where HR is > 1



# filter only HR > 1
HR_1 <- AAA %>%
  filter(HR > 1)

# look at the position of RR ~ 1
abline(v= 34.67, col="red", lty=3, lwd=3)




# confidence interval (CI) (all data)

  tt <- 2 * terms$se.fit[,]
  upper_ci <- tms + tt
  lower_ci <- tms - tt
  
  
  # upper_ci <- exp(upper_ci)
  # lower_ci <- exp(lower_ci)


# Get the data used for the rug (density plot on the bottom)----------------------
  envir = environment(formula(rms_fit_PM25))
  
  mf <- model.frame(rms_fit_PM25)
  if (is.null(data))
    data <- eval(rms_fit_PM25$call$data, envir)
  if (is.null(data))
    data <- mf


##########################################################################################
######################           #########################################################
###################### GAM model #########################################################
######          GENERALISED ADDICTIVE MODEL      #########################################
######################           #########################################################
##########################################################################################

library(gam)  
  
rms_fit_PM25_gam <- gam(sum_patients ~ rcs(mean_PM25, 3) + Gender + AGE_BIN,
                        family = Gamma(),
                        data = AQ_HEALTH)

termplot2(rms_fit_PM25_gam, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep("Relative Risk", times=3),
          cex.lab=1.5, cex.axis=2.5,  cex.main = 2, ylim = c(-0.2, 0.1), # ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4),# ,   
          cex.lab=1.5, cex.axis=1.5,  cex.main = 2, las = 2, font=2,
          #  xlab = c("conc", "Gender"),
          xlab = c((expression(paste(PM[2.5], " daily concentration (µg/",m^3, ")")))),
          #   main=  ("Health Response Curve for PM2.5 (Generalised Linear Model)"),
          #   main=  ("Hazard Ratio for asthma by gender (Generalised Linear Model)"),
          main=  ("Relative Risk for asthma by age bins (Generalised Linear Model)"),
          col.se=rgb(.2,.2,1,.4), col.term="black")



abline(h=1, col="red", lty=3, lwd=3)
abline(v= 36.00, col="red", lty=3, lwd=3)

  


###########################################################################################
# Generalized Addictive Model #############################################################
###########################################################################################

library(gam)

rms_fit_PM25_gam <- gam(sum_patients ~ rcs(mean_PM25, 4) + Gender, family = poisson,
                        data = AQ_HEALTH)

termplot2(rms_fit_PM25_gam, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep("Hazard Ratio", times=3),
          cex.lab=1.5, cex.axis=2.5,  cex.main = 2, ylim = c(-0.5, 0.6), # ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4),# ,   
          cex.lab=1.5, cex.axis=1.5,  cex.main = 2, las = 2, font=2,
          #  xlab = c("conc", "Gender"),
          xlab = c((expression(paste(PM[2.5], " daily concentration (µg/",m^3, ")")))),
          #   main=  ("Health Response Curve for PM2.5"),
          #  main=  ("Hazard Ratio for asthma by gender"),
          main=  ("Relative Risk for asthma by age bins"),
          col.se=rgb(.2,.2,1,.4), col.term="black")





###-----------------------------------------------#######################################
###-----------------------------------------------#######################################

















  
  
    
  
###########################################################################  
## kind of POPULATION ATTRIBUTABLE FRACTION (%) ###########################

  AAA$PAF <- ((AAA$HR -1)/AAA$HR) *100
  AAA <- as.data.frame(AAA)  
  
    p_PAF <- ggplot(AAA, aes(PAF)) +
    geom_histogram(binwidth= 1, colour="black", fill="white") +
    theme_bw() 
    p_PAF
    
    
    
    p_PM25 <- ggplot(AQ_data_PM25, aes(mean_PM25)) +
      geom_histogram(binwidth= 5, colour="black", fill="white") +
      theme_bw() 
    p_PM25
    
  
  
#####################################################################
#####################################################################
#####################################################################

# calculate postion of the intercept..and therefore the limit value
# predict_PM25 <- rms_fit_PM25$x
# HAZARD_RATIO <- rms_fit_PM25$linear.predictors
# HAZARD_RATIO_res <- rms_fit_PM25$residuals
# data <- cbind(predict_PM25[,1], HAZARD_RATIO)
  
# combine the data together-----------------------------------------
# data <- as.data.frame(data)



# have a look a the data with HR > 0
# data <- data %>%
#  filter(HAZARD_RATIO >= 0)


# sort(data$HAZARD_RATIO)









# nice cursor plot with the RR--------------------------------------------

# PM10----------------------------------------------------------------------
fit_PM10 = cph(SurvObj_patients_PM10 ~ rcs(mean_PM10, 4), data = AQ_data_PM10, x=T, y=T)
ddist <- datadist(AQ_data_PM10)
options(datadist="ddist")

# PM2.5----------------------------------------------------------------------
fit_PM25 = cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4), data = AQ_data_PM25, x=T, y=T)
ddist <- datadist(AQ_data_PM25)
options(datadist="ddist")

fit_PM25
plot(summary(fit_PM25), log=T)

# O3------------------------------------------------------------------------

fit_O3 = cph(SurvObj_patients_O3 ~ rcs(mean_O3, 4), data = AQ_data_O3, x=T, y=T)
ddist <- datadist(AQ_data_O3)
options(datadist="ddist")
fit_O3
plot(summary(fit_O3), log=T)
##------------------------------------------------------------------------
