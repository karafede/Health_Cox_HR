
### comparison between Health data and PM2.5 concentrations

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



health_data <- health_data %>%
  filter(Gender %in% c("Male", "Female"))


health_data$Gender <- as.factor(health_data$Gender)
str(health_data)

# make a column with bin to count patients
health_data$bin <- "1"
health_data$bin  <- as.numeric(health_data$bin)

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

health_data$AGE_BIN <- as.factor(health_data$AGE_BIN)



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

health_data$AGE_BIN <- as.factor(health_data$AGE_BIN)
health_data$Gender <- as.factor(health_data$Gender)
health_data$Facility <- as.factor(health_data$Facility)

str(health_data)


# count patients in each day (all genders all ages)
health_data_sum <- health_data %>%
  group_by(Date) %>%
           # Gender,
           # AGE_BIN) %>%
  summarise(sum_patients = sum(bin, na.rm = TRUE))
  health_data_sum <- na.omit(health_data_sum) 







###############################################################################
## load satellite data for PM2.5 from MODIS------------------------------------

PM25_AOD <- read_csv("PM10_PM25_2011_2016_MODIS.csv")

# get only data over Abu Dhabi -----------------------------------------------

# load station info over Abu Dhabi ----------------------------
EAD_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info.csv")
PM25_AOD$Pollutant <- "PM2.5"

# attach infos to satellite PM2.5 data
PM25_AOD <- EAD_info %>%
  left_join(PM25_AOD, c("Site", "Pollutant", "Latitude", "Longitude"))

# remove all lines with NA
PM25_AOD <- na.omit(PM25_AOD)


# get daily means ######################################
PM25_AOD <- PM25_AOD %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(AOD_PM25, na.rm = TRUE))



# join PM2.5 data and health data ######################
AQ_data_PM25 <- PM25_AOD %>%
  left_join(health_data_sum, "Date")
AQ_data_PM25[sapply(AQ_data_PM25,is.na)] = NA 

# remove all lines with NA
AQ_data_PM25 <- na.omit(AQ_data_PM25)
str(AQ_data_PM25)

min <- as.Date("2011-06-01") 
  max <- as.Date("2013-06-30") 

p_PM25 <- ggplot(AQ_data_PM25, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "PM2.5 mean")) +
  geom_smooth() +
  theme(legend.position="none") + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=20, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=15, colour = "black")) +
  ylab(expression(paste(PM[25], " (5g/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 200) +
  xlim(min, max)
#  scale_x_date(limits = c(min, max))
p_PM25


p_health <- ggplot(AQ_data_PM25, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "patients")) +
#  geom_smooth() +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=20, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=15, colour = "black")) +
  ylab("number. asthma symptoms") + 
  ylim(0, 750) +
xlim(min, max)
p_health


# combine plots
grid.newpage()
grid.draw(rbind(ggplotGrob(p_PM25), ggplotGrob(p_health), size = "last"))



################################################################################
# remove outliers #############################################################

# Get the values from smooth fit

AAA <- AQ_data_PM25
AAA$Date <- as.numeric(AAA$Date)  # need Date to be numeric
str(AAA)

smooth_health <- predict(loess(sum_patients ~ Date ,AAA), AAA$Date)
AQ_data_PM25$sum_patients_flat <- AQ_data_PM25$sum_patients - smooth_health
smooth_health <-as.data.frame(smooth_health)
smooth_PM25 <- predict(loess(mean_PM25 ~ Date ,AAA), AAA$Date)
smooth_PM25 <- as.data.frame(smooth_PM25)

# calculate the PM2.5 mean from soothed data------
MEAN_PM25 <- colMeans(smooth_PM25)

#### remove outliers function---------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 3 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#### ----------------------------------------------------------------------------------
#### ----------------------------------------------------------------------------------

PM25_no_outliers <- remove_outliers(AAA$mean_PM25)
par(mfrow = c(1, 2))
boxplot(AAA$mean_PM25)
boxplot(PM25_no_outliers)

# new table with additional smooth column without outliers-----
BBB <- cbind(AAA, PM25_no_outliers)

# smooth fit with mean_PM25 without outliers
smooth_PM25_no_outliers <- predict(loess(PM25_no_outliers ~ Date ,BBB), BBB$Date)
AQ_data_PM25$mean_PM25_flat <- BBB$PM25_no_outliers - smooth_PM25_no_outliers
smooth_PM25_no_outliers <- as.data.frame(smooth_PM25_no_outliers)

# calculate the PM2.5 mean from soothed data WITHOUT OUTLIERS ------------------
MEAN_PM25_no_outliers <- colMeans(smooth_PM25_no_outliers)


p_PM25_no_outliers <- ggplot(AQ_data_PM25, aes(Date, PM25_no_outliers)) + 
  theme_bw() +
  geom_line(aes(y = PM25_no_outliers, col = "PM25_no_outliers")) +
  geom_smooth() +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (5g/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 150)  
p_PM25_no_outliers


# combine plots
grid.newpage()
grid.draw(rbind(ggplotGrob(p_PM25_no_outliers), ggplotGrob(p_health), size = "last"))




############################################################################
############################################################################
################ subtract seasonal trend ###################################
############################################################################

# remake plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p_PM25_flat <- ggplot(AQ_data_PM25, aes(Date, mean_PM25_flat)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25_flat, col = "PM2.5 mean")) +
  geom_smooth() +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (5g/",m^3, ")", " 24h-mean"))) + 
  ylim(-30, 100)  
p_PM25_flat


p_health_flat <- ggplot(AQ_data_PM25, aes(Date, sum_patients_flat)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients_flat, col = "patients")) +
  geom_smooth() +
  theme(legend.position="none") + 
  ylab(" tot num. patients") + 
  ylim(-100, 300) 
p_health_flat

# combine plots
grid.newpage()
grid.draw(rbind(ggplotGrob(p_PM25_flat), ggplotGrob(p_health_flat), size = "last"))




