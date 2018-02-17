
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


setwd("D:/R_processing/Clinics 2013-2016")
source("D:/R_processing/termplot2.R")


# load clinics data from other Emirates except Dubai and Abu Dhabi

# health_data <- read_csv("D:/R_processing/Health data/Asthma_Raw_Data.csv")
# health_data[sapply(health_data,is.na)] = NA 
# health_data <- na.omit(health_data)

other_clinics_2013 <- read_csv("D:/R_processing/Clinics 2013-2016/Clinics_2013.csv")
other_clinics_2014_2016 <- read_csv("D:/R_processing/Clinics 2013-2016/Clinics_2014_2016.csv")
other_clinics_2013_2016 <- rbind(other_clinics_2013,
                                 other_clinics_2014_2016)
write_csv(other_clinics_2013_2016, "other_clinics_2013_2016.csv")


# load clinics data
other_clinics_2013_2016 <- read_csv("other_clinics_2013_2016.csv")
other_clinics_2013_2016 <- na.omit(other_clinics_2013_2016)



other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  mutate(DateTime = mdy_hm(`Diagnosis Date & Time`, tz = "UTC"),
         Date = date(DateTime),
         year = year(DateTime))

# rename Person- MPI & Diagnosis column
colnames(other_clinics_2013_2016)[2] <- "Person_MPI"
colnames(other_clinics_2013_2016)[12] <- "Diagnosis_Description"
colnames(other_clinics_2013_2016)[11] <- "Diagnosis_Code"
colnames(other_clinics_2013_2016)[7] <- "Gender"
colnames(other_clinics_2013_2016)[3] <- "Encounter_Type"
colnames(other_clinics_2013_2016)[9] <- "Age"
colnames(other_clinics_2013_2016)[10] <- "HOSPITAL"


##########################################################################################
# calculate TIME difference between hospital admissions for the SAME PATIENT (Person_MPI) 

other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  group_by(Person_MPI) %>%
  mutate(time_diff = c(0,as.numeric(diff(Date), units="days"))) 


# select unique person MPI identifier and 
 other_clinics_2013_2016 <- other_clinics_2013_2016[!duplicated(other_clinics_2013_2016[c("Person_MPI", "Diagnosis_Description" )]),]
 write_csv(other_clinics_2013_2016, "other_clinics_2013_2016_unique_MPI.csv")

# load data
other_clinics_2013_2016 <- read_csv("other_clinics_2013_2016_unique_MPI.csv")
diagnosis_description <-  other_clinics_2013_2016[!duplicated(other_clinics_2013_2016["Diagnosis_Description"]),]
diagnosis_code <-  other_clinics_2013_2016[!duplicated(other_clinics_2013_2016["Diagnosis_Code"]),]
hospitals <-  other_clinics_2013_2016[!duplicated(other_clinics_2013_2016["HOSPITAL"]),]

write.csv(hospitals, "hospital.csv")


# count patients onle once
other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  filter(time_diff >= 0)

#### FIND HOPSITALS on the MAP!!!!!!!!!!!!!!!!!!! (26 facilities)

# create a column with simplified IC9 code
other_clinics_2013_2016$IC9 <- trunc(other_clinics_2013_2016$Diagnosis_Code, 3)


# write_csv(other_clinics_2013_2016, "other_clinics_2013_2016_unique_MPI.csv")

###########################################################################################################
# filter only selected IC9 CODE according to George Thruston proposal (NYU --> EAD) #######################
###########################################################################################################

# select health outcome of about respiratory and  (this will be assigned to STATUS = 1)
#### diseases for cardiac, respiratory system

RESPIRATORY_other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  filter(IC9 %in% c(464:466, 
                    480:487,
                    490:492, 494:496, 493))

# make a column with bin to count patients with the above Health Outcomes
RESPIRATORY_other_clinics_2013_2016$disease <- "RESPIRATORY"


CARDIO_other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  filter(IC9 %in% c(410,
                    413:414,
                    416,
                    426:427,
                    428,
                    430:437))
CARDIO_other_clinics_2013_2016$disease <- "CARDIO"


CONTROLS_other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  filter(IC9 %in% c(430:437))
CONTROLS_other_clinics_2013_2016$disease <- "CONTROLS"


INJURIES_other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  filter(IC9 %in% c(800:849))
INJURIES_other_clinics_2013_2016$disease <- "INJURIES"

# combine in an unique dataset
health_data <- rbind(RESPIRATORY_other_clinics_2013_2016,
                     CARDIO_other_clinics_2013_2016,
                     CONTROLS_other_clinics_2013_2016,
                     INJURIES_other_clinics_2013_2016)


health_data <- health_data %>%
  filter(Gender %in% c("Male", "Female"))

health_data$Gender <- as.factor(health_data$Gender)
health_data$disease <- as.factor(health_data$disease)
str(health_data)

# select only respiratory health outcomes

health_data <- health_data %>%
  filter(disease == 'RESPIRATORY')
health_data$bin <- 1

#### categorise disease with 1 == Respiratory, 2 == Cardio, Controls and Imjuries #####

# health_data$bin <- data.frame()
# for (i in 1:nrow(health_data)){
#   if (health_data$disease[i] == "RESPIRATORY"){
#     health_data$bin[i] <- 1
#   }
#   if (health_data$disease[i] == "CARDIO"){
#     health_data$bin[i] <- 0
#   }
#   if (health_data$disease[i] == "CONTROLS"){
#     health_data$bin[i] <- 0
#   }
#   if (health_data$disease[i] == "INJURIES"){
#     health_data$bin[i] <- 0
#   }
# }


health_data <- health_data %>%
  filter(Encounter_Type %in% c("Outpatient", 
                               "RX Outpatient Lifetime",
                               "Recurring Outpatient"))


# health_data <- health_data %>%
#   filter(!CitizenShip == "Non-Citizen")



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

health_data$AGE <- health_data$Age

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
          #  avg_time_diff = mean(time_diff, nam.rm = TRUE))

#### filter health data for years < 2016

health_data <- health_data %>%
  filter(Date <= "2015-10-11")


# quick plot ######################################


jpeg('D:/R_processing/plots/Dubai_Northern_Clinics_patients_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


min <- as.Date("2013-01-02") 
max <- as.Date("2015-10-11") 

# min <- as.Date("2015-03-20") 
# max <- as.Date("2015-04-30") 


# count patients in each day (all genders all ages)
health_data_sum <- health_data %>%
  group_by(Date) %>%
  summarise(sum_patients = sum(sum_patients, na.rm = TRUE))
  

plot <- ggplot(health_data_sum, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="blue") +
  # stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression("sum patients per day")) + 
  ggtitle("counts admissions (Dubai and Northern Emirates - respiratory, 2013-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylim(0, 170) + 
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
  summarise(sum_patients = sum(sum_patients, na.rm = TRUE))


#### filter health data for years < 2016

health_data <- health_data %>%
  filter(Date <= "2015-10-11")


health_data_TOTAL <- health_data %>%
  summarise(sum = sum(sum_patients))


# quick plot #################

min <- as.Date("2013-01-01") 
max <- as.Date("2015-09-01") 

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
  ylim(0, 200) + 
  xlim(min, max) 
plot



# calculate ANNUAL MEAN of counts over the years of data

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


# bind the above averaged to the health data to create a typical seasona trend
counts_season <- rbind(health_annual_mean[2:365, ],    # 2013
                       health_annual_mean[1:365, ],    # 2014
                       health_annual_mean[1:284, ])    # 2015

counts_season <- as.data.frame(counts_season)

health_data <- cbind(health_data, counts_season)





jpeg('D:/R_processing/plots/Seasonality_Dubai_Northern_Clinics_patients_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(health_data, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="red") +
#  geom_line(aes(y = daily_counts_seasons, col = "daily_counts_seasons"), alpha=1, col="blue") +
  geom_smooth(method="lm", aes(y = daily_counts_seasons, col = "daily_counts_seasons"), formula = y ~ poly(x, 26), size = 1, fill = "blue", col = "black") +  
    theme(legend.position="none") + 
#  stat_smooth(method = "loess") +
  ylab(expression("admissions (counts)")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=32, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32, colour = "black")) +
  ylim(0, 160) + 
  xlim(min, max) 
plot

par(oldpar)
dev.off()




# subtract the seasonal trend from the PM2.5 data and add ANNUAL MEAN
health_data$detrend_counts <- (health_data$sum_patients - health_data$daily_counts_seasons)  + ANNUAL_DAILY_COUNTS


jpeg('D:/R_processing/plots/NO_Seasonality_Dubai_Northern_Clinics_patients_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(health_data, aes(Date, sum_patients)) + 
  theme_bw() +
  # geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="red") +
  geom_line(aes(y = detrend_counts, col = "detrend_counts"), alpha=1, col="blue") +
  theme(legend.position="none") + 
  ylab(expression("admissions (counts)")) + 
  stat_smooth(method = "loess", fill = "red") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=32, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32, colour = "black")) +
  ylim(0, 150) + 
  xlim(min, max) 
plot


par(oldpar)
dev.off()


########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################

#####################################################
#### Load air quality data from 2013 to 2016 ########
#### use only DUBAI and NCMS data ###################
#### for year 2013 use satellite data ###############


PM25_AOD <- read_csv("D:/R_processing/PM10_PM25_2011_2016_MODIS.csv")

# select unique Site name in the PM25_AOD satellited data file
sites_PM25_AOD <- PM25_AOD[!duplicated(PM25_AOD[c("Site", "Latitude", "Longitude")]),]

# load new names stations....to check and change....
DM_new_names <- read.csv("D:/AQ_data/new_name_Stations/Stations_DM_2.csv")
DM_new_names <- DM_new_names %>%
  select(Site, 
         longitude,
         Latitude)
NCMS_new_names <- read.csv("D:/AQ_data/new_name_Stations/Stations_NCMS_2.csv")
NCMS_new_names <- NCMS_new_names %>%
  select(Site, 
         longitude,
         Latitude)
EAD_new_names <- read.csv("D:/AQ_data/new_name_Stations/Stations_EAD_2.csv")
EAD_new_names <- EAD_new_names %>%
  select(Site, 
         longitude,
         Latitude)

new_sites_names <- rbind(EAD_new_names, DM_new_names, NCMS_new_names)
new_sites_names <- na.omit(new_sites_names)
str(new_sites_names)
new_sites_names$Site <- as.character(new_sites_names$Site)
new_sites_names$Site <- sort(new_sites_names$Site)


# load station info over DM and NCMS ----------------------------
DM_info <- read.csv("D:/AQI/Stations_DM_info.csv")
NCMS_info <- read.csv("D:/AQI/Stations_NCMS_info.csv")

site_info <- rbind(DM_info,
                   NCMS_info)


PM25_AOD$Pollutant <- "PM2.5"


# attach infos to satellite PM2.5 data (only for Dubai and NCMS stations)
PM25_AOD <- site_info %>%
  left_join(PM25_AOD, c("Site", "Pollutant", "Latitude", "Longitude"))


PM25_AOD <- PM25_AOD %>%
  filter(Date < "2017-01-01" & Date >= "2013-01-01") %>%
  filter(Pollutant == "PM2.5")

PM25_AOD <- PM25_AOD %>%
  select(Date,
         Site,
         AOD_PM25)
names(PM25_AOD)[names(PM25_AOD) == 'AOD_PM25'] <- 'Daily_mean'

# PM25_AOD <- PM25_AOD %>%
#   group_by(Date) %>%
#   summarise(mean_PM25 = mean(AOD_PM25, na.rm = TRUE))

##########################################################
##### CHANGE NAMES to SITES where neccessary #############
##########################################################


# change names
# PM25_AOD$Site  <- ifelse(grepl("Bain Al Jesrain", PM25_AOD$Site, ignore.case = TRUE), 
#                          "Bain Aljesrain", PM25_AOD$Site)
# PM25_AOD$Site  <- ifelse(grepl("DUBAI AIR PORT", PM25_AOD$Site, ignore.case = TRUE), 
#                          "Dubai Airport", PM25_AOD$Site)
# PM25_AOD$Site  <- ifelse(grepl("EMIRATES HILLS", PM25_AOD$Site, ignore.case = TRUE), 
#                          "Emirates Hills", PM25_AOD$Site)
# PM25_AOD$Site  <- ifelse(grepl("JEBEL ALI PORT", PM25_AOD$Site, ignore.case = TRUE), 
#                          "Jebel Ali Port", PM25_AOD$Site)
# PM25_AOD$Site  <- ifelse(grepl("JEBEL ALI VILLAGE", PM25_AOD$Site, ignore.case = TRUE), 
#                          "Jebel Ali Village", PM25_AOD$Site)
# PM25_AOD$Site  <- ifelse(grepl("MUSHRIF", PM25_AOD$Site, ignore.case = TRUE), 
#                          "Mushrif", PM25_AOD$Site)
# PM25_AOD$Site  <- ifelse(grepl("SHK. MOHD. BIN ZAYED ROAD", PM25_AOD$Site, ignore.case = TRUE), 
#                          "SHK. Mohd. Bin Zayed Road", PM25_AOD$Site)
# PM25_AOD$Site  <- ifelse(grepl("SHK. ZAYED ROAD", PM25_AOD$Site, ignore.case = TRUE), 
#                          "SHK.Zayed Road", PM25_AOD$Site)



####################################################################
##### get daily data from Monitoring station for 2014, 2015 and 2016

# dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"
# dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"
# dir_AQ <- "D:/AQ_data/daily/Daily filtered with 4 boxplot"


# EAD_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2013 _daily_filtered.csv"))
# EAD_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2014 _daily_filtered.csv"))
# EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2015 _daily_filtered.csv"))
# EAD_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2016 _daily_filtered.csv"))
# EAD_AQ_2016_new <- read.csv("D:/AQ_data/daily/database_EAD_2016_daily_mean_full.csv")

# DM_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2013 _daily_filtered.csv"))[-1]
# DM_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2014 _daily_filtered.csv"))[-1]
# DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2015 _daily_filtered.csv"))[-1]
# DM_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2016 _daily_filtered.csv"))[-1]
# 
# NCMS_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_NCMS_ 2013 _daily_filtered.csv"))[-1]
# NCMS_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_NCMS_ 2014 _daily_filtered.csv"))[-1]
# NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_NCMS_ 2015 _daily_filtered.csv"))[-1]
# NCMS_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_NCMS_ 2016 _daily_filtered.csv"))[-1]


# use new processed AQ data
dir_AQ <- "D:/AQ_data/hourly_FK_new/hourly_data/test_FK/Daily_mean"

DM_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_2013_daily_mean.csv"))[-1]
DM_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_2014_daily_mean.csv"))[-1]
DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_2015_daily_mean.csv"))[-1]
DM_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_2016_daily_mean.csv"))[-1]

NCMS_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2013_daily_mean.csv"))[-1]
NCMS_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2014_daily_mean.csv"))[-1]
NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2015_daily_mean.csv"))[-1]
NCMS_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2016_daily_mean.csv"))[-1]

EAD_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_EAD_2013_daily_mean.csv"))[-1]
EAD_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_EAD_2014_daily_mean.csv"))[-1]
EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD_2015_daily_mean.csv"))[-1]
EAD_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_EAD_2016_daily_mean.csv"))[-1]


# bind data together BUT NO Abu Dhabi data
UAE_AQ <- rbind( # EAD_AQ_2013, EAD_AQ_2014, EAD_AQ_2015, EAD_AQ_2016,
                DM_AQ_2013, DM_AQ_2014, DM_AQ_2015, DM_AQ_2016,
                NCMS_AQ_2013, NCMS_AQ_2014, NCMS_AQ_2015, NCMS_AQ_2016)

# UAE_AQ <- rbind(EAD_AQ_2013, EAD_AQ_2014, EAD_AQ_2015, EAD_AQ_2016)


UAE_AQ$Site <- as.character(UAE_AQ$Site)
UAE_AQ$Site_Type <- as.character(UAE_AQ$Site_Type)
UAE_AQ$Pollutant <- as.character(UAE_AQ$Pollutant)
str(UAE_AQ)


# filter only PM2.5 data after ------------------------------------------------

PM25_measures <- UAE_AQ %>%
  mutate(year = year(Date)) %>%
  filter(Pollutant == "PM2.5") 
#  filter(year > 2013)



# #### remove outliers function---------------------------------------------------------
# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }
# 
# 
# PM25_measures_no_outliers <- remove_outliers(PM25_measures$Daily_mean)
# PM25_measures <- cbind(PM25_measures, PM25_measures_no_outliers)
# 
# 
# Annual_means <- PM25_measures %>%
#   group_by(year) %>%
#   summarise(mean=mean(PM25_measures_no_outliers, na.rm = TRUE))
# # summarise(mean=mean(Daily_mean, na.rm = TRUE))
# 
# Annual_mean <- Annual_means %>%
#   summarise(mean=mean(mean, na.rm = T))




PM25_measures <- PM25_measures %>%
  select(Date,
         Site,
         Daily_mean)

# bind satellite and measurements data together -----------------------------

PM25_all <- rbind(PM25_AOD,
                  PM25_measures)

# make daily averages for DM and NCMS data ----------------------------------
PM25_all <- PM25_all %>%
  group_by(Date) %>%
  summarise(mean_PM25  = mean(Daily_mean, na.rm = TRUE))

# PM25_all[sapply(PM25_all,is.na)] = NA 
# PM25_all <- na.omit(PM25_all)

# quick plot #################


jpeg('D:/R_processing/plots/Dubai_Northern_Clinics_PM25_Daily_Average.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(PM25_all, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="blue") +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ggtitle("Daily PM2.5 concentration (Dubai and Northern Emirates)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 300)  
plot

par(oldpar)
dev.off()



#### remove outliers function---------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 4.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

####################################################################################


# remove outliers-----------------------------------------
PM25_all_no_outliers <- remove_outliers(PM25_all$mean_PM25)

# new table with additional smooth column without outliers-----
PM25_all <- cbind(PM25_all, PM25_all_no_outliers)

PM25_all <- PM25_all %>%
  select(Date,
         PM25_all_no_outliers)
names(PM25_all)[names(PM25_all) == 'PM25_all_no_outliers'] <- 'mean_PM25'


plot <- ggplot(PM25_all, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="blue") +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 150)  
plot




#######################################################################################
################# DO NOT RUN THIS PART ################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
##### remove seasonality.......fit Curve with a sinusoidal function ###################
#######################################################################################

# Time <- as.numeric(PM25_all$Date)
# PM25 <- PM25_all$mean_PM25
# 
# xc<-cos(2*pi*Time/366)
# xs<-sin(2*pi*Time/366)
# fit.lm <- lm(PM25 ~ xc + xs)
# 
# # access the fitted series (for plotting)
# fit <- fitted(fit.lm)  
# plot(fit)
# 
# # find predictions for original time series
# pred <- predict(fit.lm, newdata=data.frame(Time=Time))    
# plot(pred)
# 
# 
# PM25_all <- cbind(PM25_all, pred) 

# calculate ANNUAL MEAN over the years of data

PM25_annual <- PM25_all %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(annual_PM25 = mean(mean_PM25, na.rm = TRUE))
ANNUAL_MEAN <- mean(PM25_annual$annual_PM25)


# make an average of all daily concentrations over the years (seasonality)

PM25_season <- PM25_all %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(month,
           day) %>%
  summarise(season_PM25 = mean(mean_PM25, na.rm = TRUE))

 
# DATE <- as.data.frame(PM25_all[,1])
# colnames(DATE)[1] <- "Date"

# bind the above averaged to the PM2.5 data

PM25_AVG_Jan_Feb <- PM25_season[1:28 ,]
PM25_AVG_March_Dec <- PM25_season[30:366 ,]
PM25_AVG_no_leap <- rbind(PM25_AVG_Jan_Feb, PM25_AVG_March_Dec)
PM25_leap <- PM25_season

PM25_season <- rbind(PM25_AVG_no_leap,    # 2013
                     PM25_AVG_no_leap,    # 2014
                     PM25_AVG_no_leap,    # 2015
                     PM25_leap)    # 2016

str(PM25_all)
str(PM25_season)
PM25_season <- as.data.frame(PM25_season)

PM25_all <- cbind(PM25_all, PM25_season)



min <- as.Date("2013-01-01") 
max <- as.Date("2015-09-01") 


jpeg('D:/R_processing/plots/Seasonality_Dubai_Northern_PM25.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(PM25_all, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="red") +
#  geom_line(aes(y = season_PM25, col = "season"), alpha=1, col="blue") +
  geom_smooth(method="lm", aes(y = season_PM25, col = "season_PM25"), formula = y ~ poly(x, 26), size = 1, fill = "blue", col = "black") +  
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=32, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32, colour = "black")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 150) +
  xlim(min, max)

plot

par(oldpar)
dev.off()





# subtract the seasonal trend from the PM2.5 data and add ANNUAL MEAN
PM25_all$detrend_PM25 <- (PM25_all$mean_PM25 - PM25_all$season_PM25)  + ANNUAL_MEAN





jpeg('D:/R_processing/plots/NO_Seasonality_Dubai_Northern_PM25.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)



plot <- ggplot(PM25_all, aes(Date, mean_PM25)) + 
  theme_bw() +
#  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="red") +
  geom_line(aes(y = detrend_PM25, col = "season"), alpha=1, col="blue") +
  stat_smooth(method = "loess", fill = "red") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=32, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32, colour = "black")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(-10, 120)  +
  xlim(min, max)
plot

par(oldpar)
dev.off()


#################################################################################################
##### RUN THIS PART #############################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#### join health data with PM2.5 daily concentrations ###########################################


# introduce a LAG in the health data (shift admission to hosptitals by 1 days)

str(health_data)
health_data <- as.data.frame(health_data)
health_data <- health_data %>%
  mutate(Date = Date -0)  # subtract x number of days


AQ_HEALTH <- PM25_all %>%
  left_join(health_data, "Date")
AQ_HEALTH[sapply(AQ_HEALTH,is.na)] = NA 

# remove all lines with NA
AQ_HEALTH <- na.omit(AQ_HEALTH)
str(AQ_HEALTH)



# AQ_data_PM25_sum_admissions <- AQ_HEALTH %>%
#  group_by(Gender) %>%
#    summarise(sum_admissions = sum(sum_patients, na.rm = TRUE))

############################################################
########## SAVE DATA #######################################
############################################################

# write.csv(AQ_HEALTH, "health_data_daily_counts.csv")
write.csv(AQ_HEALTH, "HEALTH_DATA_PM25_COUNTS_Other_EMIRATES.csv")
AQ_HEALTH <- read.csv("HEALTH_DATA_PM25_COUNTS_Other_EMIRATES.csv")




#############################################################
##### Summary STATISTICAL plots #############################
#############################################################


#!!!!!!!!!!!!!### remember to change to DETREND data when NECESSARY!!!!!

# average all variables by day

# AQ_HEALTH_SUMMARY_STATS <- AQ_HEALTH %>%
#   group_by(Date) %>%
#   summarise(mean_PM25 = mean(mean_PM25),
#             sum_patients = sum(sum_patients))


AQ_HEALTH_SUMMARY_STATS <- AQ_HEALTH %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(detrend_PM25),
            sum_patients = sum(detrend_counts))


jpeg('D:/R_processing/plots/Dubai_Northern_Clinics_PM25_distribution_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_PM25 <- ggplot(AQ_HEALTH_SUMMARY_STATS,  aes(mean_PM25)) + 
  theme_bw() +
  #  geom_point(stat="bin", binwidth=5) +
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  ggtitle("PM distribution (Dubai and Northern Emirates, 2013-2015)") + 
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





jpeg('D:/R_processing/plots/Dubai_Northern_Clinics_Sum_Patients_distribution_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_sum_patients <- ggplot(AQ_HEALTH_SUMMARY_STATS,  aes(sum_patients)) + 
  theme_bw() +
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  ylim(0, 200) +
  ggtitle("number of patients per day (Dubai and Northern Emirates, 2013-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  xlab("admissions per day") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=32),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=32)) +
  ylab("number of days") +
xlim(0, 150)
p_sum_patients

par(oldpar)
dev.off()


#### find relation between sum patiensts and PM25 concentration


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
    # normalise
    AAA<- AAA/num_day
    
  }else{
    AAA <- AQ_HEALTH_SUMMARY_STATS %>%
      filter(mean_PM25 <= xxx[i] & mean_PM25 >= xxx[i-1]) %>%
      summarise(sum_patients = sum(sum_patients))
    num_day <- AQ_HEALTH_SUMMARY_STATS %>%
      filter(mean_PM25 <= xxx[i] & mean_PM25 >= xxx[i-1]) 
    num_day<-nrow(num_day)
    # normalise
    AAA<- AAA/num_day
  }
  BBB<- rbind(BBB, AAA)
  
}


SUM_PATIENTS_BINS <- as.data.frame(cbind(xxx, BBB))
SUM_PATIENTS_BINS <- na.omit(SUM_PATIENTS_BINS)


########################################################################################


jpeg('D:/R_processing/plots/Dubai_Northern_Clinics_counts_vs_PM25.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_health <- ggplot(SUM_PATIENTS_BINS, aes(xxx, sum_patients)) + 
  theme_bw() +
  geom_point(size = 5) +
  geom_smooth() +
  ylim(0, 100) +
  ggtitle("number of patients per day (Dubai and Northern Emirates, 2013-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  xlab("sum patients per day") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=32),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=32)) +
  xlab(expression(paste(PM[2.5], " (µg/",m^3, ")", " "))) +
  theme(legend.position="none") + 
  ylab("average admissions per day") +
  xlim(5, 125)
#  ylim(0, 750) 
p_health

par(oldpar)
dev.off()


curve_admissions_PM25 <- predict(loess(sum_patients ~ xxx ,SUM_PATIENTS_BINS),
                                 SUM_PATIENTS_BINS$xxx)

# xxx is the PM2.5 concentration

fit_admission_PM25 <- lm(sum_patients ~ xxx, data=SUM_PATIENTS_BINS)
summary(fit_admission_PM25) 

# show results
# Call:
#   lm(formula = sum_patients ~ xxx, data = SUM_PATIENTS_BINS)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.5865 -3.1542  0.0537  3.2174  8.9955 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 41.48075    3.39879  12.205 2.49e-07 ***
#   xxx          0.20847    0.04618   4.514  0.00112 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.522 on 10 degrees of freedom
# Multiple R-squared:  0.6708,	Adjusted R-squared:  0.6379 
# F-statistic: 20.38 on 1 and 10 DF,  p-value: 0.001118




###############################################################################################################
####    Generalised linear model   ############################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
#library(survival)

# SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(sum_patients))

# rms_fit_PM25_glm <- glm(sum_patients ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, family = Gamma(),
#                         data = AQ_HEALTH, x=T, y=T)

# rms_fit_PM25_glm <- glm(sum_patients ~ rcs(mean_PM25, 4), family = Gamma(),
#                         data = AQ_HEALTH, x=T, y=T)


### Apply Poisson Regression

AQ_HEALTH_pos <- AQ_HEALTH %>%
  filter(detrend_PM25 > 0) %>%
  filter(detrend_counts > 0)


rms_fit_PM25_glm <- glm(detrend_counts ~ rcs(detrend_PM25, 3), family = poisson(),
                          data = AQ_HEALTH_pos, x=T, y=T)
summary(rms_fit_PM25_glm)


# Call:
#   glm(formula = detrend_counts ~ rcs(detrend_PM25, 3), family = poisson(), 
#       data = AQ_HEALTH_pos, x = T, y = T)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -9.926  -1.882  -0.028   1.597   9.995  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                       3.8907142  0.0318635 122.106   <2e-16 ***
#   rcs(detrend_PM25, 3)detrend_PM25  0.0010975  0.0008199   1.339   0.1807    
# rcs(detrend_PM25, 3)detrend_PM25' 0.0020879  0.0008892   2.348   0.0189 *  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 9265.4  on 997  degrees of freedom
# Residual deviance: 9192.1  on 995  degrees of freedom
# AIC: Inf
# 
# Number of Fisher Scoring iterations: 4


par(mar=c(6,10,3,5))

# plot this later on (first make statistics calculations below)
termplot2(rms_fit_PM25_glm, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          # ylab=rep("Relative Risk", times=3),
          ylab = "",
          xlab = "",
          cex.lab=2, cex.axis=2.5,  cex.main = 4, ylim = c(-0.4, 0.4), # ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4),# ,   
          cex.lab=2, cex.axis=2.5,  cex.main = 2, las = 1, font=2,
          #  xlab = c("conc", "Gender"),
         # xlab = c((expression(paste(PM[2.5], " daily concentration (µg/",m^3, ")")))),
          #   main=  ("Health Response Curve for PM2.5 (Generalised Linear Model)"),
          #main=  ("Hazard Ratio for asthma by gender (Generalised Linear Model)"),
          # main=  ("Relative Risk for asthma by age bins (Generalised Linear Model)"),
          col.se=rgb(.2,.2,1,.4), col.term="black", lwd.term = 2)


abline(h=1, col="red", lty=1, lwd=2)
abline(v= 48, col="red", lty=3, lwd=2)
abline(v= 20, col="blue", lty=3, lwd=2)



abline(h = y_min, col="red", lty=1, lwd=2)
abline(v = 30, col ="black", lty=1, lwd=3)

ymin <- min(exp(0.4))
ymax <- max(exp(-0.4))
x_val <- 0:100

par(new=TRUE)
plot(x_val, line_eq + 0.105, ylim=range(c(ymin, ymax)), axes = F, xlab = "", ylab = "",
     col="black",lty=2, lwd=3, type = "l")

#######################################  
## calculate the relative risk RR #####
#######################################

se = TRUE   # standard error
which.terms <- terms


# The "terms" option returns a matrix giving the fitted values of each term in the model
# formula on the linear predictor scale.

terms <- if (is.null(terms))
  predict(rms_fit_PM25_glm, type = "terms", se.fit = se)
tms <- as.matrix(if (se)
  terms$fit
  else terms)

#### find treshold value where RR == 1
# check values above RR = 1
# calculate the relative risk (Relative risk = exp(coef(model)))
tms <- exp(tms)  # make data exponential
tms <- as.data.frame(tms)
#  colnames(tms) <- c("RR", "Gender", "AGE_BIN")
colnames(tms) <- c("RR")
# AAA <- cbind(AQ_HEALTH$mean_PM25, tms)
AAA <- cbind(AQ_HEALTH_pos$detrend_PM25, tms)


# look where RR is > 1
# filter only RR > 1
RR_1 <- AAA %>%
  filter(RR >= 1)

# for confidence intervals (CI)
# exp(cbind(coefficients(rms_fit_PM25_glm), confint(rms_fit_PM25_glm)))


########################################################################
########################################################################

data_lm <- AAA%>%
  filter(RR>= 1)
y_min <- min(AAA$RR)


data_lm <- data_lm %>%
  arrange(RR)
data_lm <- data_lm[1:100,]  # increase of 5 ug/m3 circa
xxx <- lm(RR~`AQ_HEALTH_pos$detrend_PM25`, data=data_lm)
xxx$coefficients
x_val <- 0:100
line_eq<- xxx$coefficients[1]+ xxx$coefficients[2]*x_val
plot(line_eq)
ind_intersection <- which(abs(y_min-line_eq) ==min(abs(y_min-line_eq)))


limit_PM25 <- (line_eq [ind_intersection ]- xxx$coefficients[1])/xxx$coefficients[2] 

########################################################################
########################################################################


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

write.csv(RR_1, "D:/R_processing/RR_DUBAI_Northern.csv")

# number of mean daily hospital admissions
AQ_HEALTH_mean <-  AQ_HEALTH %>%
  summarise(mean_admissions = mean(sum_patients, na.rm = T),
            mean_admiss_detrend = mean(detrend_counts, na.rm = T),
            mean_PM25 = mean(mean_PM25),
            mean_PM25_detrend = mean(detrend_PM25))


#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################



##################################################################################
##################----------------------------------##############################
##################################################################################


### COX sURVIVAL MODEL------TRIAL-----------------------------------------------

library(survival)


# create a survival object
# we have chosen the a time-varying variable that is our survival object...	
# a formula object, with the response on the left of a ~ operator, and the terms on the right. 
# The response must be a survival object as returned by the Surv function.

#### survival object is the total number of patient admitted every day in hospitals. 


SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(sum_patients))

# with detrend
# SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(detrend_counts))

ddist <- datadist(AQ_HEALTH)
options(datadist="ddist")

# RCS = restricted cubic spline----log

# age
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(Age, 4) , data = AQ_HEALTH, x=T, y=T)

# mean PM25
rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_HEALTH, x=T, y=T)rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) , data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Age, data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4), data = AQ_HEALTH, x=T, y=T)


# detrend_PM25
#  rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(detrend_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH, x=T, y=T)
#  rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(detrend_PM25, 4) + Gender, data = AQ_HEALTH, x=T, y=T)
#  rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(detrend_PM25, 4) , data = AQ_HEALTH, x=T, y=T)
#  rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(detrend_PM25, 4) + Age, data = AQ_HEALTH, x=T, y=T)


# km.as.one <- survfit(SurvObj_patients_PM25 ~ 1, data = AQ_HEALTH, conf.type = "log-log")
# km.as.one <- survfit(SurvObj_patients_PM25 ~ Gender, data = AQ_HEALTH, conf.type = "log-log")
# plot(km.as.one)
 # how to get confidence interval
#  http://stackoverflow.com/questions/36941746/how-to-get-95-ci-from-rs-coxph

 # rms_fit_PM25 <- survfit(cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_data_PM25, x=T, y=T))
 
rms_fit_PM25
summary(rms_fit_PM25)

## plot ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# jpeg('D:/R_processing/plots/response_curve_PM25.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)


termplot2(rms_fit_PM25, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep("Hazard Ratio", times=3),
          ylim = c(-0.5, 0.5),
         # cex.lab=1.5, cex.axis=2.5,  cex.main = 2,# ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4), ylim = c(-0.5, 0.8),   
          cex.lab=1.5, cex.axis=1.5,  cex.main = 2, las = 2, font=2,
      #  xlab = c("conc", "Gender"),
          xlab = c((expression(paste(PM[2.5], " daily concentration (µg/",m^3, ")")))),
       #   main=  ("Health Response Curve for PM2.5"),
        #  main=  ("Hazard Ratio for asthma by gender"),
       main=  ("Hazard Ratio for respiratory diseases"),
          col.se=rgb(.2,.2,1,.4), col.term="black")


abline(h=1, col="red", lty=3, lwd=3)
abline(v= 42.84, col="red", lty=3, lwd=3)

# par(oldpar)
# dev.off()


#####################################################################
#### calculate fit line and Standard Error Intervals ################
#####################################################################

# Get the the terms that are of interest
se = TRUE   # standard error
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
  filter(HR >= 1)

# look at the position of RR ~ 1
abline(v= 42.84, col="red", lty=3, lwd=3)

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

  
  