
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


# select unique person MPI identifier
 other_clinics_2013_2016 <- other_clinics_2013_2016[!duplicated(other_clinics_2013_2016[c("Person_MPI", "Diagnosis_Description" )]),]
 write_csv(other_clinics_2013_2016, "other_clinics_2013_2016_unique_MPI.csv")

# load data
other_clinics_2013_2016 <- read_csv("other_clinics_2013_2016_unique_MPI.csv")

# list all diagnosis codes
diagnosis_description <-  other_clinics_2013_2016[!duplicated(other_clinics_2013_2016["Diagnosis_Description"]),]
diagnosis_code <-  other_clinics_2013_2016[!duplicated(other_clinics_2013_2016["Diagnosis_Code"]),]
# list all clinics
hospitals <-  other_clinics_2013_2016[!duplicated(other_clinics_2013_2016["HOSPITAL"]),]

write.csv(hospitals, "hospital.csv")


# count patients only once
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
  filter(IC9 %in% c(464:466,  # respiratory track infections
                    480:487,  # respiratroy track infections
                    490:492,  # chronic obstructive pulmonary disease
                    493))     # asthma

# make a column with bin to count patients with the above Health Outcomes
RESPIRATORY_other_clinics_2013_2016$disease <- "RESPIRATORY"


CARDIO_other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  filter(IC9 %in% c(410:414,  # ischemic heart disease
                    429,      # ischemic heart disease
                    426:427,  # heart rhythm disturbance
                    428,      # heart failure
                    430:438,  # cerebrovascular
                    440-448)) #peripeheral vascualr disease
 CARDIO_other_clinics_2013_2016$disease <- "CARDIO"


# CONTROLS_other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
#   filter(IC9 %in% c(430:437))
# CONTROLS_other_clinics_2013_2016$disease <- "CONTROLS"


INJURIES_other_clinics_2013_2016 <- other_clinics_2013_2016 %>%
  filter(IC9 %in% c(800:849))  # injuries
INJURIES_other_clinics_2013_2016$disease <- "INJURIES"

# combine in an unique dataset
# health_data <- rbind(RESPIRATORY_other_clinics_2013_2016)
                   #  CARDIO_other_clinics_2013_2016)
                   #  CONTROLS_other_clinics_2013_2016,
                    # INJURIES_other_clinics_2013_2016)

###################
# only injuries ##
##################
health_data <- rbind(INJURIES_other_clinics_2013_2016)



health_data <- health_data %>%
  filter(Gender %in% c("Male", "Female"))

health_data$Gender <- as.factor(health_data$Gender)
health_data$disease <- as.factor(health_data$disease)
str(health_data)

# select only respiratory health outcomes

# health_data <- health_data %>%
#   filter(disease == 'RESPIRATORY')

# consider all data
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
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) 
  # ylim(0, 170) + 
  # xlim(min, max) 
plot


output_folder <- "D:/R_processing/plots_new/"
png(paste0(output_folder,"Dubai_Northern_Clinics_patients_respiratory_control.jpg"),
    width = 1700, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
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
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) 
  # ylim(0, 200) + 
  # xlim(min, max) 
plot



# calculate ANNUAL MEAN of counts over the years of data

health_data_annual <- health_data %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(annual_daily_counts = mean(sum_patients, na.rm = TRUE))
ANNUAL_DAILY_COUNTS <- mean(health_data_annual$annual_daily_counts)




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
  ylim(0, 300)  
plot


output_folder <- "D:/R_processing/plots_new/"
png(paste0(output_folder,"Dubai_Northern_Clinics_PM25_Daily_Average.jpg"),
    width = 1700, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


### Temperature data from MERRA reanalysis ##############
#########################################################

# load Daily temperature data for the Northern Emirates (2013-2016)

Temp <- read.csv("D:/R_processing/Temperature_Data/Northern_EMIRATES/Mean_Temp_MERRA_North_UAE_2013_2016.csv")

Temp <- Temp %>%
  mutate(Date = mdy(Date))

# plot temperature data
plot <- ggplot(Temp, aes(Date, Temp)) + 
  theme_bw() +
  geom_line(aes(y = Temp, col = "Temp"), alpha=1, col="blue") +
  stat_smooth(method = "lm") +
  theme(legend.position="none") + 
  ggtitle("Daily Temperature") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  ylab(expression(paste("Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) 
plot

output_folder <- "D:/R_processing/plots_new/"
png(paste0(output_folder,"Dubai_Northern_Temperature.jpg"),
    width = 1700, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()

# Load Relative Humidiy data from MERRA

RH <- read.csv("D:/R_processing/Temperature_Data/Northern_EMIRATES/Mean_RH_Day_MERRA_North_UAE_2013_2016.csv")
# remove lines with -9999 in the RH column
RH <- RH[!RH$RH ==-9999, ]

RH <- RH %>%
  mutate(Date = mdy(Date))


# join PM2.5 data with Temperature data

PM25_all <- PM25_all %>%
  left_join(Temp , c("Date"))


# join PM2.5 data with RH data

PM25_all <- PM25_all %>%
  left_join(RH , c("Date"))


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

#  Join temperature data
# PM25_all <- PM25_all %>%
#   left_join(Temp , c("Date"))
# 
# #  Join RH data
# PM25_all <- PM25_all %>%
#   left_join(RH , c("Date"))


PM25_all <- PM25_all %>%
  select(Date,
         PM25_all_no_outliers,
         Temp,
         RH)
names(PM25_all)[names(PM25_all) == 'PM25_all_no_outliers'] <- 'mean_PM25'


plot <- ggplot(PM25_all, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="red") +
# stat_smooth(method = "lm") +
  geom_smooth(method="lm", aes(y = mean_PM25, col = "mean_PM25"), formula = y ~ poly(x, 26), size = 1, fill = "blue", col = "black") + 
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (µg/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylim(0, 150)  
plot


output_folder <- "D:/R_processing/plots_new/"
png(paste0(output_folder,"Dubai_Northern_Clinics_PM25_Daily_Average_NO_outliers.jpg"),
    width = 1700, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



#################################################################################################
##### RUN THIS PART #############################################################################
#################################################################################################
#################################################################################################
#################################################################################################

library(dlnm) ;library(splines)
library(foreign)
library(tsModel)
#install.packages("Epi")
library(Epi)
#install.packages("astsa")
library(astsa)


#### join health data with PM2.5 daily concentrations ############################

health_data <- health_data %>%
  left_join(PM25_all, c("Date"))

# add time variable to data (this is the JULIAN day????)
 health_data$time <- 1:nrow(health_data)
 
 health_data <- health_data %>%
   mutate(year = year(Date),
          month = month(Date),
          day = day(Date))


health_data <- health_data %>%
  filter(sum_patients > 0)
health_data <- health_data[!is.na(health_data$mean_PM25),]

###################################################################
#### find relation between sum patiensts and PM25 concentration ###
###################################################################

AQ_HEALTH_SUMMARY_STATS <- health_data %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(mean_PM25),
            sum_patients = sum(sum_patients))

BBB <- NULL
xxx= seq(from = 10, to = 150, by =10)
xxx= seq(from = 10, to = 150, by =5)


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


# plot

p_health <- ggplot(SUM_PATIENTS_BINS, aes(xxx, sum_patients)) + 
  theme_bw() +
  geom_point(size = 5) +
  geom_smooth() +
  ylim(0, 10) +
  ggtitle("(INJURIUES) - number of patients per day (Dubai and Northern Emirates, 2013-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  xlab("sum patients per day") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=32),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=32)) +
  xlab(expression(paste(PM[2.5], " (µg/",m^3, ")", " "))) +
  theme(legend.position="none") + 
  ylab("average admissions per day") +
  xlim(5, 150)
#  ylim(0, 750) 
p_health


output_folder <- "D:/R_processing/plots_new/"
png(paste0(output_folder,"CONTROL_Dubai_Northern_Clinics_counts_vs_PM25.jpg"),
    width = 1900, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(p_health)
dev.off()


curve_admissions_PM25 <- predict(loess(sum_patients ~ xxx ,SUM_PATIENTS_BINS),
                                 SUM_PATIENTS_BINS$xxx)

# xxx is the PM2.5 concentration

fit_admission_PM25 <- lm(sum_patients ~ xxx, data=SUM_PATIENTS_BINS)
summary(fit_admission_PM25) 

# Call:
#   lm(formula = sum_patients ~ xxx, data = SUM_PATIENTS_BINS)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.7181 -0.3621 -0.1199  0.2964  2.2991 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.827290   0.417811  11.554 2.65e-10 ***
#   xxx         -0.001149   0.005346  -0.215    0.832    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8829 on 20 degrees of freedom
# Multiple R-squared:  0.002306,	Adjusted R-squared:  -0.04758 
# F-statistic: 0.04622 on 1 and 20 DF,  p-value: 0.832





####################################################################
############## HEALTH ANALYSIS #####################################
####################################################################


# CREATING THE LAG in the PM2.5 data
cb <- crossbasis(health_data$mean_PM25, lag=c(0,5),argvar=list(fun="lin"),
                 arglag=list(fun="integer"))


# SUMMARY
summary(cb)

##########################################################
##########################################################

health_data$Hot.Humid[health_data$Temp > 30 & health_data$RH > 75] <- 1
# assign 0 value to all NA Hot.Humid data
health_data$Hot.Humid[is.na(health_data$Hot.Humid)] <- 0

# assign name of days of the week to each date

health_data$dow <- weekdays(as.Date(health_data$Date), abbreviate = T)


# WORKING days of the week (arabic countries)
health_data$d_o_w[health_data$dow =="Fri"] <- 1
health_data$d_o_w[health_data$dow =="Sat"] <- 0
health_data$d_o_w[health_data$dow =="Sun"] <- 0
health_data$d_o_w[health_data$dow =="Mon"] <- 0
health_data$d_o_w[health_data$dow =="Tue"] <- 0
health_data$d_o_w[health_data$dow =="Wed"] <- 0
health_data$d_o_w[health_data$dow =="Thu"] <- 0

# assign holidays

# introduce holidays
health_data$holidays[health_data$Date == "2013-01-01"] <- 1
health_data$holidays[health_data$Date == "2013-01-24"] <- 1
health_data$holidays[health_data$Date == "2013-06-06"] <- 1
health_data$holidays[health_data$Date == "2013-08-08"] <- 1
health_data$holidays[health_data$Date == "2013-08-09"] <- 1
health_data$holidays[health_data$Date == "2013-08-10"] <- 1
health_data$holidays[health_data$Date == "2013-10-14"] <- 1
health_data$holidays[health_data$Date == "2013-10-15"] <- 1
health_data$holidays[health_data$Date == "2013-10-16"] <- 1
health_data$holidays[health_data$Date == "2013-10-17"] <- 1
health_data$holidays[health_data$Date == "2013-11-03"] <- 1
health_data$holidays[health_data$Date == "2013-12-01"] <- 1
health_data$holidays[health_data$Date == "2013-12-02"] <- 1


health_data$holidays[health_data$Date == "2014-01-01"] <- 1
health_data$holidays[health_data$Date == "2014-01-12"] <- 1
health_data$holidays[health_data$Date == "2014-05-25"] <- 1
health_data$holidays[health_data$Date == "2014-06-29"] <- 1
health_data$holidays[health_data$Date == "2014-07-27"] <- 1
health_data$holidays[health_data$Date == "2014-07-28"] <- 1
health_data$holidays[health_data$Date == "2014-07-29"] <- 1
health_data$holidays[health_data$Date == "2014-07-30"] <- 1
health_data$holidays[health_data$Date == "2014-07-31"] <- 1
health_data$holidays[health_data$Date == "2014-10-03"] <- 1
health_data$holidays[health_data$Date == "2014-10-04"] <- 1
health_data$holidays[health_data$Date == "2014-10-25"] <- 1
health_data$holidays[health_data$Date == "2014-12-02"] <- 1
health_data$holidays[health_data$Date == "2014-12-03"] <- 1
health_data$holidays[health_data$Date == "2014-12-04"] <- 1

health_data$holidays[health_data$Date == "2015-01-01"] <- 1
health_data$holidays[health_data$Date == "2015-01-03"] <- 1
health_data$holidays[health_data$Date == "2015-05-16"] <- 1
health_data$holidays[health_data$Date == "2015-06-18"] <- 1
health_data$holidays[health_data$Date == "2015-07-16"] <- 1
health_data$holidays[health_data$Date == "2015-07-17"] <- 1
health_data$holidays[health_data$Date == "2015-07-18"] <- 1
health_data$holidays[health_data$Date == "2015-07-19"] <- 1
health_data$holidays[health_data$Date == "2015-07-20"] <- 1
health_data$holidays[health_data$Date == "2015-09-23"] <- 1
health_data$holidays[health_data$Date == "2015-09-24"] <- 1
health_data$holidays[health_data$Date == "2015-09-25"] <- 1
health_data$holidays[health_data$Date == "2015-09-26"] <- 1
health_data$holidays[health_data$Date == "2015-10-15"] <- 1
health_data$holidays[health_data$Date == "2015-12-01"] <- 1
health_data$holidays[health_data$Date == "2015-12-02"] <- 1
health_data$holidays[health_data$Date == "2015-12-03"] <- 1
health_data$holidays[health_data$Date == "2015-12-04"] <- 1


health_data$holidays[is.na(health_data$holidays)] <- 0

# 3 DAYS MOVING AVERAGE FOR TEMP & RH 

library(zoo)

health_data$Temp_3day <- rollapplyr(health_data$Temp,list(-(3:1)),mean,fill=NA)
health_data$RH_3day <- rollapplyr(health_data$RH,list(-(3:1)),mean,fill=NA)

# replace NA temperature values with 22.7 degrees celsius
health_data$Temp_3day[is.na(health_data$Temp_3day)] <- 22.7

# replace NA RH values with 64.5 %
health_data$RH_3day[is.na(health_data$RH_3day)] <- 64.5
health_data$RH[is.na(health_data$RH)] <- 64.5

# make all data integer
health_data$sum_patients <- as.integer(health_data$sum_patients)
health_data$mean_PM25 <- as.integer(health_data$mean_PM25)



# RUN THE GLM MODEL
# model <- glm(detrend_counts ~ cb + ns(Date,20)+ health_data$d_o_w +health_data$holidays + ns(Date, 6) +
#                ns(Temp_3day, 6)+ ns(RH,3)+ ns(RH_3day, 3)+ health_data$Hot.Humid, 
#              family=poisson(), health_data)


modx <- glm(sum_patients ~ cb + ns(time,12)+ health_data$dow + ns(Temp, 6) + health_data$holidays +
               ns(Temp_3day, 6)+ ns(RH,3)+ ns(RH_3day, 3),
             family=poisson(), health_data)

summary(modx)

# modx <- glm(health_data$detrend_counts ~ health_data$mean_PM25 +ns(time,20)+
#               health_data$d_o_w +health_data$holidays + 
#               ns(Temp, 6)+ ns(Temp_3day, 6)+  ns(RH,3)+ ns(RH_3day, 3)+ health_data$Hot.Humid,
#             family=poisson(),health_data)


modx <- glm(health_data$sum_patients ~ health_data$mean_PM25  + ns(time,12) + health_data$holidays +
            health_data$dow + ns(Temp, 6)+ ns(Temp_3day, 6)+  ns(RH,3)+ ns(RH_3day, 3),
            family=poisson(),health_data)

summary(modx)
# residual plot
# plot(modx)
plot(modx$residuals)


# modx <- glm(detrend_counts ~ rcs(mean_PM25, 3),
#             family=poisson(),health_data, na.action = na.exclude)

# modx <- glm(detrend_counts ~ mean_PM25,
#             family=poisson(),health_data, na.action = na.exclude)


# modx <- glm(detrend_counts ~ ns(mean_PM25,3) + ns(time,7)+
#               health_data$d_o_w + 
#               ns(Temp, 6)+ ns(Temp_3day, 6)+  ns(RH,3)+ ns(RH_3day, 3) + Hot.Humid,
#             family=poisson(),health_data, na.action = na.exclude)


se = TRUE   # standard error
predAll <- predict(modx, type = "terms", se.fit = se)
tms <- predAll$fit[,1]
tms <- exp(tms)
tms <- as.data.frame(tms)
colnames(tms) <- c("RR")
upper_CI =  exp(predAll$fit[,1] + 1.96 *  predAll$se.fit[,1])
lower_CI = exp(predAll$fit[,1] - 1.96 * predAll$se.fit[,1])

AAA <- cbind(health_data$mean_PM25, tms, lower_CI, upper_CI)
colnames(AAA) <- c("mean_PM25", "RR", "Lower_CI", "Upper_CI")

##############################
#### exposure curve ##########
##############################

plot <- ggplot(AAA, aes(mean_PM25, RR)) + 
  theme_bw() +
  geom_line(aes(y = RR, col = "RR"), alpha=1, col="red") +
  geom_line(aes(y = Lower_CI, col = "Lower_CI"), alpha=1, col="blue") +
  geom_line(aes(y = Upper_CI, col = "Upper_CI"), alpha=1, col="blue") + 
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 8)) + 
  ylab(expression(paste("Relative Risk"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  ylim(0.8, 1.4) +
  geom_hline(yintercept = 1) +
  ggtitle("glm(detrend_counts ~ mean_PM25 + ns(time,6)+
              health_data$dow + ns(Temp, 6)+ ns(Temp_3day, 6)+  ns(RH,3)+ ns(RH_3day, 3),
          family=poisson(),health_data)")
plot


output_folder <- "D:/R_processing/plots_new/"
png(paste0(output_folder,"RR_glm_CONTROL.jpg"),
    width = 1700, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


#################################################
#### residuals ##################################


modx <- glm(sum_patients ~ cb + ns(time,12)+ health_data$dow +health_data$holidays + ns(Temp, 6)+
              ns(Temp_3day, 6)+ ns(RH,3)+ ns(RH_3day, 3)+ health_data$Hot.Humid,family=quasipoisson(),health_data)

summary(modx)  # is does not give AIC (use poisson() to get value for AIC)

acf2(residuals(modx))
res <- residuals(modx, type="deviance")
summary(res)
plot(res,ylim=c(-10,10),pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time glm(sum_patients ~ cb + ns(time,12)+ health_data$dow +health_data$holidays + ns(Temp, 6)+
     ns(Temp_3day, 6)+ ns(RH,3)+ ns(RH_3day, 3)+ health_data$Hot.Humid,family=quasipoisson(),health_data)",ylab="Deviance residuals",xlab="date")
abline(h=0,lty=2,lwd=2)



modx <- glm(sum_patients ~ mean_PM25 + ns(time,12)+ health_data$dow +health_data$holidays + ns(Temp, 6)+
              ns(Temp_3day, 6)+ ns(RH,3)+ ns(RH_3day, 3)+ health_data$Hot.Humid,family=quasipoisson(),health_data)

summary(modx)  # is does not give AIC (use poisson() to get value for AIC)

acf2(residuals(modx))
res <- residuals(modx, type="deviance")
summary(res)
plot(res,ylim=c(-10,10),pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time (glm(sum_patients ~ mean_PM25 + ns(time,12)+ health_data$dow +health_data$holidays + ns(Temp, 6)+
     ns(Temp_3day, 6)+ ns(RH,3)+ ns(RH_3day, 3)+ health_data$Hot.Humid,family=quasipoisson(),health_data)",ylab="Deviance residuals",xlab="date")
abline(h=0,lty=2,lwd=2)




################################################
################################################
#install.packages("Epi")
library(Epi)
library(splines)
library(dlnm)
#install.packages("dplyr")
library(dplyr)

###Annual

# RR as per 10 ug/m3
health_data$mean_PM25 <- health_data$mean_PM25/10

# positive lags
tablag <- matrix(NA, 5+1, 3, dimnames = list(paste("Lag",0:5),
                                        c("RR","ci.low","ci.hi")))

i <- 1

# RUN THE LOOP
for(i in 0:5) {
  
  PM25lag <- Lag(health_data$mean_PM25,i)
  
  # mod <- glm(health_data$mean_PM25 ~ PM25lag + ns(health_data$Date,20)+ 
  #              health_data$d_o_w + health_data$holidays + ns(Temp, 6)+ ns(Temp_3day, 6) +
  #              ns(RH,3)+ ns(RH_3day, 3)+ Hot.Humid, 
  #            family=quasipoisson(),health_data,  na.action = na.exclude)
  

  mod <- glm(health_data$sum_patients ~ PM25lag + ns(health_data$time,12)+health_data$holidays +
               health_data$dow + ns(Temp, 6)+ ns(Temp_3day, 6) +
               ns(RH,3)+ ns(RH_3day, 3), 
             family=quasipoisson(), health_data,  na.action = na.exclude)
  
  
  
  tablag[i+1,] <- ci.lin(mod,subset="PM25lag",Exp=T)[5:7]
}

tablag



plot(0:5,0:5,type="n",ylim=c(0.95,1.05),main="Lag vs. RR (DUBAI - Northern Emirates)", 
     xlab="Lag (days)",ylab="RR and 95%CI per 10ug/m3 pm increase")
abline(h=1)
arrows(0:5,tablag[,2],0:5,tablag[,3],length=0.05,angle=90,code=3)
points(0:5,tablag[,1],pch=19)

### Lead Plot


# negative lags
tablag1 <- matrix(NA,5+1,3,dimnames=list(paste("Lag",0:-5),
                                         c("RR","ci.low","ci.hi")))


# RUN THE LOOP
for(i in 0:5) {
  
  PM25lag <- lead(health_data$mean_PM25,i)
  
  # mod <- glm(health_data$detrend_counts ~ pm10lag + ns(health_data$Date,20)+ 
  #              health_data$d_o_w + health_data$holidays + ns(Temp, 6)+ ns(Temp_3day, 6) +
  #              ns(RH,3)+ ns(RH_3day, 3)+ Hot.Humid, 
  #            family=quasipoisson(), health_data,  na.action = na.exclude)
  


  mod <- glm(health_data$sum_patients ~ PM25lag + ns(health_data$time,12)+ health_data$holidays +
               health_data$dow + ns(Temp, 6)+ ns(Temp_3day, 6) +
               ns(RH,3)+ ns(RH_3day, 3),
             family=quasipoisson(), health_data,  na.action = na.exclude)
  
  
  tablag1[i+1,] <- ci.lin(mod,subset="PM25lag",Exp=T)[5:7]
}


tablag1
tab1 <- tablag1[6:2,]
tab1
tab <- rbind(tab1,tablag)
tab


plot(-5:5,-5:5,type="n",ylim=c(0.95,1.05),main="Lag vs. RR (control - holidays included)", 
     xlab="Lag (days)",ylab="RR and 95%CI per 10ug/m3 pm increase")
abline(h=1)
arrows(-5:5,tab[,2],-5:5,tab[,3],length=0.05,angle=90,code=3)
points(-5:5,tab[,1],pch=19)





######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
