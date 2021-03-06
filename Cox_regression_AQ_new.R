
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


# age_bin <- NULL
# 
# # health_data <- health_data[1:1000,]
# 
# for (i in 1:nrow(health_data)) {
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] == 0)
#   # AGE_BIN = as.character("00")
#     AGE_BIN = 1
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] < 4 & health_data$Age[i] > 1)
#   # AGE_BIN = as.character("01-04 years")
#     AGE_BIN = 2
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] < 9 & health_data$Age[i] > 5)
#   #  AGE_BIN = as.character("05-09 years")
#     AGE_BIN = 3
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 14 & health_data$Age[i] >= 10)
#   #  AGE_BIN = as.character("10-14 years")
#   AGE_BIN = 4
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 19 & health_data$Age[i] >= 15)
#     #  AGE_BIN = as.character("15-19 years")
#     AGE_BIN = 5
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 24 & health_data$Age[i] >= 20)
#     #  AGE_BIN = as.character("20-24 years")
#     AGE_BIN = 6
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 29 & health_data$Age[i] >= 25)
#     #  AGE_BIN = as.character("25-29 years")
#     AGE_BIN = 7
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 34 & health_data$Age[i] >= 30)
#     #  AGE_BIN = as.character("30-34 years")
#     AGE_BIN = 8
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 39 & health_data$Age[i] >= 35)
#     #  AGE_BIN = as.character("39-35 years")
#     AGE_BIN = 9
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 44 & health_data$Age[i] >= 40)
#     #  AGE_BIN = as.character("40-44 years")
#     AGE_BIN = 10
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 49 & health_data$Age[i] >= 45)
#     #  AGE_BIN = as.character("45-49 years")
#     AGE_BIN = 11
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 54 & health_data$Age[i] >= 50)
#     #  AGE_BIN = as.character("50-54 years")
#     AGE_BIN = 12
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 59 & health_data$Age[i] >= 55)
#     #  AGE_BIN = as.character("55-59 years")
#     AGE_BIN = 13
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 64 & health_data$Age[i] >= 60)
#     #  AGE_BIN = as.character("60-64 years")
#     AGE_BIN = 14
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 69 & health_data$Age[i] >= 65)
#     #  AGE_BIN = as.character("65-69 years")
#     AGE_BIN = 15
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 74 & health_data$Age[i] >= 70)
#     #  AGE_BIN = as.character("70-74 years")
#     AGE_BIN = 16
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 79 & health_data$Age[i] >= 75)
#     #  AGE_BIN = as.character("75-79 years")
#     AGE_BIN = 17
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] <= 84 & health_data$Age[i] >= 80)
#     #  AGE_BIN = as.character("80-84 years")
#     AGE_BIN = 18
# 
#   if (!is.na(health_data$Age[i]) & health_data$Age[i] >= 85)
#     #  AGE_BIN = as.character(">85 years")
#     AGE_BIN = 19
# 
# 
#   print(i)
#   age_bin <- rbind(AGE_BIN, age_bin)
#   age_bin <- as.vector(age_bin)
#   age_bin <- as.data.frame(age_bin)
# 
#   }
# 
#   age_bin <- age_bin %>%
#     arrange(-row_number())

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





# load health data with age bins
# health_data <- read_csv("health_data_age_bins.csv")
# str(health_data)
# 
# health_data$AGE_BIN <- as.factor(health_data$AGE_BIN)
# health_data$Gender <- as.factor(health_data$Gender)
# 
# Health_gender <- health_data %>%
#            group_by(Gender) %>%
#   summarise(somma = sum(bin, na.rm = TRUE))
# 
# 
# 
# health_data <- read_csv("health_data_age_bins.csv")
# str(health_data)
# 
# health_data$AGE_BIN <- as.factor(health_data$AGE_BIN)
# health_data$Gender <- as.factor(health_data$Gender)
# 
# Health_age <- health_data %>%
#   group_by(AGE_BIN) %>%
#   summarise(somma = sum(bin, na.rm = TRUE))


# Ozone data----------------------------------------------------------------
EAD_O3_2013 <- read_csv("C:/Users/fkaragulian/Dropbox/daily data/Daily_O3/database_EAD_2013_O3_daily.csv")

# select only sites in Abu Dhabi-----------------------------------
# EAD_O3_2013 <- EAD_O3_2013 %>%
#   filter(Site == c("Khadeja Primary School", "Khalifa High School",
#                    "Bain Aljesrain", "Khalifa City A" ,"Baniyas School"))

AQ_data_O3 <- EAD_O3_2013 

# replace NaN (not a Number with NA that is a missing value)
AQ_data_O3[sapply(AQ_data_O3,is.na)] = NA 

AQ_data_O3$Mean_8hour <- as.numeric(AQ_data_O3$Mean_8hour)
AQ_data_O3$MAX_8hour <- as.numeric(AQ_data_O3$MAX_8hour)


AQ_data_O3 <- AQ_data_O3 %>%
  mutate(year = year(Date)) %>%
  dplyr:: select(Date,
                 Site,
                 Mean_8hour,
                 year) 



AQ_data_O3 <- AQ_data_O3 %>%
  group_by(Date) %>%
  summarise(mean_O3 = mean(Mean_8hour, na.rm = TRUE))


# Join health data with AQ data
AQ_data_O3 <- AQ_data_O3 %>%
  left_join(health_data, "Date")
AQ_data_O3[sapply(AQ_data_O3,is.na)] = NA 

# remove all lines with NA
AQ_data_O3 <- na.omit(AQ_data_O3)


# load PM10 data-----------------------------------------------

EAD_data_2013 <- read.csv("C:/Users/fkaragulian/Dropbox/database_EAD_2013_daily.csv")

# select only sites in Abu Dhabi-----------------------------------
# EAD_data_2013 <- EAD_data_2013 %>%
#   filter(Site == c("Hamdan Street", "Khadeja Primary School", "Khalifa High School",
#                    "Bain Aljesrain", "Mussafah", "Al Mafraq", "Khalifa City A" ,"Baniyas School"))


AQ_data_PM10 <- EAD_data_2013 %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date),
         Date = date(date)) %>%
  filter(Pollutant == "PM10") %>%
  group_by(Date) %>%
  summarise(mean_PM10 = mean(Value, na.rm = TRUE))

# join PM10 data and health data-----

AQ_data_PM10 <- AQ_data_PM10 %>%
  left_join(health_data, "Date")
AQ_data_PM10[sapply(AQ_data_PM10,is.na)] = NA 


# remove all lines with NA
AQ_data_PM10 <- na.omit(AQ_data_PM10)



###############################################################################
## load satellite data for PM2.5 from MODIS------------------------------------

# PM25_AOD <- read_csv("PM25_from_AOD_MODIS.csv")
PM25_AOD <- read_csv("PM10_PM25_2011_2016_MODIS.csv")

# get only data over Abu Dhabi -----------------------------------------------

# load station info over Abu Dhabi ----------------------------
EAD_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info.csv")
PM25_AOD$Pollutant <- "PM2.5"

# attach infos to satellite PM2.5 data
PM25_AOD <- EAD_info %>%
  left_join(PM25_AOD, c("Site", "Pollutant", "Latitude", "Longitude"))


# PM25_AOD_2013 <- PM25_AOD %>%
#   filter(Date < "2014-01-01" & Date > "2013-01-01") %>%
# group_by(Date) %>%
#   summarise(mean_PM25 = mean(AOD_PM25, na.rm = TRUE))

PM25_AOD <- PM25_AOD %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(AOD_PM25, na.rm = TRUE))

# join PM2.5 data and health data-----

# AQ_data_PM25 <- PM25_AOD_2013 %>%
#   left_join(health_data, "Date")
# AQ_data_PM25[sapply(AQ_data_PM25,is.na)] = NA 

AQ_data_PM25 <- PM25_AOD %>%
  left_join(health_data, "Date")
AQ_data_PM25[sapply(AQ_data_PM25,is.na)] = NA 

# remove all lines with NA
AQ_data_PM25 <- na.omit(AQ_data_PM25)
str(AQ_data_PM25)

AQ_data_PM25_sum_admissions <- AQ_data_PM25 %>%
 group_by(Gender) %>%
   summarise(sum_admissions = sum(sum_patients, na.rm = TRUE))



## load datellite data for PM10 from MODIS------------------------------------

PM10_AOD <- read_csv("PM25_from_AOD_MODIS.csv")

PM10_AOD_2013 <- PM10_AOD %>%
  filter(Date < "2014-01-01" & Date > "2013-01-01") %>%
  group_by(Date) %>%
  summarise(mean_PM10 = mean(AOD_PM10, na.rm = TRUE))

# join PM10 data and health data-----

AQ_data_PM10_sat <- PM10_AOD_2013 %>%
  left_join(health_data, "Date")
AQ_data_PM10_sat[sapply(AQ_data_PM10_sat,is.na)] = NA 

# remove all lines with NA
AQ_data_PM10_sat <- na.omit(AQ_data_PM10_sat)

##################################################################################
##################----------------------------------##############################
##################################################################################


### COX sURVIVAL MODEL------TRIAL-----------------------------------------------

library(survival)

# ## Add survival object. (you can specifica a status...if you have one. i.e: status == 2 is death
# 
# # sum Patients
# AQ_data_all$SurvObj_patients <- with(AQ_data_all, Surv(sum_patients))
# 
# # PM10 expsure
# AQ_data_all$SurvObj_PM10 <- with(AQ_data_all, Surv(mean_PM10))
# 
# #O3 exposure
# AQ_data_all$SurvObj_O3 <- with(AQ_data_all, Surv(mean_O3))
# 
# ## Kaplan-Meier estimator. "log-log" confidence interval
# km.as.one_patients <- survfit(SurvObj_patients ~ 1, data = AQ_data_all , conf.type = "log-log")
# 
# km.as.one_PM10 <- survfit(SurvObj_PM10 ~ 1, data = AQ_data_all , conf.type = "log-log")
# km.as.one_O3 <- survfit(SurvObj_O3 ~ 1, data = AQ_data_all , conf.type = "log-log")
# 
# # km.as.one <- survfit(SurvObj ~ 1, data = AQ_data_all)
# km.as.one_patients
# summary(km.as.one_patients)
# km.as.one_PM10
# summary(km.as.one_PM10)
# km.as.one_O3
# 
# 
# ## Plot
# plot(km.as.one_patients, xlab = "sum patients", ylab = "survival probability")
# plot(km.as.one_PM10, xlab = "PM10 concentration (ug/m3)", ylab = "survival probability" )
# plot(km.as.one_O3, xlab = "O3 concentration (ug/m3)", ylab = "survival probability")


####***********************************************######################
#########################################################################

# Cox Proportional Hazard model Cox-PH (to calcualte the Hazard Ratio HR)
## Fit Cox regression: 
### response curve (HR versus pollutant concentration)------------------------------

# PM10------------------------------------------------------------------------------

# create a survival object
SurvObj_patients_PM10 <- with(AQ_data_PM10, Surv(sum_patients))

ddist <- datadist(AQ_data_PM10)
options(datadist="ddist")

# RCS = restricted cubic spline----
rms_fit_PM10 <- cph(SurvObj_patients_PM10 ~ rcs(mean_PM10, 4), data = AQ_data_PM10, x=T, y=T)
rms_fit_PM10
summary(rms_fit_PM10)

termplot2(rms_fit_PM10, se=T, rug.type="density", rug=T, density.proportion=.05,
         se.type="polygon",  yscale="exponential", log="y",
         ylab=rep("Hazard Ratio", times=2),
         main=rep("response curve", times=2),
         col.se=rgb(.2,.2,1,.4), col.term="black")
abline(h=1, col="red", lty=3)

# locator()


abline(v=112.4, col="red", lty=3)

fit_PM10 <- coxph(SurvObj_patients_PM10 ~ rcs(mean_PM10, 4) + Gender, data = AQ_data_PM10, x=T, y=T)
fit_PM10
summary(fit_PM10)

termplot2(fit_PM10, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          xlab = c("conc", "Gender"),
          ylab=rep("Hazard Ratio", times=2),
          main=rep("response curve", times=2),
          col.se=rgb(.2,.2,1,.4), col.term="black")
abline(h=1, col="red", lty=3)



# PM10 from satellite ---------------------------------------------------------

# create a survival object
SurvObj_patients_PM10_sat <- with(AQ_data_PM10_sat, Surv(sum_patients))

ddist <- datadist(AQ_data_PM10_sat)
options(datadist="ddist")

# RCS = restricted cubic spline----
rms_fit_PM10_sat <- cph(SurvObj_patients_PM10_sat ~ rcs(mean_PM10, 4), data = AQ_data_PM10_sat, x=T, y=T)
rms_fit_PM10_sat
summary(rms_fit_PM10_sat)

termplot2(rms_fit_PM10_sat, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep("Hazard Ratio", times=2),
          main=rep("response curve", times=2),
          col.se=rgb(.2,.2,1,.4), col.term="black")
abline(h=1, col="red", lty=3)




# PM2.5------------------------------------------------------------------------------

# create a survival object
SurvObj_patients_PM25 <- with(AQ_data_PM25, Surv(sum_patients))

ddist <- datadist(AQ_data_PM25)
options(datadist="ddist")

# RCS = restricted cubic spline----log
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_data_PM25, x=T, y=T)
 rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, data = AQ_data_PM25, x=T, y=T)

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
         # cex.lab=1.5, cex.axis=2.5,  cex.main = 2,# ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4),# ylim = c(-0.5, 0.8),   
          cex.lab=1.5, cex.axis=1.5,  cex.main = 2, las = 2, font=2,
      #  xlab = c("conc", "Gender"),
          xlab = c((expression(paste(PM[2.5], " daily concentration (�g/",m^3, ")")))),
       #   main=  ("Health Response Curve for PM2.5"),
        #  main=  ("Hazard Ratio for asthma by gender"),
       main=  ("Hazard Ratio for asthma by age bins"),
          col.se=rgb(.2,.2,1,.4), col.term="black")


abline(h=1, col="red", lty=3, lwd=3)
abline(v= 35.8, col="red", lty=3, lwd=3)

# par(oldpar)
# dev.off()



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
AAA <- cbind(AQ_data_PM25$mean_PM25, tms)
# look where HR is > 1



# filter only HR > 1
HR_1 <- AAA %>%
  filter(HR > 1)

# look at the position of RR ~ 1
abline(v= 35.85929, col="red", lty=3, lwd=3)




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






# Ozone------------------------------------------------------------------------

SurvObj_patients_O3 <- with(AQ_data_O3, Surv(sum_patients))

ddist <- datadist(AQ_data_O3)
options(datadist="ddist")

fit_O3 <- cph(SurvObj_patients_O3 ~ rcs(mean_O3, 4), data = AQ_data_O3, x=T, y=T)
fit_O3
summary(fit_O3)

termplot2(fit_O3, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep("Hazard Ratio", times=2),
          main=rep("response curve", times=2),
          col.se=rgb(.2,.2,1,.4), col.term="black")
abline(h=1, col="red", lty=3)








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
