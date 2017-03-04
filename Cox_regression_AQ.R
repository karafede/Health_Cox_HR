
library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(grid)
library(rms)


EAD_O3_2013 <- read_csv("C:/Users/fkaragulian/Dropbox/daily data/Daily_O3/database_EAD_2013_O3_daily.csv")

# select only sites in Abu Dhabi-----------------------------------
EAD_O3_2013 <- EAD_O3_2013 %>%
  filter(Site == c("Khadeja Primary School", "Khalifa High School",
                   "Bain Aljesrain", "Khalifa City A" ,"Baniyas School"))


# load trial health data

health_data <- read_csv("D:/R_processing/Health data/Asthma_Raw_Data.csv")
health_data[sapply(health_data,is.na)] = NA 

# filter data

health_data <- health_data %>%
  mutate(Date = mdy_hm(Date, tz = "UTC"),
         Date = date(Date),
         year = year(Date)) %>%
  filter(year == "2013") %>%
  filter(Gender == c("Male", "Female"))


# categorise gender with 1 == Female, 2 == Male

health_data$sex <- as.data.frame()
for (i in 1:nrow(health_data)){
  if (health_data$Gender[i] == "Female"){
    health_data$sex[i] <- 1
  }
  if (health_data$Gender[i] == "Male"){
    health_data$sex[i] <- 2
  }
}    



# make a column with bin to count patients
health_data$bin <- "1"
health_data$bin  <- as.numeric(health_data$bin)



# count patients in each day 
health_data <- health_data %>%
  group_by(Date,
           Age,
           sex,
           Gender) %>%
  summarise(sum_patients = sum(bin, na.rm = TRUE))



EAD_data_2013 <- read.csv("C:/Users/fkaragulian/Dropbox/database_EAD_2013_daily.csv")

# select only sites in Abu Dhabi-----------------------------------
EAD_data_2013 <- EAD_data_2013 %>%
  filter(Site == c("Hamdan Street", "Khadeja Primary School", "Khalifa High School",
                   "Bain Aljesrain", "Mussafah", "Al Mafraq", "Khalifa City A" ,"Baniyas School"))

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
  summarise(mean_O3 = mean(Mean_8hour, na.rm = TRUE))


# Join health data with AQ data
AQ_data_O3 <- AQ_data_O3 %>%
  left_join(health_data, "Date")



# load PM10 data-----------------------------------------------

AQ_data_PM10 <- EAD_data_2013 %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date),
         Date = date(date)) %>%
  group_by(Date) %>%
  summarise(mean_PM10 = mean(Value, na.rm = TRUE))

# join data again-----

AQ_data_all <- AQ_data_PM10 %>%
  left_join(AQ_data_O3, "Date")

AQ_data_all[sapply(AQ_data_all,is.na)] = NA 

# remove all lines with NA
AQ_data_all <- na.omit(AQ_data_all)





##################################################################################
##################----------------------------------##############################
##################################################################################


### COX sURVIVAL MODEL------TRIAL-----------------------------------------------

library(survival)

## Add survival object. (you can specifica a status...if you have one. i.e: status == 2 is death

# sum Patients
AQ_data_all$SurvObj_patients <- with(AQ_data_all, Surv(sum_patients))

# PM10 expsure
AQ_data_all$SurvObj_PM10 <- with(AQ_data_all, Surv(mean_PM10))

#O3 exposure
AQ_data_all$SurvObj_O3 <- with(AQ_data_all, Surv(mean_O3))

## Kaplan-Meier estimator. "log-log" confidence interval 
km.as.one_patients <- survfit(SurvObj_patients ~ 1, data = AQ_data_all , conf.type = "log-log")

km.as.one_PM10 <- survfit(SurvObj_PM10 ~ 1, data = AQ_data_all , conf.type = "log-log")
km.as.one_O3 <- survfit(SurvObj_O3 ~ 1, data = AQ_data_all , conf.type = "log-log")

# km.as.one <- survfit(SurvObj ~ 1, data = AQ_data_all)
km.as.one_patients
summary(km.as.one_patients)
km.as.one_PM10
summary(km.as.one_PM10)
km.as.one_O3


## Plot
plot(km.as.one_patients, xlab = "sum patients", ylab = "survival probability")
plot(km.as.one_PM10, xlab = "PM10 concentration (ug/m3)", ylab = "survival probability" )
plot(km.as.one_O3, xlab = "O3 concentration (ug/m3)", ylab = "survival probability")


####***********************************************######################
#########################################################################

# Cox Proportional Hazard model Cox-PH (to calcualte the Hazard Ratio HR)
## Fit Cox regression: 
res.cox1 <- coxph(SurvObj_patients ~ mean_PM10, data = AQ_data_all)
res.cox1
summary(res.cox1)


res.cox1 <- coxph(SurvObj_patients ~ mean_PM10 + Gender + Age, data = AQ_data_all)
res.cox1
summary(res.cox1)


res.cox2 <- coxph(SurvObj_patients ~ mean_O3 + Age, data = AQ_data_all)
res.cox2
summary(res.cox2)


### response curve (HR versus pollutant concentration)------------------------------

ddist <- datadist(AQ_data_all)
options(datadist="ddist")

# nice cursor plot with the RR--------------------------------------------
fit_O3 = cph(SurvObj_patients ~ mean_O3, data = AQ_data_all, x=T, y=T)
fit_O3
plot(summary(fit_O3), log=T)


fit_PM10 = cph(SurvObj_patients ~ mean_PM10, data = AQ_data_all, x=T, y=T)
fit_PM10
plot(summary(fit_PM10), log=T)
##------------------------------------------------------------------------


# fit1 = cph(SurvObj_patients ~ mean_PM10 + mean_O3 + sex + Age, data = AQ_data_all, x=T, y=T)
fit1 = cph(SurvObj_patients ~ mean_O3 + Age, data = AQ_data_all, x=T, y=T)
fit1




par(mfrow=c(1,2))
termplot(fit1, term=2, se=TRUE, col.term=1, col.se=1)

# par("mar")
# par(mar=c(1,1,1,1))
par(mfrow=c(1,2))
termplot(fit1, se=T, 
         se.type="polygon", 
         ylab=rep("Hazard Ratio", times=2),
         main=rep("response curve", times=2),
         col.se=rgb(.2,.2,1,.4), col.term="black")


fit2 = cph(SurvObj_patients ~ mean_O3, data = AQ_data_all, x=T, y=T)
termplot(fit2, se=T, 
         se.type="polygon", 
         ylab=rep("Hazard Ratio", times=2),
         main=rep("response curve", times=2),
         col.se=rgb(.2,.2,1,.4), col.term="black")


fit3 = cph(SurvObj_patients ~ mean_PM10, data = AQ_data_all, x=T, y=T)
termplot(fit3, se=T, 
         se.type="polygon", 
         ylab=rep("Hazard Ratio", times=2),
         main=rep("response curve", times=2),
         col.se=rgb(.2,.2,1,.4), col.term="black")

#################################################################################


