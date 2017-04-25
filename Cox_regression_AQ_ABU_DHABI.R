
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
           Age,
           AGE_BIN) %>%
  summarise(sum_patients = sum(bin, na.rm = TRUE))


# quick plot ######################################

min <- as.Date("2011-06-01") 
max <- as.Date("2013-05-31") 

plot <- ggplot(health_data, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="blue") +
  # stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression("Sum Patients (counts)")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylim(0, 200) + 
  xlim(min, max) 
plot


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

plot <- ggplot(PM25_AOD, aes(Date, mean_PM25)) + 
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
  ylim(0, 420)  
plot



# remove outliers #######################################

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
  filter(year <= 2013 & year >= 2011) %>%
  select(- mean_PM25)
names(PM25_AOD)[names(PM25_AOD) == 'PM25_sat_no_outliers'] <- 'mean_PM25'


############################################################################
############################################################################
# join PM2.5 data and health data-----


AQ_HEALTH <- PM25_AOD %>%
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



##################################################################################
##################----------------------------------##############################
##################################################################################

### COX sURVIVAL MODEL------TRIAL-----------------------------------------------

library(survival)

# PM2.5------------------------------------------------------------------------------

# create a survival object
SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(sum_patients))

# AQ_HEALTH$Date <- as.numeric(AQ_HEALTH$Date)
# SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(Date, sum_patients))

ddist <- datadist(AQ_HEALTH)
options(datadist="ddist")

# RCS = restricted cubic spline----log
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_HEALTH, x=T, y=T)
 rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH, x=T, y=T)

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
          xlab = c((expression(paste(PM[2.5], " daily concentration (µg/",m^3, ")")))),
       #   main=  ("Health Response Curve for PM2.5"),
        #  main=  ("Hazard Ratio for asthma by gender"),
       main=  ("Hazard Ratio for asthma by age bins"),
          col.se=rgb(.2,.2,1,.4), col.term="black")


abline(h=1, col="red", lty=3, lwd=3)
abline(v= 34.67, col="red", lty=3, lwd=3)

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
