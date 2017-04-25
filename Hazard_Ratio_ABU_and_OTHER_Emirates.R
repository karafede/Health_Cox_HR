
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


# load AQ data and health data from 2011 to 2013 for Abu Dhabi
AQ_HEALTH_ABU_DHABI <- read.csv("D:/R_processing/HEALTH_DATA_PM25_COUNTS_ABU_DHABI.csv")

AQ_HEALTH_ABU_DHABI <- AQ_HEALTH_ABU_DHABI %>%
  select(Date,
         Age,
         AGE_BIN,
         Gender,
         mean_PM25,
         sum_patients)


# normalise the dataset (sum_patients)
# AQ_HEALTH_ABU_DHABI$sum_patients <- (AQ_HEALTH_ABU_DHABI$sum_patient)/max(AQ_HEALTH_ABU_DHABI$sum_patient)


# load AQ data and health data from 2013 to 2015 for Other Emirates
 AQ_HEALTH_other <- read.csv("HEALTH_DATA_PM25_COUNTS_Other_EMIRATES.csv")

AQ_HEALTH_other <- AQ_HEALTH_other %>%
  select(Date,
         Age,
         AGE_BIN,
         Gender,
         mean_PM25,
         sum_patients)

# normalise the dataset (sum_patients)
# AQ_HEALTH_other$sum_patients <- (AQ_HEALTH_other$sum_patient)/max(AQ_HEALTH_other$sum_patient)


# bind the datasets

AQ_HEALTH <- rbind(AQ_HEALTH_ABU_DHABI,
                   AQ_HEALTH_other)


# count patients in each day (all genders all ages)
# AQ_HEALTH <- AQ_HEALTH %>%
#   group_by(Date) %>%
#   summarise(sum_patients = sum(sum_patients, na.rm = TRUE),
#             mean_PM25 = mean(mean_PM25, na.rm = TRUE))

AQ_HEALTH$Date <- as.Date(AQ_HEALTH$Date)
str(AQ_HEALTH)

# quick plot #################

min <- as.Date("2011-06-01") 
max <- as.Date("2015-10-11") 

plot <- ggplot(AQ_HEALTH, aes(Date, sum_patients)) + 
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






### COX sURVIVAL MODEL------TRIAL-----------------------------------------------

library(survival)

SurvObj_patients_PM25 <- with(AQ_HEALTH, Surv(sum_patients))


AAAA <- as.data.frame(SurvObj_patients_PM25)

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


# coxph
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(detrend_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) , data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Age, data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(Age, 4) , data = AQ_HEALTH, x=T, y=T)


km.as.one <- survfit(SurvObj_patients_PM25 ~ 1, data = AQ_HEALTH, conf.type = "log-log")
km.as.one <- survfit(SurvObj_patients_PM25 ~ Gender, data = AQ_HEALTH, conf.type = "log-log")
plot(km.as.one)
# how to get confidence interval
#  http://stackoverflow.com/questions/36941746/how-to-get-95-ci-from-rs-coxph

# rms_fit_PM25 <- survfit(cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_data_PM25, x=T, y=T))

rms_fit_PM25
summary(rms_fit_PM25)

## plot ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 



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
abline(v= 38.14, col="red", lty=3, lwd=3)

# par(oldpar)
# dev.off()


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
  filter(HR >= 1)

# look at the position of RR ~ 1
abline(v= 38.14, col="red", lty=3, lwd=3)




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



