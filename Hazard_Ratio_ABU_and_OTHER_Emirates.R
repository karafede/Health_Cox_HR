
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
# AQ_HEALTH_ABU_DHABI <- read.csv("D:/R_processing/NO_Seasonality_HEALTH_DATA_PM25_COUNTS_ABU_DHABI.csv")

AQ_HEALTH_ABU_DHABI <- AQ_HEALTH_ABU_DHABI %>%
  select(Date,
         AGE_BIN,
         Gender,
         mean_PM25,
         sum_patients)


# AQ_HEALTH_ABU_DHABI <- AQ_HEALTH_ABU_DHABI %>%
#   select(Date,
#          detrend_PM25,
#          detrend_counts)


# normalise the dataset (sum_patients)
# AQ_HEALTH_ABU_DHABI$sum_patients <- (AQ_HEALTH_ABU_DHABI$sum_patient)/max(AQ_HEALTH_ABU_DHABI$sum_patient)


# load AQ data and health data from 2013 to 2015 for Other Emirates
 AQ_HEALTH_other <- read.csv("HEALTH_DATA_PM25_COUNTS_Other_EMIRATES.csv")
# AQ_HEALTH_other <- read.csv("NO_Seasonality_HEALTH_DATA_PM25_COUNTS_Other_EMIRATES.csv")

AQ_HEALTH_other <- AQ_HEALTH_other %>%
  select(Date,
         AGE_BIN,
         Gender,
         mean_PM25,
         sum_patients)

# AQ_HEALTH_other <- AQ_HEALTH_other %>%
#   select(Date,
#          detrend_PM25,
#          detrend_counts)

# normalise the dataset (sum_patients)
# AQ_HEALTH_other$sum_patients <- (AQ_HEALTH_other$sum_patient)/max(AQ_HEALTH_other$sum_patient)


# bind the datasets

AQ_HEALTH_TOTAL <- rbind(AQ_HEALTH_ABU_DHABI,
                   AQ_HEALTH_other)


colnames(AQ_HEALTH_TOTAL)[colnames(AQ_HEALTH_TOTAL) == 'detrend_PM25'] <- 'mean_PM25'
colnames(AQ_HEALTH_TOTAL)[colnames(AQ_HEALTH_TOTAL) == 'detrend_counts'] <- 'sum_patients'


# count patients in each day (all genders all ages)
# AQ_HEALTH <- AQ_HEALTH %>%
#   group_by(Date) %>%
#   summarise(sum_patients = sum(sum_patients, na.rm = TRUE),
#             mean_PM25 = mean(mean_PM25, na.rm = TRUE))

AQ_HEALTH_TOTAL$Date <- as.Date(AQ_HEALTH_TOTAL$Date)
str(AQ_HEALTH_TOTAL)


# quick plot #################

min <- as.Date("2011-06-01") 
max <- as.Date("2015-10-11") 



jpeg('D:/R_processing/plots/Health_Data_Abu_Dhabi_&_Other_Emirates.jpg',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_HEALTH_TOTAL, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "sum_patients"), alpha=1, col="blue") +
 # stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression("Sum Patients (counts)")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylim(0, 600) + 
  xlim(min, max) 
plot


par(oldpar)
dev.off()




# average all variables by day

AQ_HEALTH_SUMMARY_STATS <- AQ_HEALTH_TOTAL %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(mean_PM25),
            sum_patients = sum(sum_patients))


jpeg('D:/R_processing/plots/Combined_PM25_distribution_asthma.jpg',   
     quality = 100, bg = "white", res = 200, width = 12, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_PM25 <- ggplot(AQ_HEALTH_SUMMARY_STATS,  aes(mean_PM25)) + 
  theme_bw() +
  #  geom_point(stat="bin", binwidth=5) +
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  ggtitle("PM distribution (Abu Dhabi & Dubai & Northern - patients respiratory, 2011-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  ylab("number of days") + 
  ylim(0, 350) +
  xlab(expression(paste(PM[2.5], " (µg/",m^3, ")", " "))) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=20)) 
p_PM25

par(oldpar)
dev.off()





jpeg('D:/R_processing/plots/Combined_Sum_Patients_distribution_respiratory.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_sum_patients <- ggplot(AQ_HEALTH_SUMMARY_STATS,  aes(sum_patients)) + 
  theme_bw() + 
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  ylim(0, 130) +
  ggtitle("number of patients per day (Abu Dhabi & Dubai & Northern Emirates, 2011-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  xlab("sum patients per day") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=20)) +
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



jpeg('D:/R_processing/plots/Combined_counts_vs_PM25_normalised.jpg',   
     quality = 100, bg = "white", res = 200, width = 13, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p_health <- ggplot(SUM_PATIENTS_BINS, aes(xxx, sum_patients)) + 
  theme_bw() +
  geom_point() +
  geom_smooth() +
  ylim(0, 400) +
  ggtitle("number of patients per day (Abu Dhabi & Dubai & Northern Emirates, 2011-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  xlab("sum patients per day") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=20)) +
  xlab(expression(paste(PM[2.5], " (µg/",m^3, ")", " "))) +
  theme(legend.position="none") + 
  ylab("daily average numer of patients per day") 
p_health


par(oldpar)
dev.off()


### COX sURVIVAL MODEL------TRIAL-----------------------------------------------

library(survival)

SurvObj_patients_PM25 <- with(AQ_HEALTH_TOTAL, Surv(sum_patients))


AAAA <- as.data.frame(SurvObj_patients_PM25)

ddist <- datadist(AQ_HEALTH_TOTAL)
options(datadist="ddist")

# RCS = restricted cubic spline----log

# age
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(Age, 4) , data = AQ_HEALTH, x=T, y=T)

# mean PM25
rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH_TOTAL, x=T, y=T)
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender, data = AQ_HEALTH_TOTAL, x=T, y=T)rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) , data = AQ_HEALTH, x=T, y=T)
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Age, data = AQ_HEALTH_TOTAL, x=T, y=T)
# rms_fit_PM25 <- cph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4), data = AQ_HEALTH_TOTAL, x=T, y=T)


# coxph
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH_TOTAL, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(detrend_PM25, 4) + Gender + AGE_BIN, data = AQ_HEALTH_TOTAL, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) , data = AQ_HEALTH_TOTAL, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(mean_PM25, 4) + Age, data = AQ_HEALTH_TOTAL, x=T, y=T)
# rms_fit_PM25 <- coxph(SurvObj_patients_PM25 ~ rcs(Age, 4) , data = AQ_HEALTH_TOTAL, x=T, y=T)


km.as.one <- survfit(SurvObj_patients_PM25 ~ 1, data = AQ_HEALTH_TOTAL, conf.type = "log-log")
km.as.gender <- survfit(SurvObj_patients_PM25 ~ Gender, data = AQ_HEALTH_TOTAL, conf.type = "log-log")
plot(km.as.one)
## Plot
plot(km.as.gender, xlab = "PM25 concentration (ug/m3)", ylab = "survival probability" )
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


###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
####    Generalised linear model   ############################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################


rms_fit_PM25_glm <- glm(sum_patients ~ rcs(mean_PM25, 3) + Gender + AGE_BIN, family = Gamma(),
                        data = AQ_HEALTH_TOTAL)


termplot2(rms_fit_PM25_glm, se=T, rug.type="density", rug=T, density.proportion=.05,
          se.type="polygon",  yscale="exponential", log="y",
          ylab=rep("Relative Risk", times=3),
          cex.lab=1.5, cex.axis=2.5,  cex.main = 2, ylim = c(-0.3, 0.3), # ylim = c(-0.2, 0.6) , #ylim = c(-0.2, 0.4),# ,   
          cex.lab=1.5, cex.axis=1.5,  cex.main = 2, las = 2, font=2,
          #  xlab = c("conc", "Gender"),
          xlab = c((expression(paste(PM[2.5], " daily concentration (µg/",m^3, ")")))),
          #   main=  ("Health Response Curve for PM2.5 (Generalised Linear Model)"),
          #  main=  ("Hazard Ratio for asthma by gender"),
          main=  ("Relative Risk for asthma by age bins (Generalised Linear Model)"),
          col.se=rgb(.2,.2,1,.4), col.term="black")




abline(h=1, col="red", lty=3, lwd=3)
abline(v= 36.5, col="red", lty=3, lwd=3)


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
colnames(tms) <- c("RR", "Gender", "AGE_BIN")
AAA <- cbind(AQ_HEALTH_TOTAL$mean_PM25, tms)



# look where RR is > 1
# filter only RR > 1
RR_1 <- AAA %>%
  filter(RR >= 1)


