
library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(grid)
library(rms)
library(stats)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data")


wd <- getwd()
EAD_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2013_O3_daily.csv"))
EAD_O3_2013 <- read_csv("C:/Users/fkaragulian/Dropbox/daily data/Daily_O3/database_EAD_2013_O3_daily.csv")

# select only sites in Abu Dhabi-----------------------------------
EAD_O3_2013 <- EAD_O3_2013 %>%
  filter(Site == c("Khadeja Primary School", "Khalifa High School",
                   "Bain Aljesrain", "Khalifa City A" ,"Baniyas School"))




# load trial health data

health_data <- read_csv("D:/R_processing/Health data/Asthma_Raw_Data.csv")
# health_data <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/R_processing/Health data/Asthma_Raw_Data.csv")
health_data[sapply(health_data,is.na)] = NA 


health_data <- health_data %>%
  mutate(Date = mdy_hm(Date, tz = "UTC"),
         Date = date(Date),
         year = year(Date)) %>%
  filter(year == "2013") %>%
filter(Gender == c("Male", "Female"))
#  filter(year == "2013", Gender == "Male" , Age < 30)



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
  group_by(Date) %>%
           # sex,
           # Age) %>%
  summarise(sum_patients = sum(bin, na.rm = TRUE))

sum(health_data$sum_patients)



# plot time series for all the number of patients

patients_time_series <- ggplot(health_data, aes(Date, sum_patients)) + 
  theme_bw() +
  geom_line(aes(y = sum_patients, col = "patients")) +
  geom_smooth() +
  theme(legend.position="none") + 
  ylab(" tot num. patients") + 
  ylim(0, 750) 
patients_time_series



 EAD_data_2013 <- read_csv("database_EAD_2013_daily.csv")
 EAD_data_2013 <- read.csv("C:/Users/fkaragulian/Dropbox/database_EAD_2013_daily.csv")
# EAD_data_2014 <- read_csv("database_EAD_2014_daily.csv")
# EAD_data_2015 <- read_csv("database_EAD_2015_daily.csv")
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

AQ_data_all <- AQ_data_all %>%
  filter(mean_PM10 < 206)

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
  


p_O3 <- ggplot(AQ_data_all, aes(Date, mean_O3)) + 
        theme_bw() +
        geom_line(aes(y = mean_O3, col = "O3 mean")) +
        geom_smooth() +
        theme(legend.position="none") + 
        ylab(expression(paste(O[3], " (µg/",m^3, ")", " 8h-mean"))) + 
         ylim(0, 150)  
p_O3


p_PM10 <- ggplot(AQ_data_all, aes(Date, mean_PM10)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM10, col = "PM10 mean")) +
  geom_smooth() +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 350)  
p_PM10
  

p_health <- ggplot(AQ_data_all, aes(Date, sum_patients)) + 
            theme_bw() +
            geom_line(aes(y = sum_patients, col = "patients")) +
            geom_smooth() +
            theme(legend.position="none") + 
            ylab(" tot num. patients") + 
            ylim(0, 750) 
p_health

  
# combine plots
grid.newpage()
grid.draw(rbind(ggplotGrob(p_O3), ggplotGrob(p_PM10), ggplotGrob(p_health), size = "last"))



############################################################################
# display counts of days and number of patients-----------------------------

# PM10--------------------------------------------------------------------

p_PM10 <- ggplot(AQ_data_all,  aes(mean_PM10)) + 
  geom_point(stat="bin", binwidth=5) +
  scale_x_continuous("PM10 ug/m3") +
#  scale_y_continuous("number of days") +
  ylab("number of days") + 
  ylim(0, 15) +
  theme_bw() 
p_PM10


p_health <- ggplot(AQ_data_all, aes(mean_PM10, sum_patients)) + 
  theme_bw() +
  geom_point() +
  geom_smooth() +
  scale_x_continuous("PM10 ug/m3") +
  theme(legend.position="none") + 
  ylab("tot num. patients") + 
  ylim(0, 750) 
p_health



# combine plots
grid.newpage()
grid.draw(rbind(ggplotGrob(p_PM10), ggplotGrob(p_health), size = "last"))


# O3-------------------

p_O3 <- ggplot(AQ_data_all,  aes(mean_O3)) + 
  geom_point(stat="bin", binwidth=5) +
  scale_x_continuous("O3 ug/m3") +
  #  scale_y_continuous("number of days") +
  ylab("number of days") + 
  ylim(0, 20) +
  theme_bw() 
p_O3


p_health <- ggplot(AQ_data_all, aes(mean_O3, sum_patients)) + 
  theme_bw() +
  geom_point() +
  geom_smooth() +
  scale_x_continuous("O3 ug/m3") +
  theme(legend.position="none") + 
  ylab("tot num. patients") + 
  ylim(0, 750) 
p_health



# combine plots
grid.newpage()
grid.draw(rbind(ggplotGrob(p_O3), ggplotGrob(p_health), size = "last"))


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


km.as.one_patients
summary(km.as.one_patients)
km.as.one_PM10
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

res.cox2 <- coxph(SurvObj_patients ~ mean_O3, data = AQ_data_all)
res.cox2
summary(res.cox2)


### response curve (HR versus pollutant concentration)------------------------------

ddist <- datadist(AQ_data_all)
options(datadist="ddist")
fit1 = cph(SurvObj_patients ~ mean_PM10, data = AQ_data_all, x=T, y=T)
fit1

# termplot(fit1, term=2, se=TRUE, col.term=1, col.se=1)


# plot(summary(fit1), log=T)

setwd("D:/R_processing")

jpeg('PM10_HR.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

termplot(fit1, se=T, density.proportion = 0.05,rug=T,
          se.type="polygon",
          ylab=rep("Hazard Rate"),
          xlab = expression(paste(PM[10], " (µg/",m^3, ")")), 
          main=rep("response curve"),
          col.se=rgb(.2,.2,1,.4), col.term="black")
abline(h=0, col="red", lty=3)


par(oldpar)
dev.off()

survplot(fit1, loglog=F, logt=F, xlab= "sum patients")
#################################################################################


ddist <- datadist(AQ_data_all)
options(datadist="ddist")
fit2 = cph(SurvObj_patients ~ mean_O3, data = AQ_data_all, x=T, y=T)
fit2

# termplot(fit2, term=2, se=TRUE, col.term=1, col.se=1)
# plot(summary(fit2, mean_O3=c(127,10)))

termplot(fit2, se=T, density.proportion = 0.05,rug=T,
         se.type="polygon", 
         ylab=rep("Hazard Rate"),
         xlab = expression(paste(O[3], " (µg/",m^3, ")")),
         main=rep("response curve"),
         col.se=rgb(.2,.2,1,.4), col.term="black")

survplot(fit2, loglog=F, logt=F, xlab= "sum patients")






##################################################################################
# density and histograms plot with days counts counts -----------------------------

setwd("D:/R_processing")

jpeg('O3_hist.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


AQ_data_all %>%  
  ggplot(aes(mean_O3)) + 
  geom_histogram(aes(fill=..count..),binwidth=5, stat = "bin") +
  theme_bw() +
  xlab(expression(paste(O[3], " (µg/",m^3, ")"))) +
  ylab("number of days") +
  guides(fill=FALSE) +  
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
  xlim(0, 120) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
  ggtitle(expression(paste("Distribution of average daily"," ", O[3], " conc. (2013 - Abu Dhabi)"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))


par(oldpar)
dev.off()

#########################################################


jpeg('PM10_hist.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


AQ_data_all %>%  
  ggplot(aes(mean_PM10)) + 
  geom_histogram(aes(fill=..count..),binwidth=5, stat = "bin") +
  theme_bw() +
  guides(fill=FALSE) +  
  xlab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  ylab("number of days") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
  xlim(0, 300) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
  ggtitle(expression(paste("Distribution of average daily"," ", PM[10], " conc.(2013-Abu Dhabi)"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))


  par(oldpar)
  dev.off()


#########################################################

AQ_data_all %>%  
  ggplot(aes(mean_O3, ..count..)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  theme_bw()


AQ_data_all %>%  
  ggplot(aes(mean_O3)) + 
  geom_line(aes(fill=..count..), stat="bin", binwidth=5) +
  theme_bw() 


AQ_data_all %>%  
  ggplot(aes(mean_O3)) + 
  geom_point(stat="bin", binwidth=5) +
  scale_x_continuous("O3 ug/m3") +
  scale_y_continuous("number of days") +
  theme_bw() 



# Draw with black outline, white fill

AQ_data_all %>%  
ggplot(aes(mean_O3)) +
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  theme_bw() 

AQ_data_all %>%  
  ggplot(aes(mean_PM10)) +
  geom_histogram(binwidth = 5, colour="black", fill="white") +
  theme_bw() 









# Histogram overlaid with kernel density curve

AQ_data_all %>% 
ggplot(aes(mean_O3)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth= 5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  theme_bw() 


AQ_data_all %>% 
ggplot(aes(mean_O3)) +
  geom_histogram(binwidth= 5, colour="black", fill="white") +
  geom_vline(aes(xintercept = mean(mean, na.rm=TRUE)),   
             color="red", linetype="dashed", size=1) +
  theme_bw() 



##############################################################################
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


# With mean lines, using cdat from above
NCMS_day_CO %>%
ggplot(aes(value)) +
  geom_histogram(binwidth = .2, colour="black", fill="white") + 
  facet_grid(site ~ .) +
  geom_vline(data= mean_value, aes(xintercept = mean),
             linetype="dashed", size=1, colour="red") +
  theme_bw()



