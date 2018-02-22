

##################################################################################
#####  look at long-trend data ###################################################

# use satellite PM25 data (2011 to 2013)
# use measurements PM25 data 2014 to 2016 - all UAE

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# satellite data
setwd("D:/R_processing")
PM25_sat <- read_csv("PM10_PM25_2011_2016_MODIS.csv")

# get only data over Abu Dhabi -----------------------------------------------

# load station info over Abu Dhabi ----------------------------
EAD_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info.csv")
DM_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_DM_info.csv")
NCMS_info <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_NCMS_info.csv")

stations_info <- rbind(EAD_info, DM_info, NCMS_info)

PM25_sat$Pollutant <- "PM2.5"

# attach infos to satellite PM2.5 data
PM25_sat <- stations_info %>%
  left_join(PM25_sat, c("Site", "Pollutant", "Latitude", "Longitude"))

# remove all lines with NA
PM25_sat <- na.omit(PM25_sat)

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
PM25_sat_no_outliers <- remove_outliers(PM25_sat$AOD_PM25)

# new table with additional smooth column without outliers-----
PM25_sat_no_outliers <- cbind(PM25_sat, PM25_sat_no_outliers)

# select only data from 2011 to 2013
PM25_sat_no_outliers <- PM25_sat_no_outliers %>%
  select(Date,
         Site,
         PM25_sat_no_outliers)

PM25_sat_no_outliers <- PM25_sat_no_outliers %>%
  mutate(year = year(Date)) %>%
  filter(year <= 2013 & year >= 2011)
names(PM25_sat_no_outliers)[names(PM25_sat_no_outliers) == 'PM25_sat_no_outliers'] <- 'Daily_mean'


############################################################
#### load monitoring data from 2014 to 2016

 dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"
 # dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"


EAD_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2013 _daily_filtered.csv"))
EAD_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2014 _daily_filtered.csv"))
EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2015 _daily_filtered.csv"))
EAD_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2016 _daily_filtered.csv"))

DM_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2013 _daily_filtered.csv"))
DM_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2014 _daily_filtered.csv"))
DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2015 _daily_filtered.csv"))
DM_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2016 _daily_filtered.csv"))

NCMS_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2013 _daily_filtered.csv"))
NCMS_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2014 _daily_filtered.csv"))
NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2015 _daily_filtered.csv"))
NCMS_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2016 _daily_filtered.csv"))


# use new processed AQ data
dir_AQ <- "D:/AQ_data/hourly_FK_new/hourly_data/test_FK/Daily_mean"
dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/Daily_mean"

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



# bind data together
UAE_AQ <- rbind(EAD_AQ_2013, EAD_AQ_2014, EAD_AQ_2015, EAD_AQ_2016,
                DM_AQ_2013, DM_AQ_2014, DM_AQ_2015, DM_AQ_2016,
                NCMS_AQ_2013, NCMS_AQ_2014, NCMS_AQ_2015, NCMS_AQ_2016)

UAE_AQ$Site <- as.character(UAE_AQ$Site)
UAE_AQ$Site_Type <- as.character(UAE_AQ$Site_Type)
UAE_AQ$Pollutant <- as.character(UAE_AQ$Pollutant)
str(UAE_AQ)


# filter only PM2.5 data after 2013------------------------------------------------

PM25_measures <- UAE_AQ %>%
  mutate(year = year(Date)) %>%
  filter(Pollutant == "PM2.5") %>%
  filter(year > 2013)


PM25_measures <- PM25_measures %>%
  select(Date,
         Site,
         Daily_mean,
         year)

# bind satellite and measurements data together -----------------------------

PM25_all <- rbind(PM25_sat_no_outliers,
                  PM25_measures)

# get daily means (6 years of data) ######################################
PM25_all_mean <- PM25_all %>%
  group_by(Date) %>%
  summarise(mean_PM25 = mean(Daily_mean, na.rm = TRUE))



jpeg('D:/R_processing/plots/PM25_long_trend_with_outliers.jpg',
     quality = 100, bg = "white", res = 300, width = 14, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


PM25_TREND_6_YEARS <- ggplot(PM25_all_mean, aes(Date, mean_PM25)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25, col = "mean_PM25"), alpha=1, col="blue") +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (5g/",m^3, ")", " 24h-mean"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylab(expression(paste(PM[25], " (5g/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 420)  
PM25_TREND_6_YEARS


par(oldpar)
dev.off()




# remove outliers (4 times IQR)
PM25_all_mean$mean_PM25_no_outliers <- remove_outliers(PM25_all_mean$mean_PM25)

PM25_TREND_6_YEARS_no_outliers <- ggplot(PM25_all_mean, aes(Date, mean_PM25_no_outliers)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25_no_outliers, col = "mean_PM25_no_outliers")) +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  ylab(expression(paste(PM[25], " (5g/",m^3, ")", " 24h-mean"))) + 
  ylim(0, 150)  
PM25_TREND_6_YEARS_no_outliers


# get data from smooth fit curve --------------------------------------------

AAA <- PM25_all_mean
AAA$Date <- as.numeric(AAA$Date)  # need Date to be numeric

smooth_PM25_no_outliers <- predict(loess(mean_PM25_no_outliers ~ Date ,AAA),
                                   AAA$Date)


# smooth_PM25_no_outliers <- predict(lm(mean_PM25_no_outliers ~ Date ,AAA))


# remove all lines with NA
smooth_PM25_no_outliers <- na.omit(smooth_PM25_no_outliers)
XXX <- as.data.frame(smooth_PM25_no_outliers)
MIN <- min(smooth_PM25_no_outliers)


# bind smooth data to main data
PM25_all_mean <- cbind(PM25_all_mean,
                       smooth_PM25_no_outliers)

PM25_all_mean <- PM25_all_mean %>%
  mutate(year = year(Date))

# calculate means by years from the smooth curve------------------------
PM25_all_mean_YEARS <- PM25_all_mean %>%
  group_by(year) %>%
  summarise(mean_years = mean(smooth_PM25_no_outliers, na.rm = TRUE))

# calculate the ANNUAl MEAN over 6 years ------------------------------

str(PM25_all_mean_YEARS)
MEAN_PM25 <- mean(PM25_all_mean_YEARS$mean_years)

str(PM25_all_mean)

min <- as.Date("2011-06-01") 
max <- as.Date("2016-12-31") 


###########################
## Plot ###################
###########################


jpeg('D:/R_processing/plots/PM25_long_trend.jpg',
     quality = 100, bg = "white", res = 300, width = 14, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


PM25_TREND_6_YEARS_no_outliers <- ggplot(PM25_all_mean, aes(Date, mean_PM25_no_outliers)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25_no_outliers, col = "mean_PM25_no_outliers"), alpha=0.6, col = "black") +
#   stat_smooth(method = "loess", size = 1.5, formula = y ~ x, size = 1) +
  geom_smooth(method="lm", formula = y ~ poly(x, 22), size = 1, fill = "blue", col = "black") +  
  geom_smooth(method="lm", size = 1, color = "black", lty=2, lwd=2, fill = "red") + 
  
  theme(legend.position="none") + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=32, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=32),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=32, colour = "black")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " 24h-mean"))) + 
  xlim(min, max) +
  ylim(0, 150) +
  geom_hline(aes(yintercept=25), colour="black", linetype="dashed") +
  geom_hline(aes(yintercept=35), colour="black", size = 1) # linetype="dotted" 
#  geom_hline(yintercept=MEAN_PM25+1, col="blue", size = 1) +
# geom_hline(yintercept=40, col="blue", size = 1) 
# geom_hline(yintercept=MIN+0.3, col="black", size = 0.5, linetype="dashed") 
PM25_TREND_6_YEARS_no_outliers


par(oldpar)
dev.off()

######################################################################
# apply ARIMA model to determine predictions for the uear 2017 #######
######################################################################

# https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/

# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
  

library(ggplot2)
library(forecast)
library(tseries)


# make a time-series

PM25_ts = ts(PM25_all_mean[, c('mean_PM25_no_outliers')])

# remove outliers
PM25_all_mean$PM25_clean = tsclean(PM25_ts)
str(PM25_all_mean)


ggplot() +
  geom_line(data = PM25_all_mean, aes(x = Date, y = PM25_clean)) + ylab('PM25_daily_mean')

# get seasonality, trend and cycle

# make movig averages (ma)
# monthly moving average
PM25_all_mean$ma_PM25 = ma(PM25_all_mean$PM25_clean, order=30) # every 30 days moving average using the clean count with no outliers 
str(PM25_all_mean)


ggplot() +
  theme_bw() +
  geom_line(data = PM25_all_mean, aes(x = Date, y = PM25_clean, colour = "PM25_daily_mean")) +
  geom_line(data = PM25_all_mean, aes(x = Date, y = ma_PM25,   colour = "Monthly Moving Average"), size=1)  +
  ylab('PM25_daily_mean')


PM25_MOVING_AVG = ts(na.omit(PM25_all_mean$ma_PM25), frequency=30)
# make a deseasonalized time-series
decomp = stl(PM25_MOVING_AVG, s.window="periodic")
# remove seasonality
deseasonal_cnt <- seasadj(decomp)
plot(decomp)


# test stationarity of the time-series
adf.test(PM25_MOVING_AVG, alternative = "stationary")


# IF the p-value is too high....the time-serie of count_ma is NOT STATIONARY

##################################################################
# Step 5: Autocorrelations and Choosing Model Order ##############
# correlation between a series and its lags
Acf(PM25_MOVING_AVG, main='')
# or 
Pacf(PM25_MOVING_AVG, main='')

# R plots 95% significance boundaries as blue dotted lines. 
# There are significant autocorrelations with many lags in our bike series, as shown by the ACF plot below. 
# therefore statistical properties are not constant over time.


# Dickey-Fuller test

# Usually, non-stationary series can be corrected by a simple transformation such as differencing. 
# Differencing the series can help in removing its trend or cycles. The idea behind differencing is that, 
# if the original data series does not have constant properties over time, then the change from one period to another might. 
# The difference is calculated by subtracting one period's values from the previous period's values:

# Plotting the differenced series, we see an oscillating pattern around 0 with no visible strong trend. 
# This suggests that differencing of order 1 terms is sufficient and should be included in the model.

# use the deseasonalized time-series
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")


# p-value is MUCH better now....the time-serie of count_ma is NOT STATIONARY

# repeat the tests to determine whether a time-series is stationary

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#################
# ARIMA model ###
#################

# ARIMA(p, d, q)
# p = number of lags (AR)
# d = degree of differencing (I)
# q = determines the number of terms to include in the model (MA)

# use the deseasonalized time-series
auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=100, main='(1,1,1) Model Residuals')

# there is a clear pattern at lag or p == 7

fit2 = arima(deseasonal_cnt, order=c(1,1,7))

fit2

tsdisplay(residuals(fit2), lag.max=100, main='Seasonal Model Residuals')


# FORECAST
# now let's FORECAST with the output of the ARIMA model (fit2)

(fit <- arima(log(deseasonal_cnt), c(1, 1, 0),seasonal = list(order = c(1, 1, 0), period = 12)))  # 1 year


pred <- predict(fit, n.ahead = 10*1)
ts.plot(deseasonal_cnt,2.718^pred$pred, log = "y", lty = c(1,3))


fit <- auto.arima(deseasonal_cnt)
fcast <- forecast(fit, h = 50)
plot(fcast)

fit <- auto.arima(PM25_ts)
fcast <- forecast(fit, h = 100)
plot(fcast)

fit <- arima(deseasonal_cnt, order = c(1,1,30))
fcast <- forecast(fit, h = 50)
plot(fcast)


fit <- arima(PM25_ts, order = c(1,1,2))
fcast <- forecast(fit, h = 50)
plot(fcast)




##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
############ OLD plot ####################################################################
##########################################################################################
##########################################################################################


jpeg('D:/R_processing/plots/PM25_long_trend.jpg',
     quality = 100, bg = "white", res = 300, width = 14, height = 9, units = "in")


par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


PM25_TREND_6_YEARS_no_outliers <- ggplot(PM25_all_mean, aes(Date, mean_PM25_no_outliers)) + 
  theme_bw() +
  geom_line(aes(y = mean_PM25_no_outliers, col = "mean_PM25_no_outliers"), alpha=0.6) +
  stat_smooth(method = "loess") +
  theme(legend.position="none") + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " 24h-mean"))) + 
  xlim(min, max) +
  ylim(0, 150) + 
  geom_hline(yintercept=MEAN_PM25, col="green", size = 1) +
  geom_hline(yintercept=MIN-2, col="black", size = 1, linetype="dashed") +
  
  geom_text(aes(x = as.Date("2012-06-01") , y = 50,
                label = "long term annual mean: 44,2 ug/m3"), size = 8) +
  geom_text(aes(x = as.Date("2016-05-01") , y = 36,
                label = "lower limit: 39 ug/m3"), size = 8, col="black")

 PM25_TREND_6_YEARS_no_outliers


par(oldpar)
dev.off()





