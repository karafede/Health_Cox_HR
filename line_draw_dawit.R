data_lm<- AAA%>%
  filter(RR>= 1)
y_min<- min(AAA$RR)


data_lm <- data_lm %>%
  arrange(RR)
data_lm<- data_lm[1:100,]
xxx<- lm(RR~`AQ_HEALTH_pos$detrend_PM25`, data=data_lm)
xxx$coefficients
x_val<- 0:150
line_eq<- xxx$coefficients[1]+ xxx$coefficients[2]*x_val
ind_intersection <- which(abs(y_min-line_eq) ==min(abs(y_min-line_eq)))


(line_eq [ind_intersection ]- xxx$coefficients[1])/xxx$coefficients[2] -> limit_x
