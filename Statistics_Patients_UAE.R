
library(ggplot2)
library(dplyr)

# load dataset of helath data from Abu Dhabi, Dubai and Northern Emirates

Dubai_Northern <- read.csv("D:/R_processing/Clinics 2013-2016/health_data_age_bins.csv")
Abu_Dhabi <- read.csv("D:/R_processing/health_data_age_bins.csv")

# load sites for clinics in Dubai and Northern emirates

sites <- read.csv("D:/R_processing/Clinics 2013-2016/sites_hospitals.csv")
# join hospital locations for Dubai and Northern Emirates

Dubai_Northern <- Dubai_Northern %>%
  left_join(sites, "HOSPITAL")



# abu dhabi is only asthma data that is IC9 493
Abu_Dhabi$IC9 <- "493"
Abu_Dhabi$Emirate <- "Abu Dhabi"


# bind datasets with same fields

Dubai_Northern_selected <- Dubai_Northern %>%
  select(Date,
         Encounter_Type,
         IC9,
         Age,
         AGE_BIN,
         Gender,
         bin,
         Emirate)

colnames(Dubai_Northern_selected) <- c("Date", "ET", "IC9", "Age", "AGE_BIN", "Gender", "bin", "Emirate")




Abu_Dhabi_selected <- Abu_Dhabi %>%
  select(Date,
         ET,
         IC9,
         Age,
         AGE_BIN,
         Gender,
         bin,
         Emirate)

colnames(Abu_Dhabi_selected) <- c("Date", "ET", "IC9", "Age", "AGE_BIN", "Gender", "bin", "Emirate")


health_data <- rbind(Dubai_Northern_selected,
                     Abu_Dhabi_selected)


health_data <- rbind(Dubai_Northern_selected)



# sum of all the hospital admissions 

health_data_sum <- health_data %>%
  summarise(sum = sum(bin))


# group data by encounter type
health_data_ET <- health_data %>%
  dplyr::group_by(ET) %>%
  summarise(counts = sum(bin, na.rm = TRUE))


# group data by diagnosis (IC9)
health_data_IC9 <- health_data %>%
  dplyr::group_by(IC9) %>%
  summarise(counts = sum(bin, na.rm = TRUE))

# remove small ET

health_data_ET <- health_data_ET %>%
  filter(!ET == "Recurring Outpatient") %>%
  filter(!ET == "RX Outpatient Lifetime") %>%
  filter(!ET == "Observation/Medical Short Stay") %>%
  filter(!ET == "Observation/Paediatric Short Stay") %>%
  filter(!ET == "Outpatient Lifetime") 


# group data by Emirates

health_data_emirates <- health_data %>%
  group_by(Emirate) %>%
  summarise(counts = sum(bin, na.rm = TRUE))



###############################################################################
###############################################################################
# box plots ###################################################################


health_data_ET$perc_weight <- ((health_data_ET$counts)/nrow(health_data))*100

# arrange data by counts

health_data_ET <- health_data_ET %>%
  arrange(- counts)


library(plyr)

# calculate midpoints of bars
health_data_ET <- ddply(health_data_ET, .(ET), 
                       transform, pos = cumsum(perc_weight) - (0.5 * perc_weight)
)



write.csv(health_data_ET, "health_outcomes_UAE.csv")


jpeg("percentage_hospital_admissions_by_encounter_type.jpg",
     quality = 100, bg = "white", res = 200, width = 9, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


p <- ggplot(data = health_data_ET,
            aes(x = reorder(ET, - counts), perc_weight, fill = ET)) +
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  
  theme_bw() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                           
  ylab("hospital admissions (%)") +                                                        
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  xlab(" ") +          
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust=0.5, size=18)) +
  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("percentage of hospital admissions by encounter type") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=20, hjust = 0.5))
p



par(oldpar)
dev.off()



#########################################
# percentage of diagnosis by IC9 codes ##


health_data_IC9$perc_weight <- ((health_data_IC9$counts)/nrow(health_data))*100


library(plyr)

# calculate midpoints of bars
health_data_IC9 <- ddply(health_data_IC9, .(IC9), 
                        transform, pos = cumsum(perc_weight) - (0.5 * perc_weight)
)


write.csv(health_data_IC9, "diagnosis_all_UAE.csv")



#########################################
# percentage of diagnosis by IC9 codes ##

health_data_emirates$perc_weight <- ((health_data_emirates$counts)/nrow(health_data))*100

write.csv(health_data_emirates, "emirates_all_UAE.csv")



