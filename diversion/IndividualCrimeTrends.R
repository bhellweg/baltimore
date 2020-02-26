library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)


#--------------Inputting data and rearranging/sorting as needed----------------------
#Only looking at crimes committed in 2010 or later
offense1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2002_2010.xlsx")
offense1 <- filter(offense1, year(offense1$OFFENSE_DATE) > 2001)
offense2 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2011_2019.xlsx")
offense2 <- filter(offense2, year(offense2$OFFENSE_DATE) > 2009)
offense2019 <- read_excel("~/Documents/OPI/data/JulyDecember2019Offense.xlsx")
all_offense <- rbind(offense1, offense2, offense2019)

disposition1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2002_2010.xlsx")
disposition1 <- filter(disposition1, year(disposition1$COMPLAINT_DATE) > 2001)
disposition2 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2011_2019.xlsx")
disposition2 <- filter(disposition2, year(disposition2$COMPLAINT_DATE) > 2009)
disposition2019 <- read_excel("~/Documents/OPI/data/JulyDecember2019Disposition.xlsx")
all_disposition <- rbind(disposition1, disposition2, disposition2019)

no_disposition <- all_disposition %>%
  filter(is.na(all_disposition$DISPOSITION_CODE))

disp_key <- read_excel("~/Documents/OPI/data/UnlockedDJSData/COURT_DISP_DECODE.xlsx")
disp_key <- disp_key %>% rename(DISP_CATEGORY = DRG_DISP_CATEGORY)
disposition <- merge(all_disposition, disp_key, by=c("DISPOSITION_CODE", "DISPOSITION_TEXT", "DISP_CATEGORY"))

disposition <- disposition[,c(4:10, 1:3, 11, 12)]
no_disposition$DISP_RANK <- NA
#Official disposition table that we're going to want to use
all_disposition <- rbind(disposition, no_disposition)

all_disposition <- filter(all_disposition, as.Date(all_disposition$COMPLAINT_DATE) < as.Date(all_disposition$DISPOSITION_DATE))

demographics1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Demographics_2002_2019.xlsx")
demographics2 <- read_excel("~/Documents/OPI/data/JulyDecember2019Demographics.xlsx")
demographics <- rbind(demographics1, demographics2)

all_offense$REVLEGALINCIDENT_KEY <- as.numeric(all_offense$REVLEGALINCIDENT_KEY)
all_disposition$REVLEGALINCIDENT_KEY <- as.numeric(all_disposition$REVLEGALINCIDENT_KEY)
demographics$REVLEGALINCIDENT_KEY <- as.numeric(demographics$REVLEGALINCIDENT_KEY)

#merge all tables together
all_djs <- left_join(all_offense, all_disposition, by = c("REVACTOR_ID", "REVLEGALINCIDENT_KEY", "COMPLAINT_DATE"))
#sort by disposition rank
top_disp <- all_djs %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(DISP_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()
#sort by final rank
top_djs <- top_disp %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()
#might be some duplicates with the additional demographic data that was added
top_djs <- left_join(top_djs,demographics, by=c("REVACTOR_ID","REVLEGALINCIDENT_KEY")) 

#Filter for DJS data we want
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) > 2009)
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) < 2020)
#---------------------------Completed DJS table------------------------------

multiple_charge <- top_djs %>% 
  group_by(REVACTOR_ID, OFFENSE_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()

num_arrests <- multiple_charge %>%
  group_by(REVACTOR_ID) %>%
  summarize(count = n()) %>%
  ungroup()
num_arrests <- rename(num_arrests, TotalArrests = count)

arrest_count <- num_arrests %>%
  group_by(TotalArrests) %>%
  summarize(count = n())

#All MD data
arrest_count_frequency <- ggplot(arrest_count, aes(x=TotalArrests, y=count)) +
  geom_bar(stat = "identity", color="black", fill="#F2CA27") + 
  labs(title="Frequency of multiple arrests") +
  xlab("# Arrests") + 
  ylab("# Individuals") 
plot(arrest_count_frequency)

#Add total number of arrests as extra field for multiple_charge table
multiple_charge <- merge(multiple_charge, num_arrests, by="REVACTOR_ID")
multiple_charge <- multiple_charge %>%
  group_by(REVACTOR_ID) %>%
  arrange(OFFENSE_DATE)

#Assign an arrest number (sequence) to each arrest for each person
order <- function(arg1,arg2) {
  multiple_charge %>% filter(arg1 == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE.x < as.Date(arg2)) %>%
    nrow()+1
}

#Look at charges only in Baltimore City
balt_multiple_charge <- multiple_charge %>% filter(COUNTY == "Baltimore City")
balt_multiple_charge$arrestnumber <- mapply(FUN = order,balt_multiple_charge$REVACTOR_ID,balt_multiple_charge$COMPLAINT_DATE.x)

balt_multiple_charge$TotalArrests <- NULL

balt_arrests <- balt_multiple_charge %>%
  group_by(REVACTOR_ID) %>%
  summarize(count = n()) %>%
  ungroup()
balt_arrests <- rename(balt_arrests, TotalArrests = count)

balt_multiple_charge <- merge(balt_multiple_charge, balt_arrests, by="REVACTOR_ID")

balt_count <- balt_arrests %>%
  group_by(TotalArrests) %>%
  summarize(count = n())

#Bar plot for frequency of multiple offense individuals
balt_frequency <- ggplot(balt_count, aes(x=TotalArrests, y=count)) +
  geom_bar(stat = "identity", color="black", fill="#F2CA27") + 
  labs(title="Frequency of multiple arrests in Baltimore City") +
  xlab("# Arrests") + 
  ylab("# Individuals") 
plot(balt_frequency)

#Super round about way to do this but I needed this to create the severity table
crime_severity <- top_djs %>% group_by(COUNTY) %>% summarize(count = n())

#Crime severity as number of arrests increases
severity <- crime_severity %>% select(COUNTY)
severity$COUNTY <- "All MD"

first_crime <- top_djs %>% filter(arrestnum == 1)
second_crimes <- top_djs %>% filter(arrestnum == 2)
third_crimes <- top_djs %>% filter(arrestnum == 3)
fourth_crime <- top_djs %>% filter(arrestnum == 4)
fifth_crime <- top_djs %>% filter(arrestnum == 5)
sixth_crime <- top_djs %>% filter(arrestnum == 6)
seventh_crime <- top_djs %>% filter(arrestnum == 7)
eigth_crime <- top_djs %>% filter(arrestnum == 8)
ninth_crime <- top_djs %>% filter(arrestnum == 9)
tenth_crime <- top_djs %>% filter(arrestnum == 10)
severity$`1` <- sum(first_crime$FINAL_RANK)/nrow(first_crime)
severity$`2` <- sum(second_crimes$FINAL_RANK)/nrow(second_crimes)
severity$`3` <- sum(third_crimes$FINAL_RANK)/nrow(third_crimes)
severity$`4` <- sum(fourth_crime$FINAL_RANK)/nrow(fourth_crime)
severity$`5` <- sum(fifth_crime$FINAL_RANK)/nrow(fifth_crime)
severity$`6` <- sum(sixth_crime$FINAL_RANK)/nrow(sixth_crime)
severity$`7` <- sum(seventh_crime$FINAL_RANK)/nrow(seventh_crime)
severity$`8` <- sum(eigth_crime$FINAL_RANK)/nrow(eigth_crime)
severity$`9` <- sum(ninth_crime$FINAL_RANK)/nrow(ninth_crime)
severity$`10` <- sum(tenth_crime$FINAL_RANK)/nrow(tenth_crime)
severity <- severity %>% filter(row_number() == 1)

sev <- gather(severity, OffenseNum, Count,`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`)

#Display the trends in severity for charges in Baltimore
severity_trend <- ggplot(sev, aes(x=as.numeric(OffenseNum), y=Count, group=1)) +
  geom_line(color = "black", size=0.1) +
  geom_point(color = "#F2CA27", size = 2) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(1, 10 ,1  )) +
  labs(x = "Offense Number", y = "Average Severity", 
       title = "Trends in offense severity by number of arrests")
plot(severity_trend)

#Crime severity of informaled charges
balt_informal <- balt_multiple_charge %>% filter(DETNDECIDE_DEC == "Informaled")

informaled_total <- balt_informal %>% 
  group_by(arrestnumber) %>%
  summarize(count = n())

view(spread(informaled_total, arrestnumber, count))

informaled_frequency <- ggplot(informaled_total, aes(x=arrestnumber, y=count)) +
  geom_bar(stat = "identity", color="black", fill="#F2CA27") + 
  labs(title="Arrest on which charge was informaled") +
  xlab("Arrest #") + 
  ylab("# Individuals") 
plot(informaled_frequency)

informaled <- severity %>% select(COUNTY)
first_informaled <- balt_informal %>% filter(arrestnumber == 1)
second_informaled <- balt_informal %>% filter(arrestnumber == 2)
third_informaled <- balt_informal %>% filter(arrestnumber == 3)
fourth_informaled <- balt_informal %>% filter(arrestnumber == 4)
fifth_informaled <- balt_informal %>% filter(arrestnumber == 5)
informaled$`1` <- sum(first_informaled$FINAL_RANK)/nrow(first_informaled)
informaled$`2` <- sum(second_informaled$FINAL_RANK)/nrow(second_informaled)
informaled$`3` <- sum(third_informaled$FINAL_RANK)/nrow(third_informaled)
informaled$`4` <- sum(fourth_informaled$FINAL_RANK)/nrow(fourth_informaled)
informaled$`5` <- sum(fifth_informaled$FINAL_RANK)/nrow(fifth_informaled)

inform <- gather(informaled, ArrestNum, Count, `1`, `2`, `3`, `4`, `5`)

informaled_trend <- ggplot(inform, aes(x=as.numeric(ArrestNum), y=Count, group=1)) +
  geom_line(color = "black", size=0.1) +
  geom_point(color = "#F2CA27", size = 2) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(1, 5,1)) +
  labs(x = "Offense Number", y = "Average Severity", 
       title = "Trends in offense severity by number of informaled arrests")
plot(informaled_trend)

#How many people were put on probation and then violated probation within 12 months?
#How many people were put on probation for their first crime and the violated probation as their second crime?
first_probation <- balt_multiple_charge %>% 
  filter(grepl("Probation|Supervision", DISPOSITION_TEXT)) %>%
  filter(ADJ_DECISION_CODE == "S") %>% 
  filter(arrestnumber == 1) %>%
  filter(TotalArrests > 1)

#Return YES if the second offense is a probation violation
#Return NO if the second offense is not a probation violation
#Currently there is an issue where if two different offenses have the same complaint date they will
#be given the same arrest number (basically we need to find a way to break ties)
second_violation <- function(id) {
  second_offense <- balt_multiple_charge %>%
    filter(REVACTOR_ID == id) %>% 
    filter(arrestnumber == 2)
  
  if (second_offense$OFFENSE_CODE == "VIOP") {
    return("YES")
  } else {
    return("NO")
  }
}

violated_first <- first_probation %>% select(REVACTOR_ID)
violated_first$SecondViolation <- mapply(FUN=second_violation, violated_first$REVACTOR_ID)

gunviolation <- balt_multiple_charge %>% 
  filter(arrestnumber > 1) %>%
  filter(grepl("Handgun", OFFENSE_TEXT))

gun_ids <- select(gunviolation, REVACTOR_ID)

#Next Steps:
#1) Need to get all of the other violations that happened before the violation for each id
#2) Group all of those other offenses by offense text
#3) Look at proportion of total offenses that led to handgun violation?
#4) Look at # people who had a specific violation that led to handgun violation?


