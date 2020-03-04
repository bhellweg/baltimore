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
#Filtered for just Baltimore because the entire dataset took too long
multiple_charge <- top_djs %>% 
  filter(COUNTY == "Baltimore City") %>%
  group_by(REVACTOR_ID, OFFENSE_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()

num_arrests <- multiple_charge %>%
  group_by(REVACTOR_ID) %>%
  summarize(count = n()) %>%
  ungroup()
num_arrests <- rename(num_arrests, totalarrests = count)

djs <- merge(multiple_charge, num_arrests, by="REVACTOR_ID")
djs <- djs %>%
  group_by(REVACTOR_ID) %>%
  arrange(OFFENSE_DATE)

#Assign an arrest number (sequence) to each arrest for each person
order <- function(arg1, arg2) {
  djs %>% filter(arg1 == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE.x < as.Date(arg2)) %>%
    nrow()+1
}

djs$arrestnum <- mapply(FUN = order, djs$REVACTOR_ID, djs$COMPLAINT_DATE.x)

#Based on the Data Resource Guide from DJS, the average length of stay on probation is 450 days. 
#This will examine probation violations that occur within one year of the initial adjudication.

#Find all sustained offenses that got probation
probations <- djs %>% 
  filter(ADJ_DECISION_CODE == "S") %>%
  filter(grepl("Probation|Supervision", DISPOSITION_TEXT))

#Find an individuals first arrest that resulted in sustained probation
indiv_prob <- probations %>%
  select(REVACTOR_ID, COMPLAINT_DATE.x) %>%
  group_by(REVACTOR_ID) %>%
  arrange(COMPLAINT_DATE.x) %>%
  filter(row_number() == 1) %>%
  ungroup()
indiv_prob <- rename(indiv_prob, probationstart=COMPLAINT_DATE.x)

updated_djs <- left_join(djs, indiv_prob)
updated_djs <- updated_djs %>%
  mutate(dayssinceprobation = difftime(COMPLAINT_DATE.x, probationstart, units = "days"))

post_probation <- updated_djs %>% filter(dayssinceprobation > 0) %>% filter(grepl("VIOP", OFFENSE_CODE))

viop <- post_probation %>% group_by(dayssinceprobation) %>% summarize(count = n()) %>% rename(total=count)


#Graph results
probation_violations <- ggplot(viop, aes(x=as.numeric(dayssinceprobation))) +
  geom_histogram(color="black", fill="lightblue", binwidth = 50) +
  scale_x_continuous(limits=c(0, 700), breaks=c(0, 100, 200, 300, 400, 500, 600, 700)) +
  labs(x="Days since initial probation decision", y="Count", title="Violation of Probation Frequency, Baltimore City 2010-2019")
  
probation_violations
  


