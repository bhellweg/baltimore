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

#Add total number of arrests as extra field for multiple_charge table
multiple_charge <- merge(multiple_charge, num_arrests, by="REVACTOR_ID")
multiple_charge <- multiple_charge %>%
  group_by(REVACTOR_ID) %>%
  arrange(OFFENSE_DATE)

#not working
order <- function(arg1,arg2) {
  balt_multiple_charge %>% filter(arg1 == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE.x < as.Date(arg2)) %>%
    nrow()+1
}

balt_multiple_charge <- multiple_charge %>% filter(COUNTY == "Baltimore City")
balt_multiple_charge$arrestnumber <- mapply(FUN = order,balt_multiple_charge$REVACTOR_ID,balt_multiple_charge$COMPLAINT_DATE.x)

order(3082159, '2015-09-23')

arrest_count <- num_arrests %>%
  group_by(TotalArrests) %>%
  summarize(count = n())
  
arrest_count_frequency <- ggplot(arrest_count, aes(x=COUNT, y=count)) +
  geom_bar(stat = "identity", color="black", fill="#F2CA27") + 
  labs(title="Frequency of multiple arrests") +
  xlab("# Arrests") + 
  ylab("# Individuals") 
plot(arrest_count_frequency)


#Average severity of second offense, average severity of 3rd offense
crime_severity$`2nd` <- sum()

#Other things to look at: How many people were put on probation and then violated probation 
#within 12 months?



#need to figure out how to assign a sequence number to each arrest for one individual
assign_number <- function(id) {
  all_instances <- multiple_charge %>%
    dplyr::filter(REVACTOR_ID == id) %>%
    dplyr::select(REVACTOR_ID, OFFENSE_DATE)
  
  all_instances$Sequence <- seq.int(nrow(all_instances))
  
  return(all_instances)
}
