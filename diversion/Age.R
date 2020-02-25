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

balt_djs <- filter(top_djs, top_djs$COUNTY == "Baltimore City")
#---------------------------Completed DJS table------------------------------

balt_djs$year <- year(top_djs$COMPLAINT_DATE.x)

first_arrest <- balt_djs %>%
  group_by(REVACTOR_ID) %>%
  arrange(COMPLAINT_DATE.x) %>%
  filter(row_number() == 1) %>%
  ungroup()

#get number of kids of each age in the system each year
ages <- first_arrest %>% 
  select(AGE_COMPLAINT, year) %>%
  group_by(AGE_COMPLAINT, year) %>%
  summarize(count = n()) %>%
  arrange(year) %>%
  ungroup()

ages <- ages %>% rename(total = count)
ages <- na.omit(ages)

balt_djs$arrestnum <- mapply(FUN = order, balt_djs$REVACTOR_ID, balt_djs$COMPLAINT_DATE.x)

order <- function(arg1,arg2) {
  balt_djs %>% filter(arg1 == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE.x < as.Date(arg2) & ARREST_DATE.x < as.Date(arg2)) %>%
    nrow()+1
}

find_avg_arrests <- function(yearentered, age) {
  year_ids <-  balt_djs %>% 
    filter(arrestnum == 1) %>%
    filter(age == AGE_COMPLAINT) %>%
    filter(yearentered == year) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x) 
  
  crime_count <- 0
  crime_count <- crime_count + mapply(FUN=count_future_arrests, id=year_ids$REVACTOR_ID, date=year_ids$COMPLAINT_DATE.x)
  
  return(sum(crime_count)/nrow(year_ids))
}

count_future_arrests <- function(id, date) {
  balt_djs %>% filter(id == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE.x > as.Date(date)) %>%
    nrow()
}

find_avg_severity <- function(yearentered, age) {
  year_ids <-  balt_djs %>% 
    filter(arrestnum == 1) %>%
    filter(age == AGE_COMPLAINT) %>%
    filter(yearentered == year) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x)
  
  severity <- 0
  severity <- severity + mapply(FUN=future_severity, id=year_ids$REVACTOR_ID, date=year_ids$COMPLAINT_DATE.x)
  
  return(sum(severity)/nrow(year_ids))
}

future_severity <- function(id, date) {
  all_sev <- balt_djs %>% filter(id == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE.x >= as.Date(date))
  
  return(sum(all_sev$FINAL_RANK)/nrow((all_sev)))
}

#Calculates average total number of arrests expect for each age/year cohort
ages$avg <- mapply(FUN=find_avg_arrests, ages$year, ages$AGE_COMPLAINT)

ages <- ages %>% filter(AGE_COMPLAINT > 9)
ages <- ages %>% filter(AGE_COMPLAINT < 18)

ages <- ages %>%
  mutate(yrsto18 = 18 - AGE_COMPLAINT,
         yrstoend = 2020 - year,
         avgperyear = ifelse(yrsto18 < yrstoend, avg / yrsto18, avg / yrstoend )
         )
#Calculates average severity of future arrests for each cohort (includes first arrest)
ages$sev <- mapply(FUN=find_avg_severity, ages$year, ages$AGE_COMPLAINT)

#For graphing purposes
ages$charAge <- as.character(ages$AGE_COMPLAINT)

#Graph average number of expected arrests per year for each age/year cohort
agecrime <- ggplot(ages, aes(x=year, y=avgperyear, group=charAge, color=charAge)) +
  geom_path() +
  geom_point() +
  scale_x_continuous(limits = c(2010, 2019), breaks=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  ggtitle("Year of first arrest by average number of expected arrests per subsequent year") +
  xlab("Year of first arrest") +
  ylab("Average arrests per year expected until they turn 18 or before 2019")
agecrime

ageseverity <- ggplot(ages, aes(x=year, y=sev, group=charAge, color=charAge)) +
  geom_path() +
  geom_point() +
  scale_x_continuous(limits = c(2010, 2019), breaks=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  scale_y_reverse(limits=c(120,40), breaks=c(120, 110, 100, 90, 80, 70, 60, 50, 40)) +
  ggtitle("Year of first arrest by severity of subsequent arrests (includes initial arrest)") +
  xlab("Year of first arrest") +
  ylab("Average severity of subsequent arrests")
ageseverity

#Other areas to look into: 
#1) % felony charges for age/year cohorts
  #maybe we want to just see for each group of kids in X year, how many of their charges were actually sustained
#2) avg sustained/unsustained charges 



#ALL DJS DATA --> Here for state-wide comparisons but not super necessary atm
top_djs$arrestnum <- mapply(FUN = order, top_djs$REVACTOR_ID, top_djs$COMPLAINT_DATE.x)
all_first_arrest <- top_djs %>%
  group_by(REVACTOR_ID) %>%
  arrange(COMPLAINT_DATE.x) %>%
  filter(row_number() == 1) %>%
  ungroup()


