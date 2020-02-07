library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

offense1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2002_2010.xlsx")
offense2 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2011_2019.xlsx")
all_offense <- rbind(offense1, offense2)

disposition1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2002_2010.xlsx")
disposition2 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2011_2019.xlsx")
all_disposition <- rbind(disposition1, disposition2)

demographics <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Demographics_2002_2019.xlsx")

all_offense$REVLEGALINCIDENT_KEY <- as.numeric(all_offense$REVLEGALINCIDENT_KEY)
all_disposition$REVLEGALINCIDENT_KEY <- as.numeric(all_disposition$REVLEGALINCIDENT_KEY)
demographics$REVLEGALINCIDENT_KEY <- as.numeric(demographics$REVLEGALINCIDENT_KEY)

#merge all tables together
offdisp <- left_join(all_offense, all_disposition, by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
all_djs <- left_join(offdisp,demographics,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))

#Get data between 2002-01-01 and 2018-12-31
all_djs <- filter(all_djs, year(all_djs$COMPLAINT_DATE.x) > 2001)
all_djs <- filter(all_djs, year(all_djs$COMPLAINT_DATE.x) < 2019)

#find most severe case for an individual that was adjudicated
top_djs <- all_djs %>%
  group_by(REVLEGALINCIDENT_KEY) %>%
  top_n(-1, FINAL_RANK) %>%
  ungroup()
View(top_djs)
probation <- filter(top_djs, grepl("Probation|Supervision", DISPOSITION_TEXT))
probation$year <- year(probation$COMPLAINT_DATE.x) #COMPLAINT_DATE.x is from the offense table

yrprobations <- probation %>%
  filter(year > 2002, year < 2018) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

probationsbycounty <- spread(yrprobations, year, count)
formattable(probationsbycounty, align = c("l", rep("c", NCOL(probationsbycounty) - 1)))

#tile by county, number of probations per year
probationperyear <- ggplot(yrprobations, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2002, 2018)) +
  labs(x = "Year", y = "Number of Probations", 
       title = "Annual Probations in MD Counties") +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)
probationperyear


#Committed Cases
committed <- filter(top_djs, grepl("Committed", DISPOSITION_TEXT))
committed$year <- year(committed$COMPLAINT_DATE.x) #.x means from the original offense table

yrcommits <- committed %>%
  filter(year > 2002, year < 2018) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

commitsbycounty <- spread(yrcommits, year, count)
formattable(commitsbycounty, align = c("l", rep("c", NCOL(commitsbycounty) - 1)))

commitsperyear <- ggplot(yrprobations, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2002, 2018)) +
  labs(x = "Year", y = "Number of Commits", 
       title = "Annual Commits to DJS/DSS in MD Counties") +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)
commitsperyear

