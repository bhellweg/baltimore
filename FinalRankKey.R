library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)

#Access 2002-2010 arrest data (Maryland)
offense2002_2010 <- read_excel("~/Documents/OPI/data/Djs01242020/NOPASS_Offense_FY2002_2010.xlsx")
offense2011_2019 <- read_excel("~/Documents/OPI/data/Djs01242020/NOPASS_Offense_FY2011_2019.xlsx")

#Create a table comprised of the final rank, offense text and offense xtype, ascending order by rank
#Table created from 2002-2010 DJS data
rank_key02 <- offense2002_2010 %>% 
  select(OFFENSE_CODE, OFFENSE_TEXT, OFFENSE_TYPE, FINAL_RANK) %>% 
  group_by(OFFENSE_CODE, OFFENSE_TEXT, OFFENSE_TYPE, FINAL_RANK) %>%
  summarize(count=n()) %>%
  arrange(FINAL_RANK)


#Create a table comprised of the final rank, offense text and offense xtype, ascending order by rank
#Table created from 2011-2019 DJS data
rank_key11 <- offense2011_2019 %>% 
  select(OFFENSE_CODE, OFFENSE_TEXT, OFFENSE_TYPE, FINAL_RANK) %>% 
  group_by(OFFENSE_CODE, OFFENSE_TEXT, OFFENSE_TYPE, FINAL_RANK) %>%
  summarize(count=n()) %>%
  arrange(FINAL_RANK)

#Merge the two rank tables together to create final key
final_rank <- rbind(rank_key02, rank_key11)



