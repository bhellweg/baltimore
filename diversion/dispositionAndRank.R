#Katie Newbold
#Office of Performance and Innovation
#January 29, 2020

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
disposition2002_2010 <- read_excel("~/Documents/OPI/data/Djs01242020/NOPASS_Disposition_FY2002_2010.xlsx")
disposition2011_2019 <- read_excel("~/Documents/OPI/data/Djs01242020/NOPASS_Disposition_FY2011_2019.xlsx")

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

rank_key02$count <- NULL
rank_key11$count <- NULL

#Merge the two rank tables together to create final key
final_rank <- full_join(rank_key02, rank_key11) %>%
  arrange(FINAL_RANK)

#Link final rank with disposition data from 2002-2010 
#Some crimes are reported on only one sheet
disp02 <- disposition2002_2010 %>%
  select(REVACTOR_ID, ARREST_DATE, DISPOSITION_CODE, DISPOSITION_TEXT, COUNTY) %>% 
  rename(OFFENSE_DATE = ARREST_DATE)

#Select arrest data from DJS report
offense02 <- offense2002_2010 %>%
  select(REVACTOR_ID, OFFENSE_DATE, OFFENSE_TEXT, OFFENSE_TYPE, OFFENSE_CODE)

#Inner join disposition data with rank table (2002-2010 data)
disposition_offense_02 <- inner_join(offense02, disp02)

#Add severity rank to disposition and offense data (2002-2010 data)
disposition_offense_rank_02 <- inner_join(disposition_offense_02, final_rank)

#2011-2019 Disposition Data
disp11 <- disposition2011_2019 %>%
  select(REVACTOR_ID, ARREST_DATE, DISPOSITION_CODE, DISPOSITION_TEXT, COUNTY) %>%
  rename(OFFENSE_DATE = ARREST_DATE)

#2011-2019 Offense Data
offense11 <- offense2011_2019 %>%
  select(REVACTOR_ID, OFFENSE_DATE, OFFENSE_TEXT, OFFENSE_TYPE, OFFENSE_CODE)

#join disposition and offense data
disposition_offense_11 <- inner_join(offense11, disp11)
#add rank
disposition_offense_rank_11 <- inner_join(disposition_offense_11, final_rank)

#------------------------Maryland Data----------------------------
#2002-2010 plot
hist02 <- ggplot(disposition_offense_rank_02, aes(x=FINAL_RANK)) + 
  geom_histogram(binwidth = 5, color="black", fill="lightblue", alpha=0.5) +
  labs(title="Maryland Offense Frequency by Rank, 2002-2010 ", x = "Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x = element_text(angle=90))

plot(hist02)

#2011-2019 plot
hist11 <- ggplot(disposition_offense_rank_11, aes(x=FINAL_RANK)) + 
  geom_histogram(binwidth = 5, color="black", fill="lightpink", alpha=0.5) +
  labs(title="Maryland Offense Frequency by Rank, 2011-2019 ", x = "Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x = element_text(angle=90))

plot(hist11)

#------------------------Baltimore Data-----------------------------
baltimore02 <- disposition_offense_rank_02 %>% filter(COUNTY=="Baltimore City")
baltimore11 <- disposition_offense_rank_11 %>% filter(COUNTY=="Baltimore City")

baltimore_hist02 <- ggplot(baltimore02, aes(x=FINAL_RANK)) + 
  geom_histogram(binwidth = 5, color="black", fill="lightblue", alpha=0.5) +
  labs(title="Baltimore Offense Frequency by Rank, 2002-2010 ", x = "Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x = element_text(angle=90))

plot(baltimore_hist02)

baltimore_hist11 <- ggplot(baltimore11, aes(x=FINAL_RANK)) + 
  geom_histogram(binwidth = 5, color="black", fill="lightpink", alpha=0.5) +
  labs(title="Baltimore Offense Frequency by Rank, 2011-2019 ", x = "Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x = element_text(angle=90))

plot(baltimore_hist11)
  

#-------MD Arrests that didn't go to trial (no disposition information)--------
notrial02 <- disposition_offense_rank_02 %>% 
  filter(is.na(DISPOSITION_CODE)) %>%
  arrange(FINAL_RANK)

hist_notrial02 <- ggplot(notrial02, aes(x=FINAL_RANK)) +
  geom_histogram(binwidth = 5, color = "black", fill="blue", alpha=0.5) +
  labs(title="Maryland Offense With No Disposition, 2002-2010", x="Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x=element_text(angle=90))

plot(hist_notrial02)

notrial11 <-disposition_offense_rank_11 %>%
  filter(is.na(DISPOSITION_CODE)) %>%
  arrange(FINAL_RANK)

hist_notrial11 <- ggplot(notrial11, aes(x=FINAL_RANK)) +
  geom_histogram(binwidth = 5, color = "black", fill="red", alpha=0.5) +
  labs(title="Maryland Offense With No Disposition, 2011-2019", x="Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x=element_text(angle=90))

plot(hist_notrial11)

all_notrial <- rbind(notrial02, notrial11)

hist_notrial <- ggplot(all_notrial, aes(x=FINAL_RANK)) +
  geom_histogram(binwidth = 5, color = "black", fill="black", alpha=0.5) +
  labs(title="Maryland Offense With No Disposition, 2002-2019", x="Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x=element_text(angle=90))

plot(hist_notrial)

#-------------Baltimore Arrests, no disposition-------------------
baltimore_notrial02 <- disposition_offense_rank_02 %>% 
  filter(is.na(DISPOSITION_CODE), COUNTY=="Baltimore City") %>%
  arrange(FINAL_RANK)


baltimore_hist_notrial02 <- ggplot(baltimore_notrial02, aes(x=FINAL_RANK)) +
  geom_histogram(binwidth = 5, color = "black", fill="blue", alpha=0.5) +
  labs(title="Baltimore Offense With No Disposition, 2002-2010", x="Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x=element_text(angle=90))

plot(baltimore_hist_notrial02)


baltimore_notrial11 <-disposition_offense_rank_11 %>%
  filter(is.na(DISPOSITION_CODE), COUNTY=="Baltimore City") %>%
  arrange(FINAL_RANK)

baltimore_hist_notrial11 <- ggplot(baltimore_notrial11, aes(x=FINAL_RANK)) +
  geom_histogram(binwidth = 5, color = "black", fill="red", alpha=0.5) +
  labs(title="Baltimore Offense With No Disposition, 2011-2019", x="Final Rank", y="Count") +
  scale_x_continuous(breaks=seq(0, 175, 5)) +
  theme(axis.text.x=element_text(angle=90))

plot(baltimore_hist_notrial11)
