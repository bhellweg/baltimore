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

disposition2002_2010 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2002_2010.xlsx")
disposition2011_2019 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2011_2019.xlsx")
offense2002_2010 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2002_2010.xlsx")
offense2011_2019 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2011_2019.xlsx")

#combine all offense data
all_offense <- rbind(offense2002_2010, offense2011_2019)

#Make decision code key
decision_codes <- all_offense %>% 
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_frequency <- ggplot(decision_codes, aes(x=DETNDECIDE_CODE, y=decision_codes$count)) +
  geom_bar(stat="identity", color="black", fill="lightblue") +
  labs(title="Frequency of Decision Type for Youth Arrests MD, 2002-2019") +
  scale_x_discrete("Codes", labels=c("AINF", "AITO", "CRAI", "DISA", 
                            "DRNJ", "FAFP", "FDVP", "FDWA", "FFSA", 
                            "FIDA", "FNFA", "IIDA", "SAFO")) +
  scale_y_continuous(breaks=seq(0, 453128, 50000))
  
plot(decision_frequency)

#----------Decision Frequency By County-----------------------
decision_Allegany <- all_offense %>% 
  filter(COUNTY=="Allegany") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decisions_AnneArundel <- all_offense %>% 
  filter(COUNTY=="Anne Arundel") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decisions_BaltimoreCity <- all_offense %>% 
  filter(COUNTY=="Baltimore City") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decisions_BaltimoreCounty <- all_offense %>% 
  filter(COUNTY=="Baltimore County") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Calvert <- all_offense %>% 
  filter(COUNTY=="Calvert") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Caroline <- all_offense %>% 
  filter(COUNTY=="Caroline") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Carroll <- all_offense %>% 
  filter(COUNTY=="Carroll") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Cecil <- all_offense %>% 
  filter(COUNTY=="Cecil") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Charles <- all_offense %>% 
  filter(COUNTY=="Charles") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Dorchester <- all_offense %>% 
  filter(COUNTY=="Dorchester") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Frederick <- all_offense %>% 
  filter(COUNTY=="Frederick") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Garrett <- all_offense %>% 
  filter(COUNTY=="Garrett") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Harford <- all_offense %>% 
  filter(COUNTY=="Harford") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Howard <- all_offense %>% 
  filter(COUNTY=="Howard") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Kent <- all_offense %>% 
  filter(COUNTY=="Kent") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Montgomery <- all_offense %>% 
  filter(COUNTY=="Montgomery") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

#doesn't work because of apostrophe
decision_PrinceGeorge <- all_offense %>% 
  filter(COUNTY=="Prince George's") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

#Apostrophe problem -- no data extracted
decision_QueenAnnes <- all_offense %>% 
  filter(COUNTY=="Queen Anne's") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Somerset <- all_offense %>% 
  filter(COUNTY=="Somerset") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

#This currently isn't working bc of the apostrophe in the name
decision_StMary <- all_offense %>% 
  filter(COUNTY=="St. Mary's") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Talbot <- all_offense %>% 
  filter(COUNTY=="Talbot") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Washington <- all_offense %>% 
  filter(COUNTY=="Washington") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Wicomico <- all_offense %>% 
  filter(COUNTY=="Wicomico") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_Worcester <- all_offense %>% 
  filter(COUNTY=="Worcester") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)


#TODO: plot all of the counties by number of occurrences, may want to examine percentages though
# because some counties have a lot more arrests than others

