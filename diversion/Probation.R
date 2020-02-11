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
all_djs <- filter(all_djs, year(all_djs$COMPLAINT_DATE.x) > 2009)
all_djs <- filter(all_djs, year(all_djs$COMPLAINT_DATE.x) < 2019)

#find most severe case for an individual
top_djs <- all_djs %>%
  group_by(REVLEGALINCIDENT_KEY) %>%
  top_n(-1, FINAL_RANK) %>%
  ungroup()

probation <- filter(top_djs, grepl("Probation|Supervision", DISPOSITION_TEXT))
probation$year <- year(probation$COMPLAINT_DATE.x) #COMPLAINT_DATE.x is from the offense table

yrprobations <- probation %>%
  filter(year > 2009, year < 2019) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

probationsbycounty <- spread(yrprobations, year, count)
formattable(probationsbycounty, align = c("l", rep("c", NCOL(probationsbycounty) - 1)))

#tile by county, number of probations per year
probationperyear <- ggplot(yrprobations, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2018)) +
  labs(x = "Year", y = "Number of Probations", 
       title = "Annual Probations in MD Counties") +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)
probationperyear

prob2010 <- probation %>%
  filter(year > 2009, year < 2019) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

prob2010b <- spread(prob2010, year, count)
formattable(prob2010b, align = c("l", rep("c", NCOL(prob2010b) - 1)))


#Committed Cases
committed <- filter(top_djs, grepl("Committed", DISPOSITION_TEXT))
committed$year <- year(committed$COMPLAINT_DATE.x) #.x means from the original offense table

yrcommits <- committed %>%
  filter(year > 2009, year < 2019) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

commitsbycounty <- spread(yrcommits, year, count)
formattable(commitsbycounty, align = c("l", rep("c", NCOL(commitsbycounty) - 1)))

commitsperyear <- ggplot(yrcommits, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2018)) +
  labs(x = "Year", y = "Number of Commits", 
       title = "Annual Commits to DJS/DJJ in MD Counties") +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)
commitsperyear

#Adjudicated arrests
adjudicated <- top_djs %>% filter(!is.na(top_djs$ADJUDICATION_DATE))
#Adjudicated arrests that resulted in probation (to DJS, DJJ, parent, etc)
adjudicated_prob <- adjudicated %>% filter(grepl("Probation|Supervision", adjudicated$DISPOSITION_TEXT))

adj_county_prob <- adjudicated_prob %>%
  filter(year > 2009 & year < 2019) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n()) 

#Number of adjudicated crimes ending in probation
numprobation <- ggplot(adj_county_prob, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2018)) +
  labs(x = "Year", y = "Number of Probations", 
       title = "Number of adjudicated arrests that result in probation, 2010-2018") +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)
numprobation

#Adjudicated and sustained
adjudicated_sustained <- adjudicated %>% filter(grepl("S", adjudicated$ADJ_DECISION_CODE))
#Adjudicated, sustained and put on probation
adj_sus_prob <- adjudicated_sustained %>% filter(grepl("Probation|Supervision", adjudicated_sustained$DISPOSITION_TEXT))

sustained_prob <- adj_sus_prob %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

susprobation <- ggplot(sustained_prob, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2018)) +
  labs(x = "Year", y = "Number of Probations", 
       title = "Number of adjudicated, sustained arrests that result in probation, 2010-2018") +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)
susprobation

#informaled cases 
informaled <- top_djs %>% filter(grepl("Informaled", top_djs$DETNDECIDE_DEC)) 

informaled <- informaled %>%
  group_by(informaled$DECISIONINTK_TEXT) %>%
  summarize(count = n())

#What percentage of misdemeanor crimes were informaled?
misdemeanors <- filter(top_djs, grepl("Misdemeanor", top_djs$OFFENSE_TYPE))
  
mis <- misdemeanors %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

informaled_misdemeanors <- misdemeanors %>%
  filter(grepl("Informaled", misdemeanors$DETNDECIDE_DEC)) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

mis_spread <- spread(mis, year, count)
informaled_spread <- spread(informaled_misdemeanors, year, count)

percent_informaled <- mis_spread %>% select(COUNTY)
percent_informaled$'2010' <- round(as.numeric(informaled_spread$`2010`)/as.numeric(mis_spread$`2010`), digits=2)
percent_informaled$'2011' <- round(as.numeric(informaled_spread$`2011`)/as.numeric(mis_spread$`2011`), digits=2)
percent_informaled$'2012' <- round(as.numeric(informaled_spread$`2012`)/as.numeric(mis_spread$`2012`), digits=2)
percent_informaled$'2013' <- round(as.numeric(informaled_spread$`2013`)/as.numeric(mis_spread$`2013`), digits=2)
percent_informaled$'2014' <- round(as.numeric(informaled_spread$`2014`)/as.numeric(mis_spread$`2014`), digits=2)
percent_informaled$'2015' <- round(as.numeric(informaled_spread$`2015`)/as.numeric(mis_spread$`2015`), digits=2)
percent_informaled$'2016' <- round(as.numeric(informaled_spread$`2016`)/as.numeric(mis_spread$`2016`), digits=2)
percent_informaled$'2017' <- round(as.numeric(informaled_spread$`2017`)/as.numeric(mis_spread$`2017`), digits=2)
percent_informaled$'2018' <- round(as.numeric(informaled_spread$`2018`)/as.numeric(mis_spread$`2018`), digits=2)
percent_informaled <- na.omit(percent_informaled)

#Table of percentages for informaled misdemeanors
View(percent_informaled)


#Probations as a percent of all adjudicated crime (filtered for individuals most severe charge)
countyadj <- adjudicated %>% 
  group_by(COUNTY, year) %>%
  summarize(count = n())
#Total adjudicated crimes per county, 2010-2018
county_yr_adj <- spread(countyadj, year, count)
county_yr_adj <- na.omit(county_yr_adj)

#Total adjucated crimes resulting in probation per county, 2010-2018
county_prob <- adjudicated_prob %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())
county_prob_yr <- spread(county_prob, year, count)

#Percentage of total adjucated crimes resulting in probation
percentprob <- county_prob_yr %>% select(COUNTY)
percentprob$'2010' <- round(as.numeric(county_prob_yr$`2010`)/as.numeric(county_yr_adj$`2010`), digits=2)
percentprob$'2011' <- round(as.numeric(county_prob_yr$`2011`)/as.numeric(county_yr_adj$`2011`), digits=2)
percentprob$'2012' <- round(as.numeric(county_prob_yr$`2012`)/as.numeric(county_yr_adj$`2012`), digits=2)
percentprob$'2013' <- round(as.numeric(county_prob_yr$`2013`)/as.numeric(county_yr_adj$`2013`), digits=2)
percentprob$'2014' <- round(as.numeric(county_prob_yr$`2014`)/as.numeric(county_yr_adj$`2014`), digits=2)
percentprob$'2015' <- round(as.numeric(county_prob_yr$`2015`)/as.numeric(county_yr_adj$`2015`), digits=2)
percentprob$'2016' <- round(as.numeric(county_prob_yr$`2016`)/as.numeric(county_yr_adj$`2016`), digits=2)
percentprob$'2017' <- round(as.numeric(county_prob_yr$`2017`)/as.numeric(county_yr_adj$`2017`), digits=2)
percentprob$'2018' <- round(as.numeric(county_prob_yr$`2018`)/as.numeric(county_yr_adj$`2018`), digits=2)

formattable(percentprob, align = c("l", rep("c", NCOL(percentprob) - 1)))

#all graphs and tables
top_djs #TABLE - filters all DJS data to only include the most severe crime for an individual on a given day
probation #TABLE - all offenses resulting in probation
probationbycounty #TABLE - probation offenses by county
probationperyear #GRAPH - total number of probational offenses committed per year
commitsperyear #GRAPH - total number of offenses resulting in commitment to DJS per year
numprobation #GRAPH - number of adjudicated crimes resulting in probation 
susprobation #GRAPH - number of adjudicated, sustained crimes resulting in probation
percent_informaled #TABLE - percentage of all misdemeanors that were informaled across all counties
percentprob #TABLE - percentage of all adjudicated crimes that resulted in probation


