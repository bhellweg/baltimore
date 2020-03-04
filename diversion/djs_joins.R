library(readxl)
library(tidyverse)
library(lubridate)
library(formattable)
library(scales)

#Load relevant files. Warning: this is a lot of information. The file size is listed after each sheet.Total ~250MB
disp1 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Disposition_2002_2010.xlsx") #39MB
disp2 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Disposition_2011_2019.xlsx") #23MB
off1 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Offense_2002_2010.xlsx") #80MB
off2 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Offense_2011_2019.xlsx") #52MB
demo <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Demographics_2002_2019.xlsx") #42MB

#Merge the two large tables. This may also take a little while.

disp <- rbind(disp1,disp2)
off  <- rbind(off1,off2)

#Set the keys as numerics
off$REVLEGALINCIDENT_KEY <- as.numeric(off$REVLEGALINCIDENT_KEY)
disp$REVLEGALINCIDENT_KEY <- as.numeric(disp$REVLEGALINCIDENT_KEY)
demo$REVLEGALINCIDENT_KEY <- as.numeric(demo$REVLEGALINCIDENT_KEY)

#Filter for most severe offense to get unique keys
top_offense <- off %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()

error_check <- top_offense %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  summarize(count = n())

#merge all tables together
#offdisp <- left_join(all_offense, all_disposition, by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
all_djs <- left_join(top_offense, disp, by = c("REVACTOR_ID", "REVLEGALINCIDENT_KEY", "COMPLAINT_DATE"))
top_djs<- all_djs %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()
#might be some duplicates with the additional demographic data that was added
top_djs <- left_join(top_djs,demo, by=c("REVACTOR_ID","REVLEGALINCIDENT_KEY")) 

#Create Baltimore tables

baltdisp  <- disp %>% filter(COUNTY == "Baltimore City")
baltoff   <- off %>% filter(COUNTY == "Baltimore City")
baltdemo  <- demo %>% filter(COUNTY == "Baltimore City")
baltodemo <- offdemo %>% filter(COUNTY.y == "Baltimore City")
baltodisp <- offdisp %>% filter(COUNTY.y == "Baltimore City")
baltall   <- top_djs %>% filter(COUNTY == "Baltimore City")
notbalt   <- top_djs %>% filter(COUNTY != "Baltimore City")
baltall$month <- as.Date(floor_date(baltall$COMPLAINT_DATE.x, unit = "months"))
top_djs$month <- as.Date(floor_date(top_djs$COMPLAINT_DATE.x, unit = "months"))
all_djs$month <- as.Date(floor_date(all_djs$COMPLAINT_DATE.x, unit = "months"))
top_djs$informaled <- if_else(top_djs$DETNDECIDE_DEC=="Informaled",1,0,0)
top_djs$guilty <- if_else(top_djs$ADJ_DECISION_CODE == "U",0,1,0)
trials  <- top_djs %>% filter(ADJ_DECISION_CODE == "U" | ADJ_DECISION_CODE == "S") %>%
  mutate(guilty = if_else(ADJ_DECISION_CODE == "U",0,1,0)) 

View(trials)

balt2019 <- baltall %>% filter(COMPLAINT_DATE > as.Date('2018-6-30')) %>%
  mutate(guilty = if_else(ADJ_DECISION_CODE == "S",1,0,0)) %>%
  count(ADJ_OFFSEVTX,ADJ_DECISION_CODE) %>%
  group_by(ADJ_OFFSEVTX)


#Median Arrest Severity
bdjs_month <-   aggregate(FINAL_RANK ~ month,baltall,median)

sevmonth <- ggplot(bdjs_month, aes(x = bdjs_month$month, y = bdjs_month$FINAL_RANK)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_y_reverse() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Month", y = "Median Arrest Severity", 
       title = "Median Arrest Severity per Month, 2002-2019",
       subtitle = "Note: Lower Values Correspond with More Severe Offenses")
sevmonth

#For the state overall
djs_month <-   aggregate(FINAL_RANK ~ month,top_djs,median)

asevmonth <- ggplot(djs_month, aes(x = djs_month$month, y = djs_month$FINAL_RANK)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_y_reverse() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%y")) +
  labs(x = "Month", y = "Median Arrest Severity", 
       title = "Median Arrest Severity per Month, 2002-2019",
       subtitle = "Note: Lower Values Correspond with More Severe Offenses")
asevmonth

#Tile by County
county_month <- aggregate(FINAL_RANK ~ COUNTY + month,top_djs,median)

csevmonth <- ggplot(county_month, 
                    aes(x = county_month$month, y = county_month$FINAL_RANK)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = .3) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(breaks = date_breaks("2 years"), 
               labels = date_format("%y")) +
  scale_y_reverse() +
  labs(x = "Month", y = "Severity", 
       title = "Median Arrest Severity per Month, 2002-2019",
       subtitle = "Note: Lower Values Correspond with More Severe Offenses") +
  facet_wrap(~ COUNTY, nrow = 6)
csevmonth

#Youth Arrest Concentration
persondjs <- all_djs %>%
  group_by(REVACTOR_ID) %>%
  top_n(1,COMPLAINT_DATE.x)
View(persondjs)

baltperson <- persondjs %>%
  filter(COUNTY == "Baltimore City")

nbaltperson <- persondjs %>%
  filter(COUNTY != "Baltimore City")

#Percent of youth arrests in Maryland that are Felonies
(nbaltperson %>%
    filter(OFFENSE_TYPE=='Felony')%>%
    nrow())/(persondjs%>%nrow())

#Percent of youth arrests in Baltimore that are Felonies
(baltperson %>%
  filter(OFFENSE_TYPE=='Felony')%>%
  nrow())/(baltperson%>%nrow())

#Percent of youth arrests in Maryland that are felonies
(persondjs %>%
    filter(OFFENSE_TYPE=='Felony')%>%
    nrow())/(persondjs%>%nrow())

#Guilty rate at trials

guiltyrate <- trials %>% filter(month > '2010-12-31') %>% 
  group_by(COUNTY.x,month) %>%
  count(COUNTY.x,pguilty = mean(guilty))
View(guiltyrate)

rtrials <- trials %>% filter(month > '2014-12-31')

btrials <- trials %>% filter(COUNTY.x != "Baltimore City") %>% filter(OFFENSE_TYPE != "Felony")
mean(btrials$guilty)

rgmtable <-aggregate(by = c(rtrials$guilty,rtrials),rtrials,mean)
View(rgmtable)

grate <- trials %>% filter(month > '2009-12-31') %>% 
  filter(OFFENSE_TYPE == "Felony") %>%
  filter(COUNTY.x == "Baltimore City"|
           COUNTY.x == "Baltimore County"|
           COUNTY.x == "Anne Arundel"|
           COUNTY.x == "Howard") %>%
  group_by(COUNTY.x,month) %>%
  count(COUNTY.x,pguilty = mean(guilty)) %>%
  ggplot(aes(x = as.Date(month), y = pguilty)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  geom_tile()+
  ylim(0,1) +
  facet_wrap(~ COUNTY.x)+
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%y")) +
  labs(x = "Month", y = "Percent Sustained", 
       title = "Percent of Felonies Sustained in Court, 2002-2019")
grate

grate <- trials %>% filter(month > '2008-12-31') %>% 
  filter(DETNDECIDE_CODE.x == "FDWA") %>%
  filter(COUNTY.x == "Baltimore City") %>% #|
           #COUNTY.x == "Baltimore County"|
           #COUNTY.x == "Anne Arundel"|
           #COUNTY.x == "Howard") %>%
  group_by(COUNTY.x,month) %>%
  count(COUNTY.x,pguilty = mean(guilty)) %>%
  ggplot(aes(x = as.Date(month), y = pguilty)) +
  geom_line(color = "#F2CA27", size = 1) +
  geom_smooth(color = "#1A1A1A") +
  geom_tile()+
  ylim(0,1) +
  facet_wrap(~ COUNTY.x)+
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Month", y = "Percent Sustained", 
       title = "Percent of Adult Court Waivers Sustained in Court, 2009-2019",
       subtitle = "Note: Waivers are when Individuals are First Charged as Adults")
grate

fdwas <- trials %>% filter(month > '2008-12-31') %>%
  filter(DETNDECIDE_CODE.x == "FDWA") %>%
  filter(COUNTY.x == "Baltimore City")%>% 
  count(month = as.Date(month)) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "#F2CA27", size = 1) +
  geom_smooth(color = "#1A1A1A") +
  ylim(0,30) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Month", y = "Waivers from Adult Court", 
       title = "Count of Waivers from Adult Court in Baltimore, 2009-2020",
       subtitle = "Note: Data is Count per Month, Waivers are when Individuals are First Charged as Adults")
fdwas


#Informaling rate by County

infrate <- all_djs  %>% filter(month > '2014-12-31') %>% 
  group_by(REVLEGALINCIDENT_KEY) %>%
  top_n(-1,FINAL_RANK) %>%
  group_by(COUNTY) %>%
  count(COUNTY,informaled = round(100*mean(informaled),1))
View(infrate)

all_djs %>% filter(COUNTY == "Washington") %>% count()
all_djs %>% filter(COUNTY == "Baltimore City") %>% count()
all_djs %>% filter(informaled == 1) %>% View()

informals <- all_djs %>% filter(month > '2014-12-31') %>%
  group_by(REVLEGALINCIDENT_KEY) %>%
  top_n(-1,FINAL_RANK) %>%
  filter(COUNTY!="Baltimore City")

irate <- ggplot(infrate, aes(x = infrate$month, y = infrate$informaled)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  geom_tile()+
  facet_wrap(~ COUNTY,scales = "free_y")+
  scale_x_date(breaks = date_breaks("2 years"), labels = date_format("%y")) +
  labs(x = "Month", y = "Percent Informaled", 
       title = "Percent Informaled by County, 2002-2019")
irate

#### Applying Katie's Code

#Filter for DJS data we want
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) > 2009)
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) < 2020)


probation <- top_djs %>% filter("Probation" == DISP_CATEGORY)
probation$year <- year(probation$COMPLAINT_DATE.x) #COMPLAINT_DATE.x is from the offense table


yrprobations <- probation %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n())

probationsbycounty <- spread(yrprobations, year, count)
formattable(probationsbycounty, align = c("l", rep("c", NCOL(probationsbycounty) - 1)))

#tile by county, number of probations per year
probationperyear <- ggplot(yrprobations, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2010, 2012, 2014, 2016, 2018, 2020)) +
  labs(x = "Year", y = "Number of Probations", 
       title = "Annual Probations in MD Counties") +
  facet_wrap(~ COUNTY.x, nrow = 6, scales = "free_y")
probationperyear

prob2010 <- probation %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n())

prob2010b <- spread(prob2010, year, count)
formattable(prob2010b, align = c("l", rep("c", NCOL(prob2010b) - 1)))

#Committed Cases
committed <- filter(top_djs, grepl("Committed", DISPOSITION_TEXT))
committed$year <- year(committed$COMPLAINT_DATE.x) #.x means from the original offense table

yrcommits <- committed %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n())

commitsbycounty <- spread(yrcommits, year, count)
formattable(commitsbycounty, align = c("l", rep("c", NCOL(commitsbycounty) - 1)))

commitsperyear <- ggplot(yrcommits, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2020), breaks=c(2010, 2012, 2014, 2016, 2018, 2020)) +
  labs(x = "Year", y = "Number of Commits", 
       title = "Annual Commits to DJS/DJJ in MD Counties") +
  facet_wrap(~ COUNTY.x, nrow = 6, scales = "free_y") +
  ylim(0,NA)
commitsperyear

#Adjudicated arrests
adjudicated <- top_djs %>% filter(!is.na(top_djs$ADJUDICATION_DATE))
adjudicated$year <- year(adjudicated$COMPLAINT_DATE.x)
#Adjudicated arrests that resulted in probation (to DJS, DJJ, parent, etc)
adjudicated_prob <- adjudicated %>% filter(grepl("Probation|Supervision", adjudicated$DISPOSITION_TEXT))

adj_county_prob <- adjudicated_prob %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n()) 

#Number of adjudicated crimes ending in probation
numprobation <- ggplot(adj_county_prob, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2020), breaks=c(2010, 2012, 2014, 2016, 2018, 2020)) +
  labs(x = "Year", y = "Number of Probations", 
       title = "Number of adjudicated arrests that result in probation, 2010-2018") +
  facet_wrap(~ COUNTY.x, nrow = 6, scales = "free_y") +
  ylim(0,NA)
numprobation

#Adjudicated and sustained
adjudicated_sustained <- adjudicated %>% filter(grepl("S", adjudicated$ADJ_DECISION_CODE))
#Adjudicated, sustained and put on probation
adj_sus_prob <- adjudicated_sustained %>% filter(grepl("Probation|Supervision", adjudicated_sustained$DISPOSITION_TEXT))

sustained_prob <- adj_sus_prob %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n())

susprobation <- ggplot(sustained_prob, aes(x=year, y=count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2010, 2020), breaks=c(2010, 2012, 2014, 2016, 2018, 2020)) +
  labs(x = "Year", y = "Number of Probations", 
       title = "Number of adjudicated, sustained arrests that result in probation, 2010-2018") +
  facet_wrap(~ COUNTY.x, nrow = 6, scales = "free_y") +
  ylim(0,NA)
susprobation

#Probations as a percent of all adjudicated crime (filtered for individuals most severe charge)
countyadj <- adjudicated %>% 
  group_by(COUNTY.x, year) %>%
  summarize(count = n())
#Total adjudicated crimes per county, 2010-2018
county_yr_adj <- spread(countyadj, year, count)
county_yr_adj <- na.omit(county_yr_adj)

#Total adjucated crimes resulting in probation per county, 2010-2018
county_prob <- adjudicated_prob %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n())
county_prob_yr <- spread(county_prob, year, count)

#Percentage of total adjucated crimes resulting in probation
percentprob <- county_prob_yr %>% select(COUNTY.x)
percentprob$'2010' <- round(as.numeric(county_prob_yr$`2010`)/as.numeric(county_yr_adj$`2010`), digits=2)
percentprob$'2011' <- round(as.numeric(county_prob_yr$`2011`)/as.numeric(county_yr_adj$`2011`), digits=2)
percentprob$'2012' <- round(as.numeric(county_prob_yr$`2012`)/as.numeric(county_yr_adj$`2012`), digits=2)
percentprob$'2013' <- round(as.numeric(county_prob_yr$`2013`)/as.numeric(county_yr_adj$`2013`), digits=2)
percentprob$'2014' <- round(as.numeric(county_prob_yr$`2014`)/as.numeric(county_yr_adj$`2014`), digits=2)
percentprob$'2015' <- round(as.numeric(county_prob_yr$`2015`)/as.numeric(county_yr_adj$`2015`), digits=2)
percentprob$'2016' <- round(as.numeric(county_prob_yr$`2016`)/as.numeric(county_yr_adj$`2016`), digits=2)
percentprob$'2017' <- round(as.numeric(county_prob_yr$`2017`)/as.numeric(county_yr_adj$`2017`), digits=2)
percentprob$'2018' <- round(as.numeric(county_prob_yr$`2018`)/as.numeric(county_yr_adj$`2018`), digits=2)
percentprob$'2019' <- round(as.numeric(county_prob_yr$`2019`)/as.numeric(county_yr_adj$`2019`), digits=2)
formattable(percentprob, align = c("l", rep("c", NCOL(percentprob) - 1)))

#Percent of adjudicated cases that are committed
commitsbycounty <- spread(yrcommits, year, count)
formattable(commitsbycounty, align = c("l", rep("c", NCOL(commitsbycounty) - 1)))
county_yr_adj #cases adjudicated in each county each year

percentcommit <- county_prob_yr %>% select(COUNTY.x)
percentcommit$'2010' <- round(as.numeric(commitsbycounty$`2010`)/as.numeric(county_yr_adj$`2010`), digits=2)
percentcommit$'2011' <- round(as.numeric(commitsbycounty$`2011`)/as.numeric(county_yr_adj$`2011`), digits=2)
percentcommit$'2012' <- round(as.numeric(commitsbycounty$`2012`)/as.numeric(county_yr_adj$`2012`), digits=2)
percentcommit$'2013' <- round(as.numeric(commitsbycounty$`2013`)/as.numeric(county_yr_adj$`2013`), digits=2)
percentcommit$'2014' <- round(as.numeric(commitsbycounty$`2014`)/as.numeric(county_yr_adj$`2014`), digits=2)
percentcommit$'2015' <- round(as.numeric(commitsbycounty$`2015`)/as.numeric(county_yr_adj$`2015`), digits=2)
percentcommit$'2016' <- round(as.numeric(commitsbycounty$`2016`)/as.numeric(county_yr_adj$`2016`), digits=2)
percentcommit$'2017' <- round(as.numeric(commitsbycounty$`2017`)/as.numeric(county_yr_adj$`2017`), digits=2)
percentcommit$'2018' <- round(as.numeric(commitsbycounty$`2018`)/as.numeric(county_yr_adj$`2018`), digits=2)
percentcommit$'2019' <- round(as.numeric(commitsbycounty$`2019`)/as.numeric(county_yr_adj$`2019`), digits=2)
formattable(percentcommit, align = c("l", rep("c", NCOL(percentcommit) - 1)))

#informaled cases 
informaled <- top_djs %>% filter(grepl("Informal", top_djs$DETNDECIDE_DEC)) 

informaled <- informaled %>%
  group_by(DECISIONINTK_TEXT) %>%
  summarize(count = n())

#What percentage of misdemeanor crimes were informaled?
misdemeanors <- filter(top_djs, grepl("Misdemeanor", top_djs$OFFENSE_TYPE))
misdemeanors$year <- year(misdemeanors$COMPLAINT_DATE.x)

mis <- misdemeanors %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n())

informaled_misdemeanors <- misdemeanors %>%
  filter(grepl("Informaled", misdemeanors$DETNDECIDE_DEC)) %>%
  group_by(COUNTY.x, year) %>%
  summarize(count = n())

mis_spread <- spread(mis, year, count)
informaled_spread <- spread(informaled_misdemeanors, year, count)

percent_informaled <- mis_spread %>% select(COUNTY.x)
percent_informaled$'2010' <- round(as.numeric(informaled_spread$`2010`)/as.numeric(mis_spread$`2010`), digits=2)
percent_informaled$'2011' <- round(as.numeric(informaled_spread$`2011`)/as.numeric(mis_spread$`2011`), digits=2)
percent_informaled$'2012' <- round(as.numeric(informaled_spread$`2012`)/as.numeric(mis_spread$`2012`), digits=2)
percent_informaled$'2013' <- round(as.numeric(informaled_spread$`2013`)/as.numeric(mis_spread$`2013`), digits=2)
percent_informaled$'2014' <- round(as.numeric(informaled_spread$`2014`)/as.numeric(mis_spread$`2014`), digits=2)
percent_informaled$'2015' <- round(as.numeric(informaled_spread$`2015`)/as.numeric(mis_spread$`2015`), digits=2)
percent_informaled$'2016' <- round(as.numeric(informaled_spread$`2016`)/as.numeric(mis_spread$`2016`), digits=2)
percent_informaled$'2017' <- round(as.numeric(informaled_spread$`2017`)/as.numeric(mis_spread$`2017`), digits=2)
percent_informaled$'2018' <- round(as.numeric(informaled_spread$`2018`)/as.numeric(mis_spread$`2018`), digits=2)
percent_informaled$'2019' <- round(as.numeric(informaled_spread$`2019`)/as.numeric(mis_spread$`2019`), digits=2)
percent_informaled <- na.omit(percent_informaled)

#Table of percentages for informaled misdemeanors
View(percent_informaled)

#all graphs and tables
top_djs #TABLE - filters all DJS data to only include the most severe crime for an individual on a given day
probation #TABLE - all offenses resulting in probation
probationbycounty #TABLE - probation offenses by county
probationperyear #GRAPH - total number of probational offenses committed per year
commitsperyear #GRAPH - total number of offenses resulting in commitment to DJS per year
numprobation #GRAPH - number of adjudicated crimes resulting in probation 
susprobation #GRAPH - number of adjudicated, sustained crimes resulting in probation
percentcommit #TABLE - percentage of all adjudicated crimes that resulted in commitment to DJS
percent_informaled #TABLE - percentage of all misdemeanors that were informaled across all counties
percentprob #TABLE - percentage of all adjudicated crimes that resulted in probation


# sequence numbers

 order <- function(arg1,arg2){
   baltall %>% filter(arg1 == REVACTOR_ID) %>%
     filter(COMPLAINT_DATE.x < as.Date(arg2)) %>% 
     nrow()+1}
 
order(arg1 = 1292, arg2 = '2003-09-23')

baltall$arrestnumber <- mapply(FUN = order,baltall$REVACTOR_ID,baltall$COMPLAINT_DATE.x)



