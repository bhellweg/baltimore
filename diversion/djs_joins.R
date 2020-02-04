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

#Join the tables to create a master table using the keys

offdemo <- merge(off,demo,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
offdisp <- merge(off,disp,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
all_djs <- merge(offdisp,demo,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))

#Create Baltimore tables

baltdisp  <- disp %>% filter(COUNTY == "Baltimore City")
baltoff   <- off %>% filter(COUNTY == "Baltimore City")
baltdemo  <- demo %>% filter(COUNTY == "Baltimore City")
baltodemo <- offdemo %>% filter(COUNTY.y == "Baltimore City")
baltodisp <- offdisp %>% filter(COUNTY.y == "Baltimore City")
baltall   <- all_djs %>% filter(COUNTY == "Baltimore City")
baltall$month <- as.Date(floor_date(baltall$COMPLAINT_DATE.x, unit = "months"))
all_djs$month <- as.Date(floor_date(all_djs$COMPLAINT_DATE.x, unit = "months"))

View(baltall)

#Median Arrest Severity
bdjs_month <-   aggregate(FINAL_RANK ~ month,baltall,median)

sevmonth <- ggplot(bdjs_month, aes(x = bdjs_month$month, y = bdjs_month$FINAL_RANK)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_y_reverse() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%y")) +
  labs(x = "Month", y = "Median Arrest Severity", 
       title = "Median Arrest Severity per Month, 2002-2019",
       subtitle = "Note: Lower Values Correspond with More Severe Offenses")
sevmonth

#For the state overall
djs_month <-   aggregate(FINAL_RANK ~ month,all_djs,median)

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
county_month <- aggregate(FINAL_RANK ~ COUNTY + month,all_djs,median)

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

#Tile by County, filtering for highest severity charge of offenses
casedjs <- all_djs %>%
  group_by(REVLEGALINCIDENT_KEY) %>%
  top_n(-1,FINAL_RANK)

county_month <- aggregate(FINAL_RANK ~ COUNTY + month,casedjs,median)

ccsevmonth <- ggplot(county_month, 
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
ccsevmonth

#Youth Arrest Concentration
persondjs <- all_djs %>%
  group_by(REVACTOR_ID) %>%
  top_n(1,COMPLAINT_DATE.x)
View(persondjs)

baltperson <- persondjs %>%
  filter(COUNTY == "Baltimore City")

#Percent of youth arrests in Maryland that are Felonies
(persondjs %>%
    filter(OFFENSE_TYPE=='Felony')%>%
    nrow())/(persondjs%>%nrow())

#Percent of youth arrests in Baltimore that are Felonies
(baltperson %>%
  filter(OFFENSE_TYPE=='Felony')%>%
  nrow())/(baltperson%>%nrow())
