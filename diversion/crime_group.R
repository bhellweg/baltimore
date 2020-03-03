library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

bpd <- read_excel("~/Documents/OPI/data/BPD Youth Arrests.xlsx")
bpd_gun <- bpd %>% filter(grepl("GUN", chargedesc))
charge_desc <- bpd_gun %>% group_by(chargedesc) %>% summarize(count = n())

bpd_gun <- bpd_gun %>%
  mutate(guntype = ifelse(grepl("HANGUN|HANDGUN|HAND GUN|HANG GUN|HANGGUN|
                                HNADGUN|HNDGUN|HUNDGUN|HYANDGUN", chargedesc), "HANDGUN", "OTHER"))
bpd_handgun <- bpd_gun %>% filter(guntype == "HANDGUN")
bpd_handgun$year <- year(bpd_handgun$arrestdate)

#group by date (for crime grouping)

handgun_trends <- bpd_handgun %>% group_by(year) %>% summarize(count = n())
handgun_trends <- handgun_trends %>% rename(numarrests=count)

hgplot <- ggplot(handgun_trends, aes(x = as.numeric(year), y=numarrests)) +
  geom_line() +
  scale_x_continuous(limits=c(2010,2019), breaks=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  scale_y_continuous(limits=c(180, 270), breaks=c(180, 190, 200, 210, 220, 230, 240, 250, 260, 270)) +
  labs(x="Year", y="Number of Handgun Violations", title="Trends in Handgun Violation Arrests, 2010-2019")
hgplot

daily_hg <- bpd_handgun %>% 
  select(arrestnum, arrestdate, arresttime, dayofweek, nhood)

hg_counts <- bpd_handgun %>% group_by(arrestdate) %>% summarize(count = n())

#include number of arrests 
daily_hg <- merge(daily_hg, hg_counts, by="arrestdate")

#minor testing to attempt to match the BPD and DJS data by inspection
daytest <- top_djs %>% filter(ARREST_DATE == as.Date('2016-03-09')) %>% filter(COUNTY == "Baltimore City")


#CONCLUSIONS (including information gathered from police stat)
#It is challenging (basically impossible) to follow the disposition of an arrest in the DJS data if it
#is recorded in the BPD data because there is not a key to match the datasets together. By inspection
#it appears that the handgun violations recorded by BPD are not even showing up in DJS data, 
#so we cannot make conclusions about what is happening to the youth based on that. Additionally, BPD team 
#at Police Stat on March 3, 2020 mentioned that across the department they have decreased the number of 
#people they arrest for handgun violations so that one gun = only one arrest. Therefore, if they find 6 
#people in a car with one gun, they make a concerted effort to only arrest the individual who owns the gun. 
#While the daily_hg table appears to show some groups of kids arrested together for handgun violations,
#we cannot conclude what happens to them after that.

