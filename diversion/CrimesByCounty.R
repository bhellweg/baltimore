library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)

offense2002_2010 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2002_2010.xlsx")
offense2011_2019 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2011_2019.xlsx")

all_offense <- rbind(offense2002_2010, offense2011_2019)

yroffense <- all_offense %>%
  filter(year(OFFENSE_TYPE) > '2002-01-01') %>%
  group_by(COUNTY, OFFENSE_DATE, year(OFFENSE_DATE)) %>%
  summarize(count = n())

#Error: Invalid input: date_trans works with objects of class Date only
#I want it to group by year but I can't get that to work...
countycrimesperyear <- ggplot(yroffense, aes(x=year(OFFENSE_DATE), y=count)) +
  geom_tile() + 
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests per Year", 
       title = "Annual Arrests in MD Counties") +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~ COUNTY, nrow = 6) +
  ylim(0,NA)

countycrimesperyear
