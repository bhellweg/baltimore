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
all_offense$OFFENSE_DATE <- as.Date(all_offense$OFFENSE_DATE)
all_offense$year <- year(all_offense$OFFENSE_DATE)

#Filter by individual (only see their most severe case)
onecase <- all_offense %>%
  group_by(REVLEGALINCIDENT_KEY) %>%
  top_n(-1,FINAL_RANK)

#get number of offenses per year by county
yroffense <- onecase %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

#graph number of offenses by county
countycrimesperyear <- ggplot(yroffense, aes(x=year(year) , y=count)) +
  geom_tile() + 
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2005, 2019)) +
  labs(x = "Date of Arrest", y = "Number of Arrests per Year", 
       title = "Annual Arrests in MD Counties") +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)

countycrimesperyear
