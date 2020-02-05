library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

offense2002_2010 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2002_2010.xlsx")
offense2011_2019 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2011_2019.xlsx")

all_offense <- rbind(offense2002_2010, offense2011_2019)
#all_offense$OFFENSE_DATE <- as.Date(all_offense$OFFENSE_DATE)
all_offense$year <- year(all_offense$OFFENSE_DATE)

#Filter by individual (only see their most severe case)
onecase <- all_offense %>%
  group_by(REVLEGALINCIDENT_KEY) %>%
  top_n(-1,FINAL_RANK)


marijuana <- filter(onecase, grepl('< 10 grams', OFFENSE_TEXT))

#get number of offenses per year by county
#filtering for marijuana charges (possession < 10 grams decriminalized in 2015)
yroffense <- onecase %>%
  filter(year > 2001, year < 2020, !grepl('< 10 grams', OFFENSE_TEXT)) %>%
  group_by(COUNTY, year) %>%
  summarize(count = n())

#graph number of offenses by county
countycrimesperyear <- ggplot(yroffense, aes(x=year, y=count)) +
  geom_tile() + 
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_continuous(limits = c(2002, 2019)) +
  labs(x = "Date of Arrest", y = "Number of Arrests per Year", 
       title = "Annual Arrests in MD Counties") +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~ COUNTY, nrow = 6, scales = "free_y") +
  ylim(0,NA)
#display graphs
countycrimesperyear

#crime count by county
countycrimes <- spread(yroffense, year, count)

percent_change <- function(year1, year2) {
  return((year2 - year1) / year1)
}

#Percent Change Table
change <- countycrimes %>% select(COUNTY)

change$'2002-2007 (% Change)' <- round((((countycrimes$'2007' - countycrimes$'2002') / 
                                                 countycrimes$'2002') *100), digits=2)
change$'2007-2012 (% Change)' <- round((((countycrimes$'2012' - countycrimes$'2007') / 
                                                 countycrimes$'2007') *100), digits=2)
change$'2012-2017 (% Change)' <- round((((countycrimes$'2017' - countycrimes$'2012') / 
                                                 countycrimes$'2012') *100), digits=2)
change$'2015-2016 (% Change)' <- round((((countycrimes$'2016' - countycrimes$'2015') / 
                                           countycrimes$'2015') *100), digits=2)
change$'2017-2019 (% Change)' <- round((((countycrimes$'2019' - countycrimes$'2017') / 
                                                 countycrimes$'2017') *100), digits=2)
change$'Total % Change' <- round((((countycrimes$'2019' - countycrimes$'2002') / 
                                           countycrimes$'2002') *100), digits=2)


formattable(change, align = c("l", rep("c", NCOL(change) - 1)))




