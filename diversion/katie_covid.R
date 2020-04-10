library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

calls <- read_excel("~/Documents/OPI/covid/calls20200410.xlsx")
partonecrime <- read_excel("~/Documents/OPI/covid/partone20200410.xlsx")

highpriority <- calls %>%
  filter(Priority == "High") %>%
  group_by(Description) %>%
  summarize(count = n())

call_type <- function(crimetype) {
  crimes <- calls %>% 
    filter(grepl(crimetype, Description)) %>%
    group_by(District) %>%
    summarize(count = n()) %>%
    rename(total = count) %>%
    ungroup()
  
  district_crime <- ggplot(crimes, aes(x=District, y=total)) +
    geom_bar(stat="identity", color="black", fill="#F2CA27") + 
    labs(title = "Call Count by Police District", x="District", y=crimetype) +
    scale_x_discrete(limits=c("CD", "CW", "ED", "ND", "NE", "NW", "SD", "SE", "SS", "TRU", "SW", "WD"),
                     na.translate = TRUE, na.value = 0)
  district_crime
}

call_type("AUTO THEFT")


neighborhood_partone <- function(neighborhood) {
  crimes <- partonecrime %>%
    filter(Neighborhood == neighborhood) %>%
    filter(`Crime Date` > (startdate - (today() - startdate)),
           `Crime Date` < today()) %>%
    group_by(Date = ifelse(`Crime Date` > as.Date(startdate),
                           "After Shutdown",
                           "Before Shutdown")) %>%
    group_by(Description, Date) %>%
    summarize(count = n())
  
  graphname <- paste("Part One Crimes in", neighborhood)
  
  neighborhood_crime <- ggplot(crimes, aes(x=Description, y=count)) +
    geom_bar(stat="identity", fill="#F2CA27") + 
    labs(title = graphname, x="Crime", y="Total") +
    scale_x_discrete(labels=wrap_format(10)) +
    theme(axis.text.x = element_text(angle = 30)) +
    facet_wrap(~Date)
  neighborhood_crime
}

neighborhood_partone("Charles North")

#----------------Burglary/Silent Alarm Trends-----------------------------------
burglary <- calls %>%
  filter(grepl("BURGLARY", Description))

daily_burglary <- burglary %>% 
  group_by(`Police District`, `Call Date`) %>%
  count(`Call Date`) %>%
  na.omit()

burg_trend <- ggplot(daily_burglary, aes(x=as.Date(daily_burglary$`Call Date`), y=n)) +
  geom_line(color = "#F2CA27", size = 0.4, stat="identity") +
  geom_smooth(method = "loess", level=0.9, color="black", size=0.2) +
  labs(title="Trends in burglary across districts", x="Date", y="Number of Calls") +
  facet_wrap(~ daily_burglary$`Police District`, nrow = 3, scales="free_y")
burg_trend

alarms <- calls %>%
  filter(grepl("SILENT ALARM", Description))

daily_alarms <- alarms %>% 
  group_by(`Police District`, `Call Date`) %>%
  count(`Call Date`) %>%
  na.omit()

alarm_trend <- ggplot(daily_alarms, aes(x=as.Date(daily_alarms$`Call Date`), y=n)) +
  geom_line(color = "#F2CA27", size = 0.4) +
  geom_smooth(method = "loess", level=0.9, color="black", size=0.2) +
  labs(title="Trends in silent alarms across districts", x="Date", y="Number of Calls") +
  facet_wrap(~ daily_alarms$`Police District`, nrow = 3, scales="free_y")
alarm_trend


#------Finding percent change for neighborhoods with increased-----
#------burglary/silent alarm calls---------------------------------

startdate <- as.Date('2020-3-18')

burglary_alarms <- calls %>%
  filter(grepl("BURGLARY|SILENT ALARM", Description)) %>%
  filter(`Call Date` > (startdate - (today() - startdate)),
       `Call Date` < today()) %>%
  group_by(Date = ifelse(`Call Date` > as.Date(startdate),
                       "After Shutdown",
                       "Before Shutdown"))

pre_neighborhoods <- burglary_alarms %>%
  filter(Date == "Before Shutdown") %>%
  count(`Neighborhood`) %>%
  rename(`Pre Shutdown Calls` = n) %>%
  mutate(preavgcallsperday = `Pre Shutdown Calls`/as.numeric((today()-startdate)))

post_neighborhoods <- burglary_alarms %>%
  filter(Date == "After Shutdown") %>%
  count(`Neighborhood`) %>%
  rename(`Post Shutdown Calls` = n) %>%
  mutate(postavgcallsperday = `Post Shutdown Calls`/as.numeric((today()-startdate)))

neighborhoods <- merge(pre_neighborhoods, post_neighborhoods, by="Neighborhood") %>%
  na.omit() %>%
  mutate(percentchange = (`Post Shutdown Calls` - `Pre Shutdown Calls`)/`Post Shutdown Calls` * 100)

#neighborhoods that have seen an increase in average burglary/silent alarm calls
crime_increase <- neighborhoods %>%
  filter(percentchange > 0) %>%
  select(Neighborhood, `Pre Shutdown Calls`, `Post Shutdown Calls`, percentchange)


#-------------Heat map for neighborhoods with increase in burglary/silent alarm calls
dow_format <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

day_of_week <- burglary_alarms %>%
  mutate(`Day Type` = ifelse(`Day of Week` == 'Friday' |
                               `Day of Week` == 'Saturday', 
                             "Weekend", "Weekday")) %>%
  group_by(Neighborhood, Date, `Day Type`) %>%
  summarize(count = n())
  

#If updating the graph, make sure to change the number you divide count by to
#be the accurate number of weekdays/weekend before shutdown

neighborhood_freq <- day_of_week %>%
  filter(Neighborhood %in% crime_increase$Neighborhood) %>%
  left_join(crime_increase)

neighborhood_freq$`Pre Shutdown Calls` <- NULL
neighborhood_freq$`Post Shutdown Calls` <- NULL


twplot <- neighborhood_freq %>% 
  filter(percentchange > 25) %>%
  ggplot(aes(y = Neighborhood, 
             x = `Day Type`)) +
  geom_tile(aes(fill = count), na.rm=FALSE) +
  scale_fill_gradient(low = "white", 
                      high = "purple",
                      na.value = "grey50",
                      guide="colorbar") +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.6), 
        legend.title = element_blank(), 
        legend.position="top", 
        legend.direction="horizontal", 
        legend.key.width=unit(2, "cm"), 
        legend.key.height=unit(0.25, "cm")) +
  labs(y = "Hour of Arrest", x = "Day of Week", 
       title = "Average Number of Burglaries/Silent Alarm Calls") +
  scale_x_discrete(expand = c(0,0) , 
                   position = "top") + 
  geom_text(aes(label=count), size=3) +
  facet_wrap(~ Date)
twplot




