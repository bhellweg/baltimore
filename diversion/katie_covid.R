library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

calls <- read_excel("~/Documents/OPI/covid/calls911.xlsx")
partonecrime <- read_excel("~/Documents/OPI/covid/partonecrime.xlsx")

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
    group_by(Description) %>%
    summarize(count = n()) %>%
    rename(total = count)
  
  graphname <- paste("Part One Crimes in", neighborhood)
  
  neighborhood_crime <- ggplot(crimes, aes(x=Description, y=total)) +
    geom_bar(stat="identity", color="black", fill="#F2CA27") + 
    labs(title = graphname, x="Crime", y="Total") +
    scale_x_discrete(labels=wrap_format(10)) +
    theme(axis.text.x = element_text(angle = 30))
  neighborhood_crime
}

neighborhood_partone("Sandtown-Winchester")

#---------------------------------------------------
burglary_alarms <- calls %>%
  filter(grepl("SILENT ALARM|BURGLARY", Description))

daily_alarms <- burglary_alarms %>% 
  group_by(`Police District`, `Call Date`) %>%
  count(`Call Date`)

burg_alarm_trend <- ggplot(daily_alarms, aes(x=daily_alarms$`Call Date`, y=n)) +
  geom_line(color = "#F2CA27", size = 0.4) +
  geom_smooth(method = "lm", level=0.9, color="black", size=0.2) +
  labs(title="Trends in Burglary/Silent Alarms (All Districts)", x="Date", y="Number of Calls") +
  facet_wrap(~ daily_alarms$`Police District`, nrow = 6, scales="free_y")
burg_alarm_trend

pre_sip <- burglary_alarms %>%
  filter(as.Date(`Call Date`) < as.Date('2020-03-16'))

post_sip <- burglary_alarms %>%
  filter(as.Date(`Call Date`) > as.Date('2020-03-16'))

pre_neighborhoods <- pre_sip %>%
  select(`Neighborhood`, `Call Date`) %>%
  count(`Neighborhood`) %>%
  rename(`Pre SIP Calls` = n) %>%
  mutate(preavgcallsperday = `Pre SIP Calls`/42)

post_neighborhoods <- post_sip %>%
  select(`Neighborhood`, `Call Date`) %>%
  count(`Neighborhood`) %>%
  rename(`Post SIP Calls` = n) %>%
  mutate(postavgcallsperday = `Post SIP Calls`/18)

neighborhoods <- left_join(pre_neighborhoods, post_neighborhoods, by="Neighborhood") %>%
  na.omit() %>%
  mutate(percentchange = (postavgcallsperday - preavgcallsperday)/postavgcallsperday * 100)

#neighborhoods that have seen an increase in average burglary/silent alarm calls
crime_increase <- neighborhoods %>%
  filter(percentchange > 0)
view(crime_increase %>% select(Neighborhood, percentchange))


#-------------Time heat map for neighborhoods with increase in crime
dow_format <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

day_of_week <- calls %>%
  filter(Description == "BURGLARY"| Description== "SILENT ALARM") %>%
  group_by(Neighborhood, `Day of Week`, 
           Date = ifelse(`Call Date` > as.Date('2020-3-16'),
                         "After 3/16/2020",
                         "Before 3/16/2020")) %>%
  summarize(count = n()) %>%
  mutate(`Day Type` = ifelse(`Day of Week` == 'Monday' |
                               `Day of Week` == 'Tuesday' |
                               `Day of Week` == 'Wednesday' |
                               `Day of Week` == 'Thursday' |
                               `Day of Week` == 'Sunday', "Weekday", "Weekend"))

neighborhood_freq <- day_of_week %>%
  filter(Neighborhood %in% crime_increase$Neighborhood) %>%
  group_by(Neighborhood, `Day Type`, Date) %>%
  summarize(count = n()) %>%
  mutate(avg = case_when(`Day Type` == "Weekday" & Date == "Before 3/16/2020" ~ count/30,
                         `Day Type` == "Weekday" & Date == "After 3/16/2020" ~ count/13,
                         `Day Type` == "Weekend" & Date == "Before 3/16/2020" ~ count/12,
                         `Day Type` == "Weekend" & Date == "After 3/16/2020" ~ count/5)) %>%
  left_join(crime_increase)

neighborhood_freq$`Pre SIP Calls` <- NULL
neighborhood_freq$`Post SIP Calls` <- NULL
neighborhood_freq$preavgcallsperday <- NULL
neighborhood_freq$postavgcallsperday <- NULL


twplot <- neighborhood_freq %>% 
  filter(percentchange > 25) %>%
  ggplot(aes(y = Neighborhood, 
             x = `Day Type`)) +
  geom_tile(aes(fill = avg), na.rm = FALSE) +  
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
  geom_text(aes(label = round(avg, digits=3)), 
            size = 3) +
  facet_wrap(~ Date)
twplot




