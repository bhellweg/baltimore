library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

#create function for removing near-duplicates
concat <- function(arg1,arg2,arg3){
  paste(c(arg1,arg2,arg3),collapse = " ")
}

cases <- function(arg1){
  cfs %>% 
    filter(arg1 == .$`Incident Street Address`) %>% 
    nrow()
}

cfs <- read_excel("~/911 CFS 4.3.2020.xlsx") %>%
  dplyr::filter(Description == 'BURGLARY'| Description == 'SILENT ALARM') %>% 
  filter(`Call Date` > '2020-2-20',
         `Call Date` < today()) %>%
  dplyr::select(`Call Date`,
         `Call Time`,
         `Day of Week`,
         District,
         Description,
         `Incident Street Address`,
         Latitude,
         Longitude,
         Neighborhood,
         `Council District`) %>% 
  mutate(id = mapply(concat,
                     arg1 = `Call Date`,
                     arg2 = Description,
                     arg3 = `Incident Street Address`)) %>%
  filter(duplicated(id) == F) %>%
  mutate(cases = sapply(`Incident Street Address`,FUN = cases)) %>%
  mutate(popup = paste(     "<br>", "<b>Description: </b>", Description,
                            "<br>", "<b>Day of week: </b>", `Day of Week`,
                            "<br>", "<b>Date: </b>", `Call Date`,
                            "<br>", "<b>Time: </b>", `Call Time`,
                            "<br>", "<b>PD district: </b>", District,
                            "<br>", "<b>Address: </b>", `Incident Street Address`,
                            "<br>", "<b>Neighborhood: </b>", Neighborhood))

# Mapping burglaries and silent alarms

bmap <- leaflet(cfs, width = "100%") %>% 
  addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",
                   group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",
                   group = "World Imagery") %>%
  addMarkers(lng = ~cfs$Longitude, 
             lat = ~cfs$Latitude, 
             popup = cfs$popup, 
             clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)",
                   "World StreetMap", 
                   "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )
bmap

#Creating a Time Heat Map

dow_format <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

twplot <-
  cfs %>% filter(Description == 'BURGLARY') %>%
  filter(`Call Date` > '2020-2-20',
         `Call Date` < '2020-4-3') %>%
  group_by(Date = ifelse(`Call Date` > '2020-3-12',
                         "After 3/12/2020",
                         "Before 3/12/2020"),
           `Day of Week`,
           CallTime = format(floor_date(`Call Time`,
                                        "hours"),
                             "%H:%M")) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(`Day of Week` = factor(`Day of Week`, 
                            level = dow_format)) %>%
  ggplot(aes(y = CallTime, 
             x = `Day of Week`, 
             fill = count)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.6), 
        legend.title = element_blank(), 
        legend.position="top", 
        legend.direction="horizontal", 
        legend.key.width=unit(2, "cm"), 
        legend.key.height=unit(0.25, "cm")) +
  labs(y = "Hour of Arrest", x = "Day of Week", 
       title = "Number of Burglaries and Silent Alarms by Hour and Day of Week") +
  scale_fill_gradient(low = "white", 
                      high = "purple", 
                      labels = comma) +
  scale_x_discrete(expand = c(0,0) , 
                   position = "top") + 
  geom_text(aes(label = count), 
          size = 3) +
  facet_wrap(~Date,
             scales = "free_y")
twplot

# Chart of Burglary and Silent Alarm Calls for Service over Time

cfs %>% 
  #filter(Description == 'BURGLARY') %>%
  #filter(hour(`Call Time`) < 5) %>%
  #filter(`Day of Week` == "Friday") %>%
  filter(`Call Date` > '2020-2-20',
         `Call Date` < '2020-4-3') %>%
  group_by(District,
           `Call Date`,
           Description) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  ggplot(aes(x = `Call Date`, 
             y = count)) +
  geom_line(color = "blue", 
            size = 1) +
  geom_smooth(color = "black") +
  labs(x = "Date of Call", 
       y = "Number of Calls per Day", 
       title = "Daily Burglary and Silent Alarm Calls for Service") +
  facet_wrap(~ Description, scales = "free_y")+
  ylim(0,NA)



