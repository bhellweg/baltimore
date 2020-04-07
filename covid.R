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

