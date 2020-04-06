library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)
library(odbc)
library(DBI)
library(rdd)

con <- dbConnect(odbc::odbc(), "BALTIMORE", timeout = 10)
data <- dbReadTable(con, SQL("CitiStat.dbo.Police911CallsForService"))

#create function for removing near-duplicates
concat <- function(arg1,arg2,arg3){
  paste(c(arg1,arg2,arg3),collapse = " ")
}

cases <- function(arg1){
  cfs %>% 
    filter(arg1 == .$`Incident.Street.Address`) %>% 
    nrow()
}

allcfs <- data %>%
  # dplyr::filter(Description == 'BURGLARY'| Description == 'SILENT ALARM') %>% 
  filter(`Call.Date` > '2020-01-01',
         `Call.Date` < today()) %>%
  dplyr::select(`Call.Date`,
                `Call.Time`,
                `Day.of.Week`,
                District,
                Description,
                `Incident.Street.Address`,
                Latitude,
                Longitude,
                Neighborhood,
                Council.District) %>% 
  mutate(id = mapply(concat,
                     arg1 = `Call.Date`,
                     arg2 = Description,
                     arg3 = `Incident.Street.Address`)) %>%
  # filter(duplicated(id) == F) %>%
  mutate(cases = sapply(`Incident.Street.Address`,FUN = cases)) %>%
  mutate(popup = paste(     "<br>", "<b>Description: </b>", Description,
                            "<br>", "<b>Day of week: </b>", `Day.of.Week`,
                            "<br>", "<b>Date: </b>", `Call.Date`,
                            "<br>", "<b>Time: </b>", `Call.Time`,
                            "<br>", "<b>PD district: </b>", District,
                            "<br>", "<b>Address: </b>", `Incident.Street.Address`,
                            "<br>", "<b>Neighborhood: </b>", Neighborhood))

# Burglaries and Silent Alarms Analysis
bsacfs <-  allcfs %>% 
            filter(Description == 'BURGLARY'| 
             Description == 'SILENT ALARM') %>% 
            filter(duplicated(id) == F)
  
# Maps of Burglaries
bmap <- leaflet(bsacfs, width = "100%") %>% 
  addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",
                   group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",
                   group = "World Imagery") %>%
  addMarkers(lng = ~bsacfs$Longitude, 
             lat = ~bsacfs$Latitude, 
             popup = bsacfs$popup, 
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
  bsacfs %>% filter(Description == 'BURGLARY') %>%
  filter(Call.Date > (as.Date('2020-3-12')-(today()-as.Date('2020-3-12'))),
         Call.Date < today()) %>%
  group_by(Date = ifelse(`Call.Date` > as.Date('2020-3-12'),
                         "After 3/12/2020",
                         "Before 3/12/2020"),
           `Day.of.Week`,
           CallTime = format(hour(hms(Call.Time)))) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(`Day.of.Week` = factor(`Day.of.Week`, 
                                level = dow_format)) %>%
  ggplot(aes(y = CallTime, 
             x = `Day.of.Week`, 
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

bsacfs %>% 
  #filter(Description == 'BURGLARY') %>%
  #filter(hour(`Call.Time`) < 5) %>%
  #filter(`Day.of.Week` == "Friday") %>%
  filter(Call.Date > (as.Date('2020-3-12') - (today() - as.Date('2020-3-12'))),
         Call.Date < today()) %>%
  group_by(District,
           `Call.Date`,
           Description) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(`Call.Date`), 
             y = as.numeric(count))) +
  geom_line(color = "blue", 
            size = 1) +
  geom_smooth(color = "black", 
              size = 1) +
  labs(x = "Date of Call", 
       y = "Number of Calls per Day", 
       title = "Daily Burglary and Silent Alarm Calls for Service") +
  facet_wrap(~ Description, 
             scales = "free_y")+
  ylim(0,NA)


# Ranking 911 call types by change before/after

startdate <- as.Date('2020-3-10')

rdcfs <- bsacfs %>% 
  filter(Description == 'SILENT ALARM') %>%
  filter(Call.Date > (startdate - (today() - startdate)),
         Call.Date < today()) %>%
  group_by(Call.Date) %>%
  summarize(y = n()) %>%
  ungroup() %>%
  mutate(x = as.numeric(as.Date(Call.Date) - startdate)) %>%
  select(-Call.Date) %>%
  as.data.frame()
 
  bw <- with(rdcfs, IKbandwidth(x, y, cutpoint = 0))
  rdd_simple <- RDestimate(y ~ x, data = rdcfs, cutpoint = 0, bw = 15)
  summary(rdd_simple)
  
  ########################### Sorting by Regression Discontinuity
  
  startdate <- as.Date('2020-3-10')
  
  filtercfs <- allcfs %>% 
    filter(Call.Date > (startdate - (today() - startdate)),
           Call.Date < today()) %>%
    group_by(Description) %>%
    summarize(y = n()) %>%
    ungroup() %>%
    filter(.$y > 500) %>%
    arrange(desc(.$y)) %>%
    as.data.frame()
  
  ardcfs <- allcfs %>% 
    filter(Description %in% filtercfs$Description) %>%
    filter(Call.Date > (startdate - (today() - startdate)),
           Call.Date < today()) %>%
    group_by(Call.Date,
             Description) %>%
    summarize(y = n()) %>%
    ungroup() %>%
    mutate(x = as.numeric(as.Date(Call.Date) - startdate)) %>%
    select(-Call.Date) %>%
    as.data.frame()

  rddapply <- function(descr, start_date,estp){
    startdate <- as.Date(start_date)
    rddapp <- allcfs %>% 
      filter(descr == Description) %>%
      filter(Call.Date > (startdate - (today() - startdate)),
             Call.Date < today()) %>%
      group_by(Call.Date,
               Description) %>%
      summarize(y = n()) %>%
      ungroup() %>%
      mutate(x = as.numeric(as.Date(Call.Date) - startdate)) %>%
      select(-Call.Date) %>%
    RDestimate(y ~ x, ., cutpoint = 0, bw = 30)  
    ifelse(estp == 0,round(rddapp[["est"]][["LATE"]],5), round(rddapp[["p"]][[1]],5))
  }
  
  rd <- rddapply("Business Check", "2020-03-18",0)
  
  filtercfs$LATE <- mapply(rddapply,descr = filtercfs$Description,start_date = '2020-3-18',0)
  filtercfs$PVAL <- mapply(rddapply,descr = filtercfs$Description,start_date = '2020-3-18',1)
  filtercfs$LATE310 <- mapply(rddapply,descr = filtercfs$Description,start_date = '2020-3-10',0)
  filtercfs$PVAL310 <- mapply(rddapply,descr = filtercfs$Description,start_date = '2020-3-10',1)
