library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(scales)

#Loading Relevant Files

arrests <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/BPD Youth Arrests.xlsx")
census2010 <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Neighborhood 2010 Census Data.xlsx")
crime <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Part One Crime 2010 to 2020.xlsx")
adultarrests <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Adult Arrests 2012 to 2020.xlsx")
arrests$arresttime <- format(arrests$arresttime, "%H%:%M%:%S")
arrests$arrestdate <- arrests$arrestdate %>% as.Date("%m/%d/%Y", tz="EST")
arrests$month <- floor_date(arrests$arrestdate, unit = "months")
arrests$year <- floor_date(arrests$arrestdate, unit = "years")

#Creating Relevant Tables

yarrests <- arrests %>% filter(age<18)
juvarrests <- arrests %>% filter(source == "JUV ARREST")
cbifarrests <- arrests %>% filter(source == "CBIF ARREST")
asadults <- arrests %>% filter(age<18, source == "CBIF ARREST")
datetable <- as.data.frame(seq.Date(as.Date("2010/01/01"),as.Date("2020/01/01"),"days"))

#Checking the percent null for each table

pnull <- function(db){
round(100*(colSums(is.na(db))/nrow(db)),2)
}

percentnull <- as.data.frame(
rbind(
  c("arrests",pnull(arrests)),
  c("yarrests",pnull(yarrests)),
  c("juvarrests",pnull(juvarrests)),
  c("cbifarrests",pnull(cbifarrests)),
  c("asadults",pnull(asadults))))

View(percentnull)

aggregate(district ~ year, data=yarrests, function(district) 
  {round(sum(is.na(district))/sum(is.na(district)|!is.na(district)),2)}, na.action = NULL)

#Map Youth Arrests

yarrests$popup <- paste("<b>Incident #: </b>", yarrests$incidentnum, 
                    "<br>", "<b>Description: </b>", yarrests$chargedesc,
                    "<br>", "<b>Age: </b>", yarrests$age,
                    "<br>", "<b>Race: </b>", yarrests$race,
                    "<br>", "<b>Sex: </b>", yarrests$sex,
                    "<br>", "<b>Day of week: </b>", yarrests$dayofweek,
                    "<br>", "<b>Date: </b>", yarrests$arrestdate,
                    "<br>", "<b>Time: </b>", yarrests$arresttime,
                    "<br>", "<b>PD district: </b>", yarrests$district,
                    "<br>", "<b>Address: </b>", yarrests$arrestlocation,
                    "<br>", "<b>Neighborhood: </b>", yarrests$nhood)

yarmap <- leaflet(yarrests, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  addMarkers(lng = ~yarrests$long, lat = ~yarrests$lat, popup = yarrests$popup, 
             clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )
yarmap

#Graph Youth Arrests by Day

yarrests_daily <- yarrests %>%
  count(month)

ydplot <- ggplot(yarrests_daily, aes(x = yarrests_daily$month, y = n)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests", 
       title = "Monthly Youth Arrests in Baltimore, 2010 to 2020")
ydplot

#Graph Youth Charged as Adults by Day

asadults_daily <- asadults %>%
  count(month)

asadplot <- ggplot(asadults_daily, aes(x = asadults_daily$month, y = n)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(method = "loess", span = .2) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests", 
       title = "Monthly Youth Charged As Adult Arrests in Baltimore, 2010 to 2020")
asadplot

#Graph Youth Charged as Juveniles by Day

juvarrests_daily <- juvarrests %>%
  count(month)

juvdplot <- ggplot(juvarrests_daily, aes(x = juvarrests_daily$month, y = n)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(method = "loess", span = .2) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests", 
       title = "Monthly Youth Juvenile Charges in Baltimore, 2010 to 2020")
juvdplot

#Creating a Time Heat Map

yarrests$hour <- substr(yarrests$arresttime, 1,2)

arresttime <- yarrests %>%
  group_by(dayofweek, hour) %>%
  summarize(count = n())

dow_format <- c("SUNDAY","MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY")
arresttime$dayofweek <- factor(arresttime$dayofweek, level = rev(dow_format))

twplot <- 
  ggplot(arresttime, aes(x = hour, y = dayofweek, fill = count)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), 
        legend.title = element_blank(), legend.position="top", legend.direction="horizontal", 
        legend.key.width=unit(2, "cm"), legend.key.height=unit(0.25, "cm")) +
        labs(x = "Hour of Arrest", y = "Day of Week", 
       title = "Number of Youth Arrests from 2010 – 2020, by Time of Arrest") +
  scale_fill_gradient(low = "white", high = "#27AE60", labels = comma)
twplot

#Factor By Crime Category
topcrimes <- yarrests %>%
  group_by(chargedesc) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

topcrimetime <- yarrests %>%
  filter(chargedesc %in% topcrimes$chargedesc[1:20]) %>%
  group_by(chargedesc, dayofweek, hour) %>% 
  summarize(count = n())

topcrimetime$dayofweek <- factor(topcrimetime$dayofweek, level = rev(dow_format))

cwplot <- ggplot(topcrimetime, aes(x = hour, y = dayofweek, fill = count)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest", y = "Day of Week of Arrest", 
       title = "Number of Youth Arrests 2010 to 2020 by Time and Day of Week") +
  scale_fill_gradient(low = "white", high = "#2980B9") +
  facet_wrap(~ chargedesc, nrow = 6)
cwplot

#Change in Arrest Charges over Time

topcrimes <- yarrests %>%
  filter(year > '2017-01-01') %>%
  group_by(chargedesc) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

topcrimeday <- yarrests %>%
  filter(chargedesc %in% topcrimes$chargedesc[1:30]) %>%
  group_by(chargedesc, month) %>% 
  summarize(count = n())

ctaplot <- ggplot(topcrimeday, aes(x = topcrimeday$month, y = count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests per Month", 
       title = "Monthly Arrests in Baltimore, 2010 to 2020")+
  facet_wrap(~ chargedesc, scales = "free_y")+
  ylim(0,NA)
ctaplot

#Change in Charges as Adults over Time

asatopcrimes <- asadults %>%
  group_by(chargedesc) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

asatopcrimeday <- asadults %>%
  filter(chargedesc %in% asatopcrimes$chargedesc[1:16]) %>%
  group_by(chargedesc, month) %>% 
  summarize(count = n())

actaplot <- ggplot(asatopcrimeday, aes(x = asatopcrimeday$month, y = count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests per Month", 
       title = "Monthly Charges as Adults in Baltimore, 2010 to 2020")+
  facet_wrap(~ chargedesc, scales = "free_y")+
  ylim(0,NA)

actaplot

#Change in Charges as Juveniles Over Time

juvtopcrimes <- juvarrests %>%
  filter(year > '2017-01-01') %>%
  group_by(chargedesc) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

juvtopcrimeday <- juvarrests %>%
  filter(chargedesc %in% juvtopcrimes$chargedesc[1:20]) %>%
  group_by(chargedesc, month) %>% 
  summarize(count = n())

juvaplot <- ggplot(juvtopcrimeday, aes(x = juvtopcrimeday$month, y = count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests per Month", 
       title = "Monthly Charges as Juveniles in Baltimore, 2010 to 2020")+
  facet_wrap(~ chargedesc, scales = "free_y")+
  ylim(0,NA)
juvaplot

#View Youth Arrests by District

jarrests_year <- juvarrests %>%
  group_by(district, year) %>% 
  summarize(count = n())

jmdplot <- ggplot(jarrests_year, 
                  aes(x = jarrests_year$year, y = count)) +
  geom_tile() +
  geom_line(color = "#F2CA27", size = 1) +
  scale_x_date(breaks = date_breaks("1 year"), 
               labels = date_format("%y")) +
  labs(x = "Date of Arrest", y = "Number of Arrests", 
       title = "Yearly Youth Arrests in Baltimore, 2010 to 2020") +
  facet_wrap(~ district, nrow = 6)
jmdplot

#Table of the blocks with the most youth arrests
blocks <- yarrests %>%
  group_by(arrestlocation) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

topblocks <- yarrests %>%
  filter(arrestlocation %in% blocks$arrestlocation[1:100]) %>%
  filter(!is.na(arrestlocation))%>%
  filter(!is.na(district))%>%
  group_by(arrestlocation, district) %>% 
  summarize(count = n())
View(topblocks)

charlesst <- yarrests %>%
  filter(arrestlocation == "2300 CHARLES ST")
gayst <- yarrests %>%
  filter(arrestlocation == "300 GAY ST")
harlemav <- yarrests %>%
  filter(arrestlocation == "1500 HARLEM AV")
libheights <- yarrests %>%
  filter(arrestlocation == "2300 LIBERTY HGTS AV")

View(charlesst)
View(gayst)
View(harlemav)
View(libheights)

#Youth Arrests in the last year by district
lyarrests <- yarrests %>% 
  filter(year == '2017-01-01') %>%
  group_by(district) %>%
  summarise(count = n())

View(lyarrests)

#Summary of Plots:
yarmap #Cluster map of arrests, 2010 to 2020
ydplot #Daily graph of youth arrests
asadplot #Daily graph of youth charged as adults
juvdplot #Daily graph of youth charged as juveniles
twplot #Overall heat map of arrests by time and day of week
cwplot #Tile Plot of the heat map of time and week arrests by charge
ctaplot #Tile Plot of Youth Arrests per month
juvaplot #Tile plot of Juvenile Arrests per month
actaplot #Tile Plot of Charges as Adults per month
