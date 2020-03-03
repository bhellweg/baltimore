## Allie's Request:
#	Breakdown of kids arrested each year by age of all ages under 14
#	Some overview of data of kids who have had repeated contact with this system over the years, 
#  and what this interaction with the system has looked like for them
#	Adding layers to time gradient chart to overlay other items like where/when violent crime is happening
#	Redoing the time gradient chart only showing data since 2016 
#  (i.e. starting when the # of school arrests significantly went down) to see how this changes the chart

library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(scales)
library(janitor)
library(anchors)
library(ggalluvial)
library(alluvial)

#Loading Relevant Files for BPD Arrests
census2010 <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Neighborhood 2010 Census Data.xlsx")
crime <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Part One Crime 2010 to 2020.xlsx")
adultarrests <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Adult Arrests 2012 to 2020.xlsx")

arrests <- read_excel("C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/BPD Youth Arrests.xlsx")
arrests$arresttime <- format(arrests$arresttime, "%H%:%M%:%S")
arrests$arrestdate <- arrests$arrestdate %>% as.Date("%m/%d/%Y", tz="EST")
arrests$month <- floor_date(arrests$arrestdate, unit = "months")
arrests$year <- floor_date(arrests$arrestdate, unit = "years")
arrests$hour <- substr(arrests$arresttime, 1,2)

#Clean Charge Types
arrests$chargedesc <- 
  unlist(lapply(arrests$chargedesc, 
                function(x){replace(x, 
                                    x == "HGV","HANDGUN VIOLATION")}))
arrests <- as.data.frame(arrests)

#Creating Relevant Tables

yarrests <- arrests %>% filter(age<18)
juvarrests <- arrests %>% filter(source == "JUV ARREST")
cbifarrests <- arrests %>% filter(source == "CBIF ARREST")
asadults <- arrests %>% filter(age<18, source == "CBIF ARREST")

# Loading relevant files for DJS Arrests

#Load relevant files. Warning: this is a lot of information. The file size is listed after each sheet.Total ~250MB
disp1 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Disposition_2002_2010.xlsx") #39MB
disp2 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Disposition_2011_2019.xlsx") #23MB
off1 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Offense_2002_2010.xlsx") #80MB
off2 <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Offense_2011_2019.xlsx") #52MB
demo <- read_excel(
  "C:/Users/brendan.hellweg/Desktop/Youth Arrests Analysis/Data/Djs01242020/Demographics_2002_2019.xlsx") #42MB

#Merge the two large tables. This may also take a little while.

disp <- rbind(disp1,disp2)
off  <- rbind(off1,off2)

#Set the keys as numerics
off$REVLEGALINCIDENT_KEY <- as.numeric(off$REVLEGALINCIDENT_KEY)
disp$REVLEGALINCIDENT_KEY <- as.numeric(disp$REVLEGALINCIDENT_KEY)
demo$REVLEGALINCIDENT_KEY <- as.numeric(demo$REVLEGALINCIDENT_KEY)

#Filter for most severe offense to get unique keys
top_offense <- off %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()

#merge all tables together
offdisp <- left_join(all_offense, all_disposition, by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
all_djs <- left_join(top_offense, disp, by = c("REVACTOR_ID", "REVLEGALINCIDENT_KEY", "COMPLAINT_DATE"))
top_djs<- all_djs %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()

#might be some duplicates with the additional demographic data that was added
top_djs <- left_join(top_djs,demo, by=c("REVACTOR_ID","REVLEGALINCIDENT_KEY","COMPLAINT_DATE")) 
top_djs$month <- as.Date(floor_date(top_djs$COMPLAINT_DATE, unit = "months"))
top_djs$year <-  as.Date(floor_date(top_djs$COMPLAINT_DATE, unit = "years"))

#Creating new columns
all_djs$month <- as.Date(floor_date(all_djs$COMPLAINT_DATE, unit = "months"))
top_djs$informaled <- if_else(top_djs$DETNDECIDE_DEC=="Informaled",1,0,0)
top_djs$guilty <- if_else(top_djs$ADJ_DECISION_CODE == "U",0,1,0)

#Create Baltimore tables
baltall   <- top_djs %>% filter(COUNTY == "Baltimore City")
notbalt   <- top_djs %>% filter(COUNTY != "Baltimore City")
baltall$month <- as.Date(floor_date(baltall$COMPLAINT_DATE.x, unit = "months"))
trials  <- top_djs %>% filter(ADJ_DECISION_CODE == "U" | ADJ_DECISION_CODE == "S") %>%
  mutate(guilty = if_else(ADJ_DECISION_CODE == "U",0,1,0)) 


#Assign a number for each arrest
order <- function(arg1,arg2) {
  top_djs %>% filter(arg1 == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE < as.Date(arg2)) %>%
    nrow()+1
}

top_djs$Order <- mapply(FUN = order,top_djs$REVACTOR_ID,top_djs$COMPLAINT_DATE)

#	Breakdown of kids arrested each year by age of all ages under 14

under14 <- top_djs %>% 
  filter(AGE_COMPLAINT < 14) %>%
  filter(AGE_COMPLAINT > 7) %>%
  filter(COMPLAINT_DATE > "2015-1-1") %>%
  group_by(Age = factor(AGE_COMPLAINT),
           Offense = OFFENSE_TEXT,
           Severity = FINAL_RANK,
           Type = OFFENSE_TYPE,
           Year = year,
           County = COUNTY) %>%
  summarise(Count = n()) %>%
  arrange(Age,
          Offense,
          Severity,
          Year,
          Type,
          County,
          desc(Count)) %>%
  filter(County == "Baltimore City")
  
under14 %>% ggplot(.,aes(x = "",y = Count, fill = Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y",start = 0) +
  labs(title = "Distribution of Offense Types for Youth under 14",
       subtitle = "Data from 2015 to 2020")

under14 %>% ggplot(.,aes(x = Age,y = Count, fill = Type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Distribution of Offense Types by Age under 14",
       subtitle = "Data from 2015 to 2020")

allyouth <- top_djs %>% 
  filter(AGE_COMPLAINT < 18) %>%
  filter(AGE_COMPLAINT > 7) %>%
  filter(COMPLAINT_DATE > "2010-1-1") %>%
  filter(Order == 1) %>%
  group_by(Age = factor(AGE_COMPLAINT),
           Offense = OFFENSE_TEXT,
           Severity = FINAL_RANK,
           Type = OFFENSE_TYPE,
           Year = year,
           Sustained = factor(guilty),
           County = COUNTY) %>%
  summarise(Count = n()) %>%
  arrange(Age,
          Offense,
          Severity,
          Year,
          Type,
          Sustained,
          County,
          desc(Count)) %>%
  filter(County == "Baltimore City") %>%
 ggplot(aes(x = Age,
                       y = Severity,
                       fill = Sustained)) + 
  ggtitle("Arrest Severity Violin Chart by Age",
          subtitle = "Data from 2010 to present and sorted by age of youth") +
  geom_violin(position = "dodge") +
  scale_y_reverse() 

allyouth

allsev <- top_djs %>% 
  filter(AGE_COMPLAINT < 18) %>%
  filter(AGE_COMPLAINT > 7) %>%
  filter(COMPLAINT_DATE > "2015-1-1") %>%
  filter(Order < 7) %>%
  group_by(Arrests = factor(Order),
           Offense = OFFENSE_TEXT,
           Severity = FINAL_RANK,
           Type = OFFENSE_TYPE,
           Sustained = factor(guilty),
           Year = year,
           County = COUNTY) %>%
  summarise(Count = n()) %>%
  arrange(Arrests,
          Offense,
          Severity,
          Year,
          Sustained,
          Type,
          County,
          desc(Count)) %>%
  filter(County == "Baltimore City") %>%
  ggplot(aes(x = Arrests,
             y = Severity,
             fill = Sustained)) + 
  ggtitle("Arrest Severity Violin Chart by Number of Arrests",
          subtitle = "Data is from 2010 to 2020 and is sorted by number of arrests for an individual") +
  geom_violin(position = "dodge") +
  scale_y_reverse()
allsev

#	Some overview of data of kids who have had repeated contact with this system over the years, 
#  and what this interaction with the system has looked like for them

sankey <- top_djs %>% 
  filter(COUNTY == "Baltimore City") %>% 
  filter(COMPLAINT_DATE > as.Date("2018-01-01")) %>%
  group_by(#Age = AGE_COMPLAINT,
           Offense = OFFENSE_TYPE,
           Description = OFF_SEV_TXT,
           Decision = DECISIONINTK_TEXT,
           Sustained = factor(guilty),
           Outcome = DISP_CATEGORY) %>%
  summarise(Frequency = n()) %>% 
  filter(Frequency > 1) %>% 
  replace_na(list(Outcome = "Nothing")) %>%
  ggplot(aes(y = Frequency, 
             #axis1 = Age,
             axis2 = Offense,
             axis3 = Description,
             axis4 = Decision,
             axis5 = Outcome)) +
  geom_alluvium(aes(fill = Sustained)) +
  geom_stratum(width = 1/8, 
               fill = "black", 
               color = "grey") +
  geom_label(stat = "stratum", 
             infer.label = T, 
             size = 3) +
  scale_fill_brewer(type = "qual", 
                    palette = "Set1") +
  scale_x_continuous(breaks = 1:4,
                     labels = c(#"Age",
                                "Offense",
                                "Description",
                                "Decision",
                                "Outcome")) +
  ggtitle("Sankey Diagram of DJS Decisions around Baltimore Youth",
          subtitle = "Data from 2018 to 2020 for Baltimore City Arrests, Filtered to Outcome Paths with More than 10 Occurrances")

sankey


#	Adding layers to time gradient chart to overlay other items like 
#  where/when violent crime is happening
agerank <- yarrests %>%
  filter(.,year > '2016-01-01') %>%
  filter(.,arresttime != "00:00:00") %>%
  group_by(age) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

agetime <- yarrests %>%
  filter(.,year > '2016-01-01') %>%
  filter(.,arresttime != "00:00:00") %>%
  filter(.,age %in% agerank$age[1:4]) %>%
  group_by(age, dayofweek, hour) %>% 
  summarize(.,Count = n())

dow_format <- c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
agetime$dayofweek <- factor(agetime$dayofweek, level = rev(dow_format)) 

aplot <- ggplot(agetime, aes(x = reorder(dayofweek,desc(dayofweek)), 
                             y = reorder(hour,desc(hour)), fill = Count)) +
  geom_raster(interpolate = T) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6, size = 5)) +
  labs(y = "Hour of Arrest", x = "Day of Week of Arrest", 
       title = "Number of Youth Arrests 2016 to 2020 by Age of Youth") +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(position = "top") +
  facet_grid(age ~ .)
aplot

# Crimes involving Cars 
carcrimes <- yarrests %>%
  filter(., year > '2015-01-01') %>%
  filter(., str_detect(chargedesc,pattern = 
                         " CAR |CARJ|MOTOR|MOTR|MOTER|VEHICLE|VEH")) %>%
  filter(., !str_detect(chargedesc, pattern = "HOMICIDE|MANSLAUGHTER|STRIKE")) %>%
  filter(arresttime != "00:00:00") %>%
  group_by(dayofweek, hour) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(dayofweek = factor(dayofweek, 
                            level = rev(dow_format))) %>%
  ggplot(., aes(y = reorder(hour,desc(hour)), 
                x = reorder(dayofweek,desc(dayofweek)), 
                fill = count)) +
  geom_raster(aes(fill = count), 
              interpolate = T) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6), 
        legend.title = element_blank(), 
        legend.position="bottom", 
        legend.direction="horizontal", 
        legend.key.width=unit(2, "cm"), 
        legend.key.height=unit(0.25, "cm")) +
  labs(y = "Hour of Arrest", 
       x = "Day of Week", 
       title = "Number of Youth Arrests from 2015 – 2020 by Time of Arrest",
       subtitle = "Filtered to Crimes Involving Vehicles") +
  scale_x_discrete(expand = c(0,0) , 
                   position = "top") +
  scale_fill_gradient(low = "white", 
                      high = "#27AE60", 
                      labels = comma)
  carcrimes
  
# Youth Charged as Adults
  
  adultcharges <- yarrests %>%
    filter(., year > '2015-01-01') %>%
    filter(., source == "CBIF ARREST") %>%
    filter(., str_detect(chargedesc,pattern = 
                           " CAR |CARJ|MOTOR|MOTR|MOTER|VEHICLE|VEH")) %>%
    #filter(., !str_detect(chargedesc, pattern = "HOMICIDE|MANSLAUGHTER|STRIKE")) %>%
    filter(arresttime != "00:00:00") %>%
    group_by(dayofweek, hour) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(dayofweek = factor(dayofweek, 
                              level = rev(dow_format))) %>%
    ggplot(., aes(y = reorder(hour,desc(hour)), 
                  x = reorder(dayofweek,desc(dayofweek)), 
                  fill = count)) +
    geom_raster(aes(fill = count), 
                interpolate = T) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.6), 
          legend.title = element_blank(), 
          legend.position="bottom", 
          legend.direction="horizontal", 
          legend.key.width=unit(2, "cm"), 
          legend.key.height=unit(0.25, "cm")) +
    labs(y = "Hour of Arrest", 
         x = "Day of Week", 
         title = "Number of Youth Charged as Adults from 2015 – 2020 by Time of Arrest",
         subtitle = "Filtered to Crimes Involving Vehicles") +
    scale_x_discrete(expand = c(0,0) , 
                     position = "top") +
    scale_fill_gradient(low = "white", 
                        high = "#27AE60", 
                        labels = comma)
  adultcharges
  
  #Factor By Crime Category
  topcrimes <- yarrests %>%
    filter(year > '2016-01-01') %>%
    filter(arresttime != "00:00:00") %>%
    filter(., source == "CBIF ARREST") %>%
    group_by(chargedesc) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  topcrimetime <- yarrests %>%
    filter(year > '2016-01-01') %>%
    filter(arresttime != "00:00:00") %>%
    filter(., source == "CBIF ARREST") %>%
    filter(chargedesc %in% topcrimes$chargedesc[1:5]) %>%
    group_by(chargedesc, dayofweek, hour) %>% 
    summarize(count = n())
  
  topcrimetime$dayofweek <- factor(topcrimetime$dayofweek, level = rev(dow_format))
  
  cwplot <- ggplot(topcrimetime, aes(y = reorder(hour,desc(hour)), 
                                     x = reorder(dayofweek,desc(dayofweek)), fill = count)) +
    geom_raster(aes(fill = count), interpolate = T) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.6, size = 5)) +
    labs(y = "Hour of Arrest", x = "Day of Week of Arrest", 
         title = "Number of Youth Arrests 2016 to 2020 by Time and Day of Week") +
    scale_fill_gradient(low = "white", high = "#2980B9") +
    scale_x_discrete(position = "top") +
    facet_wrap(~ chargedesc, scales = "free", as.table = T)
  cwplot
  
#	Redoing the time gradient chart only showing data since 2016 
#  (i.e. starting when the # of school arrests significantly went down) 
#  to see how this changes the chart

yarrests$hour <- substr(yarrests$arresttime, 1,2)

arresttime <- yarrests %>%
  filter(year > '2016-01-01') %>%
  filter(arresttime != "00:00:00") %>%
  group_by(dayofweek, hour) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(dayofweek = factor(dayofweek, level = rev(dow_format))) %>%
ggplot(., aes(y = reorder(hour,desc(hour)), 
              x = reorder(dayofweek,desc(dayofweek)), 
              fill = count)) +
  geom_raster(aes(fill = count), 
              interpolate = T) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6), 
        legend.title = element_blank(), 
        legend.position="bottom", 
        legend.direction="horizontal", 
        legend.key.width=unit(2, "cm"), 
        legend.key.height=unit(0.25, "cm")) +
  labs(y = "Hour of Arrest", 
       x = "Day of Week", 
       title = "Number of Youth Arrests from 2016 – 2020, by Time of Arrest") +
  scale_x_discrete(expand = c(0,0) , 
                   position = "top") +
  scale_fill_gradient(low = "white", 
                      high = "#27AE60", 
                      labels = comma)
arresttime

