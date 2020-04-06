#Context: What is the broader context of youth arrests in Baltimore? Has this changed over time?
#
#  1. What proportion of young people in Baltimore are arrested for a felony or
#     misdemeanor before 18? How has this changed over time?
#  2. How do Baltimore youth arrests compare to similar cities (near and far)?
#  3. How does Baltimore compare to similar cities in youth diversion events?
#  4. What neighborhoods have the most felonies and misdemeanors?
#  5. What is the relationship between youth arrests and overall indicators of  public safety 
#     (e.g., calls for service, violent and other offense reports)?
#  6. What is the cost to the city and to communities of youth arrests in Baltimore?
#
#Load Libraries
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
    nrow()+1}
top_djs$Order <- mapply(FUN = order,top_djs$REVACTOR_ID,top_djs$COMPLAINT_DATE)

#  1. What proportion of young people in Baltimore are arrested for a felony or
#     misdemeanor before 18? How has this changed over time?

birthyear <- function(date,age){as.Date(floor_date(as.Date(date),"years")-years(as.numeric(age)))}

birthyear(as.Date("2012-12-02"),14)

births <- baltall %>% mutate(birthyear = mapply(birthyear,
                                                date = as.Date(COMPLAINT_DATE),
                                                age = as.numeric(AGE_COMPLAINT))) # %>%
          #summarise(REVACTOR_ID,birthyear,OFF_SEV_TXT,DISP_CATEGORY,count = n())
          



#  2. How do Baltimore youth arrests compare to similar cities (near and far)?
#  3. How does Baltimore compare to similar cities in youth diversion events?
#  4. What neighborhoods have the most felonies and misdemeanors?
#  5. What is the relationship between youth arrests and overall indicators of  public safety 
#     (e.g., calls for service, violent and other offense reports)?
#  6. What is the cost to the city and to communities of youth arrests in Baltimore?




