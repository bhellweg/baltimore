library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

#--------------Inputting data and rearranging/sorting as needed----------------------
#Only looking at crimes committed in 2010 or later
offense1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2002_2010.xlsx")
offense1 <- filter(offense1, year(offense1$OFFENSE_DATE) > 2001)
offense2 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2011_2019.xlsx")
offense2 <- filter(offense2, year(offense2$OFFENSE_DATE) > 2009)
offense2019 <- read_excel("~/Documents/OPI/data/JulyDecember2019Offense.xlsx")
all_offense <- rbind(offense1, offense2, offense2019)

disposition1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2002_2010.xlsx")
disposition1 <- filter(disposition1, year(disposition1$COMPLAINT_DATE) > 2001)
disposition2 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2011_2019.xlsx")
disposition2 <- filter(disposition2, year(disposition2$COMPLAINT_DATE) > 2009)
disposition2019 <- read_excel("~/Documents/OPI/data/JulyDecember2019Disposition.xlsx")
all_disposition <- rbind(disposition1, disposition2, disposition2019)

no_disposition <- all_disposition %>%
  filter(is.na(all_disposition$DISPOSITION_CODE))

disp_key <- read_excel("~/Documents/OPI/data/UnlockedDJSData/COURT_DISP_DECODE.xlsx")
disp_key <- disp_key %>% rename(DISP_CATEGORY = DRG_DISP_CATEGORY)
disposition <- merge(all_disposition, disp_key, by=c("DISPOSITION_CODE", "DISPOSITION_TEXT", "DISP_CATEGORY"))

disposition <- disposition[,c(4:10, 1:3, 11, 12)]
no_disposition$DISP_RANK <- NA
#Official disposition table that we're going to want to use
all_disposition <- rbind(disposition, no_disposition)

all_disposition <- filter(all_disposition, as.Date(all_disposition$COMPLAINT_DATE) < as.Date(all_disposition$DISPOSITION_DATE))

demographics1 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Demographics_2002_2019.xlsx")
demographics2 <- read_excel("~/Documents/OPI/data/JulyDecember2019Demographics.xlsx")
demographics <- rbind(demographics1, demographics2)

all_offense$REVLEGALINCIDENT_KEY <- as.numeric(all_offense$REVLEGALINCIDENT_KEY)
all_disposition$REVLEGALINCIDENT_KEY <- as.numeric(all_disposition$REVLEGALINCIDENT_KEY)
demographics$REVLEGALINCIDENT_KEY <- as.numeric(demographics$REVLEGALINCIDENT_KEY)

#merge all tables together
all_djs <- left_join(all_offense, all_disposition, by = c("REVACTOR_ID", "REVLEGALINCIDENT_KEY", "COMPLAINT_DATE"))
#sort by disposition rank
top_disp <- all_djs %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(DISP_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()
#sort by final rank
top_djs <- top_disp %>%
  group_by(REVACTOR_ID, REVLEGALINCIDENT_KEY, COMPLAINT_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()
#might be some duplicates with the additional demographic data that was added
top_djs <- left_join(top_djs,demographics, by=c("REVACTOR_ID","REVLEGALINCIDENT_KEY")) 

#Filter for DJS data we want
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) > 2005)
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) < 2020)
#---------------------------Completed DJS table------------------------------
#Filtered for just Baltimore because the entire dataset took too long
multiple_charge <- top_djs %>% 
  filter(COUNTY == "Baltimore City") %>%
  group_by(REVACTOR_ID, OFFENSE_DATE) %>%
  arrange(FINAL_RANK) %>%
  filter(row_number() == 1) %>%
  ungroup()

num_arrests <- multiple_charge %>%
  group_by(REVACTOR_ID) %>%
  summarize(count = n()) %>%
  ungroup()
num_arrests <- rename(num_arrests, totalarrests = count)

djs <- merge(multiple_charge, num_arrests, by="REVACTOR_ID")
djs <- djs %>%
  group_by(REVACTOR_ID) %>%
  arrange(OFFENSE_DATE)

#Assign an arrest number (sequence) to each arrest for each person
order <- function(arg1, arg2) {
  djs %>% filter(arg1 == REVACTOR_ID) %>%
    filter(COMPLAINT_DATE.x < as.Date(arg2)) %>%
    nrow()+1
}

djs$arrestnum <- mapply(FUN = order, djs$REVACTOR_ID, djs$COMPLAINT_DATE.x)

#We don't want to use data where someone committed a crime resulting in probation before 2005
#and they violated it after 2005 because we can't track days since offense
clean_djs <- djs %>% filter(!(OFFENSE_CODE == "VIOP" && arrestnum == 1))


recidivism <- function(category, criteria) {
  if (category == "violation") {
    filtered_table <- clean_djs %>%
      filter(grepl(criteria, OFFENSE_TEXT))
    viol <- criteria
    graphname <- paste("Violation:", viol, "- Probability of reoffense based on interaction outcome")
  } else if (category == "age") {
    filtered_table <- clean_djs %>%
      filter(AGE_COMPLAINT == criteria)
    a <- criteria
    graphname <- paste("Age:", a, "- Probability of reoffense based on interaction outcome")
  } else if (category == "county") {
    filtered_table <- clean_djs %>%
      filter(COUNTY == criteria)
    c <- criteria
    graphname <- paste("County:", c, "- Probability of reoffense based on interaction outcome")
  } else if (category == "charge") {
    filtered_table <- clean_djs %>%
      filter(OFFENSE_TYPE == criteria)
    ch <- criteria
    graphname <- paste("Charge:", ch, "- Probability of reoffense based on interaction outcome")
  } else {
    filtered_table <- clean_djs
    graphname <- paste("All criteria - Probability of reoffense based on interaction outcome")
  }
  
  #Resolved at Intake
  crai <- filtered_table %>%
    filter(DETNDECIDE_DEC == "Resolved/No Jurisdiction") %>%
    group_by(REVACTOR_ID) %>%
    mutate(firstcrime = ifelse(row_number() == 1, 1, 0))
  
  first_crai <- crai %>%
    filter(firstcrime == 1) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x) %>%
    rename(firstdate = COMPLAINT_DATE.x)
  
  crai_djs <- left_join(clean_djs, first_crai)
  
  crai_reoffend <- crai_djs %>% mutate(reoffense = ifelse(COMPLAINT_DATE.x > firstdate, 1, 0))
  crai_reoffenders <- crai_reoffend %>%
    filter(reoffense == 1) %>%
    group_by(REVACTOR_ID) %>%
    summarise(count = n())
  
  percent_crai_reoffense <- nrow(crai_reoffenders)/nrow(first_crai)
  #------------------------end resolved at intake-----------------
  
  #Informaled
  informaled <- filtered_table %>%
    filter(DETNDECIDE_DEC == "Informaled") %>%
    group_by(REVACTOR_ID) %>%
    mutate(firstcrime = ifelse(row_number() == 1, 1, 0))
  
  first_informaled <- informaled %>%
    filter(firstcrime == 1) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x) %>%
    rename(firstdate = COMPLAINT_DATE.x)
  
  informaled_djs <- left_join(clean_djs, first_informaled)
  
  informaled_reoffend <- informaled_djs %>% mutate(reoffense = ifelse(COMPLAINT_DATE.x > firstdate, 1, 0))
  informaled_reoffenders <- informaled_reoffend %>%
    filter(reoffense == 1) %>%
    group_by(REVACTOR_ID) %>%
    summarise(count = n())
  
  percent_informaled_reoffense <- nrow(informaled_reoffenders)/nrow(first_informaled)
  #------------------end informaled-------------------------------
  
  #Sustained
  sus <- filtered_table %>%
    filter(ADJ_DECISION_CODE == "S") %>%
    group_by(REVACTOR_ID) %>%
    arrange(COMPLAINT_DATE.x) %>%
    mutate(firstcrime = ifelse(row_number() == 1, 1, 0))
  
  first_sus <- sus %>%
    filter(firstcrime == 1) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x) %>%
    rename(firstdate = COMPLAINT_DATE.x)
  
  sus_djs <- left_join(clean_djs, first_sus)
  
  sus_reoffend <- sus_djs %>% mutate(reoffense = ifelse(COMPLAINT_DATE.x > firstdate, 1, 0))
  sus_reoffenders <- sus_reoffend %>%
    filter(reoffense == 1) %>%
    group_by(REVACTOR_ID) %>%
    summarise(count = n())
  
  percent_sustained_reoffense <- nrow(sus_reoffenders)/nrow(first_sus)
  #---------------------end sustained------------------------
  
  #Unsustained
  unsus <- filtered_table %>%
    filter(ADJ_DECISION_CODE == "U") %>%
    group_by(REVACTOR_ID) %>%
    arrange(COMPLAINT_DATE.x) %>%
    mutate(firstcrime = ifelse(row_number() == 1, 1, 0))
  
  first_unsus <- unsus %>%
    filter(firstcrime == 1) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x) %>%
    rename(firstdate = COMPLAINT_DATE.x)
  
  unsus_djs <- left_join(clean_djs, first_unsus)
  
  unsus_reoffend <- unsus_djs %>% mutate(reoffense = ifelse(COMPLAINT_DATE.x > firstdate, 1, 0))
  unsus_reoffenders <- unsus_reoffend %>%
    filter(reoffense == 1) %>%
    group_by(REVACTOR_ID) %>%
    summarise(count = n())
  
  percent_unsustained_reoffense <- nrow(unsus_reoffenders)/nrow(first_unsus)
  #--------------end unsustained----------------------
  
  #Probation
  prob <- filtered_table %>%
    filter(ADJ_DECISION_CODE == "S") %>%
    filter(grepl("Probation|Supervision", DISPOSITION_TEXT)) %>%
    group_by(REVACTOR_ID) %>%
    mutate(firstcrime = ifelse(row_number() == 1, 1, 0))
  
  first_prob <- prob %>%
    filter(firstcrime == 1) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x) %>%
    rename(firstdate = COMPLAINT_DATE.x)
  
  prob_djs <- left_join(clean_djs, first_prob)
  
  prob_reoffend <- prob_djs %>% mutate(reoffense = ifelse(COMPLAINT_DATE.x > firstdate, 1, 0))
  prob_reoffenders <- prob_reoffend %>%
    filter(reoffense == 1) %>%
    group_by(REVACTOR_ID) %>%
    summarise(count = n())
  
  percent_probation_reoffense <- nrow(prob_reoffenders)/nrow(first_prob)
  #-------------------end probation---------------------------
  
  #Committed
  commit <- filtered_table %>%
    filter(ADJ_DECISION_CODE == "S") %>%
    filter(grepl("Commit", DISPOSITION_TEXT)) %>%
    group_by(REVACTOR_ID) %>%
    mutate(firstcrime = ifelse(row_number() == 1, 1, 0))
  
  first_commit <- commit %>%
    filter(firstcrime == 1) %>%
    select(REVACTOR_ID, COMPLAINT_DATE.x) %>%
    rename(firstdate = COMPLAINT_DATE.x)
  
  commit_djs <- left_join(clean_djs, first_commit)
  
  commit_reoffend <- commit_djs %>% mutate(reoffense = ifelse(COMPLAINT_DATE.x > firstdate, 1, 0))
  commit_reoffenders <- commit_reoffend %>%
    filter(reoffense == 1) %>%
    group_by(REVACTOR_ID) %>%
    summarise(count = n())
  
  percent_commit_reoffense <- nrow(commit_reoffenders)/nrow(first_commit)
  #----------end commit--------------------
  
  reoffense <- data.frame("ResolvedAtIntake"=percent_crai_reoffense, 
                          "Informaled" = percent_informaled_reoffense, 
                          "Sustained" = percent_sustained_reoffense,
                          "Unsustained" = percent_unsustained_reoffense,
                          "Probation" = percent_probation_reoffense,
                          "Commitment" = percent_commit_reoffense)
  reoffense_table <- gather(reoffense, "Outcome", "Probability")
  
  reoffense_graph <- ggplot(reoffense_table, aes(x=reoffense_table$Outcome, y=reoffense_table$Probability)) +
    geom_bar(color="black", fill="lightblue", stat = "identity") + 
    scale_x_discrete(limits=c("ResolvedAtIntake", "Informaled", "Unsustained", 
                              "Sustained", "Probation", "Commitment"), na.translate = TRUE, na.value = 0) +
    labs(x="Outcome", y="Probability of reoffense", title=graphname)
  reoffense_graph
}

#Possible options for recidivism function
#filters: violation, age, charge, county or all
#violation: select the offense_text that you want to see (must match spelling/capitalization)
#age: 10-17 
#charge: Felony or Misdemeanor (must be capitalized)
#county: currently only deals with Baltimore City but could be expanded to other counties

recidivism("violation", "Murder 1st Degree")

