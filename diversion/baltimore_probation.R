library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

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
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) > 2009)
top_djs <- filter(top_djs, year(top_djs$COMPLAINT_DATE.x) < 2020)

#All sustained, probationable offenses
probation <- top_djs %>% 
  filter(top_djs$ADJ_DECISION_CODE == "S") %>%
  filter(grepl("Probation|Supervision", DISPOSITION_TEXT))
  

#COMPLAINT_DATE.x is from the offense table 
probation$year <- year(probation$COMPLAINT_DATE.x)

#Baltimore probationable offenses
baltimore_probation <- probation %>%
  filter(probation$COUNTY == "Baltimore City") %>%
  group_by(OFFENSE_TEXT) %>%
  summarize(count = n()) %>%
  arrange(count) %>%
  ungroup()

#Offenses in MD that result in probation
md_probation <- probation %>%
  group_by(OFFENSE_TEXT) %>%
  summarize(count = n()) %>%
  arrange(count)

#All commitment offenses
commit <- top_djs %>%
  filter(top_djs$ADJ_DECISION_CODE == "S") %>%
  filter(grepl("Commited|Commit", DISPOSITION_TEXT))

commit$year <- year(commit$COMPLAINT_DATE.x)

#Commitment to DJS offenses in Baltimore
baltimore_commit <- commit %>%
  filter(commit$COUNTY == "Baltimore City") %>%
  group_by(OFFENSE_TEXT) %>%
  summarize(count = n()) %>%
  arrange(count)

#Offenses in MD that result in commitment to DJS
md_commit <- commit %>%
  group_by(OFFENSE_TEXT) %>%
  summarize(count = n()) %>%
  arrange(count)

#What crimes in Baltimore result in probation?
all_unresolved <- top_djs %>%
  filter(top_djs$DECISIONINTK_TEXT != "Resolved at Intake")

unresolved <- all_unresolved %>%
  group_by(all_unresolved$DECISIONINTK_TEXT) %>%
  summarize(count = n())

resolved <- top_djs %>%
  filter(top_djs$DECISIONINTK_TEXT == "Resolved at Intake") %>%
  group_by(DECISIONINTK_TEXT) %>%
  summarize(count = n())

all_baltimore <- top_djs %>%
  filter(top_djs$COUNTY == "Baltimore City")

#Crimes that are going futher
baltimore_unresolved <- all_baltimore %>%
  filter(all_baltimore$DECISIONINTK_TEXT != "Resolved at Intake") %>%
  group_by(DECISIONINTK_TEXT) %>%
  summarize(count = n())

#Crimes that were resolved at intake (will have no disposition data)
baltimore_resolved <- all_baltimore %>%
  filter(all_baltimore$DECISIONINTK_TEXT == "Resolved at Intake") %>%
  group_by(DECISIONINTK_TEXT) %>%
  summarize(count = n())

get_commits <- function(offense, county) {
  offense_commits <- top_djs %>% 
    filter(grepl(county, top_djs$COUNTY))
  
  offense_commits <- offense_commits %>%
    filter(grepl("Commit", offense_commits$DISPOSITION_TEXT))
  
  offense_commits <- offense_commits %>%
    filter(offense == offense_commits$OFFENSE_TEXT) %>%
    group_by(OFFENSE_TEXT) %>%
    summarize(count = n())
  
  return(sum(as.numeric(offense_commits$count)))
}

probation_frequency <- function(offense_type, county) {
  if (county == "all") {
    all_accounts <- probation %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
      
    offense_table <- top_djs %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    return(sum(as.numeric(all_accounts$count/offense_table$count)))
    
  } else if (county == "no-baltimore") {
    all_accounts <- probation %>%
      filter(COUNTY != "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    
    offense_table <- top_djs %>%
      filter(COUNTY != "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    return(sum(as.numeric(all_accounts$count/offense_table$count)))
    
  } else {
    all_accounts <- probation %>%
      filter(COUNTY == "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    
    offense_table <- top_djs %>%
      filter(COUNTY == "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    return(sum(as.numeric(all_accounts$count/offense_table$count)))
    
  }
}
baltimore_outcomes <- baltimore_probation %>% select(OFFENSE_TEXT, count)
baltimore_outcomes$ProbationChance <- mapply(FUN = probation_frequency, offense_type = baltimore_outcomes$OFFENSE_TEXT, 
                                            county="baltimore")

#include Baltimore in MD calculations to understand probability of being put on probation for a given crime
all_md <- baltimore_outcomes %>% select(OFFENSE_TEXT, count)
all_md$MDProbation <- mapply(FUN = probation_frequency, offense_type = all_md$OFFENSE_TEXT, 
                             county="all")

#exclude Baltimore in calculations to understand being put on probation for a given crime
no_baltimore <- baltimore_outcomes %>% select(OFFENSE_TEXT, count)
no_baltimore$MDProbation <- mapply(FUN = probation_frequency, offense_type = no_baltimore$OFFENSE_TEXT, 
                                   county="no-baltimore")

#BaltimoreProbations: frequency with which an individual was put on probation for a certain crime
#MDProbation: The likelihood of being put on probation for a certain crime in MD
#NoBaltimore: The likelihood of being put on probation for a certain crime in MD, excluding Baltimore
new_md <- baltimore_probation %>% rename(BaltimoreCount = count) %>%
  mutate(ChanceBaltimoreProbation = baltimore_outcomes$ProbationChance) %>%
  mutate(ChanceMDProbation = all_md$MDProbation) %>%
  mutate(ChanceNoBaltimore = no_baltimore$MDProbation) %>%
  arrange(-BaltimoreCount)
View(new_md)

#Calculate frequence of committment for various crimes
commitment_frequency <- function(offense_type, county) {
  if (county == "all") {
    all_accounts <- commit %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    
    offense_table <- top_djs %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    return(sum(as.numeric(all_accounts$count/offense_table$count)))
    
  } else if (county == "no-baltimore") {
    all_accounts <- commit %>%
      filter(COUNTY != "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    
    offense_table <- top_djs %>%
      filter(COUNTY != "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    return(sum(as.numeric(all_accounts$count/offense_table$count)))
    
  } else {
    all_accounts <- commit %>%
      filter(COUNTY == "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    
    offense_table <- top_djs %>%
      filter(COUNTY == "Baltimore City") %>%
      filter(OFFENSE_TEXT == offense_type) %>%
      group_by(OFFENSE_TEXT) %>%
      summarize(count = n())
    return(sum(as.numeric(all_accounts$count/offense_table$count)))
    
  }
}

baltimore_committments <- baltimore_commit %>% select(OFFENSE_TEXT, count)
baltimore_committments$BaltimoreCommitmentChance <- mapply(FUN = commitment_frequency, offense_type = baltimore_committments$OFFENSE_TEXT, 
                                             county="baltimore")

#include Baltimore in MD calculations to understand probability of being put on probation for a given crime
all_md_commits <- baltimore_committments %>% select(OFFENSE_TEXT, count)
all_md_commits$MDCommits <- mapply(FUN = commitment_frequency, offense_type = all_md_commits$OFFENSE_TEXT, 
                             county="all")

#exclude Baltimore in calculations to understand being put on probation for a given crime
no_baltimore_commits <- baltimore_committments %>% select(OFFENSE_TEXT, count)
no_baltimore_commits$MDCommits <- mapply(FUN = commitment_frequency, offense_type = no_baltimore_commits$OFFENSE_TEXT, 
                                   county="no-baltimore")

#BaltimoreProbations: frequency with which an individual was put on probation for a certain crime
#ChanceMDCommit: The likelihood of being committed for a certain crime in MD
#ChanceNoBaltimore: The likelihood of being committed for a certain crime in MD, excluding Baltimore
md_commit_table <- baltimore_committments %>% rename(BaltimoreCount = count) %>%
  mutate(ChanceMDCommit = all_md_commits$MDCommits) %>%
  mutate(ChanceNoBaltimore = no_baltimore_commits$MDCommits) %>%
  arrange(-BaltimoreCount)
View(md_commit_table)


probationcommit <- merge(md_commit_table, new_md, by="OFFENSE_TEXT")
probationcommit <- rename(probationcommit, CommitCount = BaltimoreCount.x, ProbationCount = BaltimoreCount.y)

probationcommit <- probationcommit %>% filter(ProbationCount > 30)

pcgraph <- ggplot(probationcommit, aes(y=reorder(OFFENSE_TEXT, ChanceBaltimoreProbation))) +
  geom_line(aes(x=BaltimoreCommitmentChance, colour="Commitment"), group=1) +
  geom_line(aes(x=ChanceBaltimoreProbation, colour="Probation"), group=1) +
  #theme(axis.text.y = element_text(size=5)) +
  labs(y="Offense", x="Probation/Commitment Chance", colour="Key")
pcgraph

top_probation <- new_md %>% filter(BaltimoreCount > 30)

bmoreprobgraph <- ggplot(probationcommit, aes(y=reorder(OFFENSE_TEXT, ChanceBaltimoreProbation))) +
  geom_line(aes(x=ChanceBaltimoreProbation, colour="Baltimore"), group=1) +
  geom_line(aes(x=ChanceMDProbation, colour="All MD"), group=1) +
  geom_line(aes(x=ChanceNoBaltimore.y, colour="MD excluding Baltimore"), group=1) +
  labs(y="Offense", x="Probation Chance", colour="Key")
bmoreprobgraph
  
md_commit_table <- filter(md_commit_table, OFFENSE_TEXT != "Deadly Weapon Misdemeanor - Openly with Intent to Injure - Dangerous Weapon, Mace/Chemical Device - Wear or Carry")
top_md_commit <- md_commit_table %>% filter(BaltimoreCount > 30)

bmorecommitgraph <- ggplot(probationcommit, aes(y=reorder(OFFENSE_TEXT, ChanceBaltimoreProbation))) +
  geom_line(aes(x=BaltimoreCommitmentChance, colour="Baltimore"), group=1) +
  geom_line(aes(x=ChanceMDCommit, colour="All MD"), group=1) +
  geom_line(aes(x=ChanceNoBaltimore.x, colour="MD excluding Baltimore"), group=1) +
  labs(y="Offense", x="Commitment Chance", colour="Key")
bmorecommitgraph
  
  
  
  
