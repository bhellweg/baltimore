library(readxl)
library(dplyr)

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

#Join the tables to create a master table using the keys

offdemo <- merge(off,demo,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
offdisp <- merge(off,disp,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))
all_djs <- merge(offdisp,demo,by = c("REVACTOR_ID","REVLEGALINCIDENT_KEY"))

#Create Baltimore tables

baltdisp  <- disp %>% filter(COUNTY == "Baltimore City")
baltoff   <- off %>% filter(COUNTY == "Baltimore City")
baltdemo  <- demo %>% filter(COUNTY == "Baltimore City")
baltodemo <- offdemo %>% filter(COUNTY.y == "Baltimore City")
baltodisp <- offdisp %>% filter(COUNTY.y == "Baltimore City")
baltall   <- all_djs %>% filter(COUNTY == "Baltimore City")

View(baltall)
