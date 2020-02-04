#Katie Newbold
#Office of Performance and Innovation
#January 29, 2020

library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)


disposition2002_2010 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2002_2010.xlsx")
disposition2011_2019 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Disposition_2011_2019.xlsx")
offense2002_2010 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2002_2010.xlsx")
offense2011_2019 <- read_excel("~/Documents/OPI/data/UnlockedDJSData/Offense_2011_2019.xlsx")

#combine all offense data
all_offense <- rbind(offense2002_2010, offense2011_2019)

#Make decision code key
decision_codes <- all_offense %>% 
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

decision_frequency <- ggplot(decision_codes, aes(x=DETNDECIDE_CODE, y=decision_codes$count)) +
  geom_bar(stat="identity", color="black", fill="lightblue") +
  labs(title="Frequency of Decision Type for Youth Arrests MD, 2002-2019") +
  scale_x_discrete("Codes", labels=c("AINF", "AITO", "CRAI", "DISA", 
                            "DRNJ", "FAFP", "FDVP", "FDWA", "FFSA", 
                            "FIDA", "FNFA", "IIDA", "SAFO")) +
  scale_y_continuous(breaks=seq(0, 453128, 50000))
decision_frequency

#----------Decision Frequency By County-----------------------
#-----------------Allegany----------------------------
decisions_Allegany <- all_offense %>% 
  filter(COUNTY=="Allegany") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

#if (nrow(decisions_Allegany) != 13) {
#  for (row_num in nrow(decision_code); row_num++) {
#    if (decision_codes$DETNDECIDE_CODE[row_num] != decisions_Allegany$DETNDECIDE_CODE) {
#      rbind()
#    }
#  }
#}

totalAllegany <- sum(decisions_Allegany$count)

alleganyPlot <- ggplot(decisions_Allegany, aes(x=DETNDECIDE_CODE, y=count/totalAllegany)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Dispostion type frequency, Allegany, 2002-2019")
alleganyPlot

#----------------Anne Arundel---------------------
decisions_AnneArundel <- all_offense %>% 
  filter(COUNTY=="Anne Arundel") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalAnneArundel <- sum(decisions_AnneArundel$count)

anneArundelPlot <- ggplot(decisions_AnneArundel, aes(x=DETNDECIDE_CODE, y=count/totalAnneArundel)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Anne Arundel, 2002-2019")
anneArundelPlot

#-------------------Baltimore City-------------------
decisions_BaltimoreCity <- all_offense %>% 
  filter(COUNTY=="Baltimore City") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalBaltimoreCity <- sum(decisions_BaltimoreCity$count)

baltimoreCityPlot <- ggplot(decisions_BaltimoreCity, aes(x=DETNDECIDE_CODE, y=count/totalBaltimoreCity)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, BaltimoreCity, 2002-2019")
baltimoreCityPlot

#----------------Baltimore County---------------------
decisions_BaltimoreCounty <- all_offense %>% 
  filter(COUNTY=="Baltimore County") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalBaltimoreCounty <- sum(decisions_BaltimoreCounty$count)

baltimoreCountyPlot <- ggplot(decisions_BaltimoreCounty, aes(x=DETNDECIDE_CODE, y=count/totalBaltimoreCounty)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Baltimore County, 2002-2019")
baltimoreCountyPlot

#-------------------------Calvert-----------------------
decisions_Calvert <- all_offense %>% 
  filter(COUNTY=="Calvert") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalCalvert <- sum(decisions_Calvert$count)

calvertPlot <- ggplot(decisions_Calvert, aes(x=DETNDECIDE_CODE, y=count/totalCalvert)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Calvert, 2002-2019")
calvertPlot

#-----------------Caroline-----------------------
decisions_Caroline <- all_offense %>% 
  filter(COUNTY=="Caroline") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalCaroline <- sum(decisions_Caroline$count)

carolinePlot <- ggplot(decisions_Caroline, aes(x=DETNDECIDE_CODE, y=count/totalCaroline)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Caroline, 2002-2019")
carolinePlot

#--------------Carroll----------------------------
decisions_Carroll <- all_offense %>% 
  filter(COUNTY=="Carroll") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalCarroll <- sum(decisions_Carroll$count)

carrollPlot <- ggplot(decisions_Carroll, aes(x=DETNDECIDE_CODE, y=count/totalCarroll)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Carroll, 2002-2019")
carrollPlot

#-------------------Cecil------------------------
decisions_Cecil <- all_offense %>% 
  filter(COUNTY=="Cecil") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalCecil <- sum(decisions_Cecil$count)

cecilPlot <- ggplot(decisions_Cecil, aes(x=DETNDECIDE_CODE, y=count/totalCecil)) +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Cecil, 2002-2019")
cecilPlot

#---------------Charles---------------
decisions_Charles <- all_offense %>% 
  filter(COUNTY=="Charles") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalCharles <- sum(decisions_Charles$count)

charlesPlot <- ggplot(decisions_Charles, aes(x=DETNDECIDE_CODE, y=count/totalCharles)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Charles, 2002-2019")
charlesPlot

#---------------------Dorchester-----------------------
decisions_Dorchester <- all_offense %>% 
  filter(COUNTY=="Dorchester") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalDorchester <- sum(decisions_Dorchester$count)

dorchesterPlot <- ggplot(decisions_Dorchester, aes(x=DETNDECIDE_CODE, y=count/totalDorchester)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Dorchester, 2002-2019")
dorchesterPlot

#---------------------Frederick--------------------
decisions_Frederick <- all_offense %>% 
  filter(COUNTY=="Frederick") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalFrederick <- sum(decisions_Frederick$count)

frederickPlot <- ggplot(decisions_Frederick, aes(x=DETNDECIDE_CODE, y=count/totalFrederick)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Frederick, 2002-2019")
frederickPlot

#------------------------Garrett--------------------
decisions_Garrett <- all_offense %>% 
  filter(COUNTY=="Garrett") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalGarrett <- sum(decisions_Garrett$count)

garrettPlot <- ggplot(decisions_Garrett, aes(x=DETNDECIDE_CODE, y=count/totalGarrett)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Garrett, 2002-2019")
garrettPlot

#---------------------Harford------------------------
decisions_Harford <- all_offense %>% 
  filter(COUNTY=="Harford") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalHarford <- sum(decisions_Harford$count)

harfordPlot <- ggplot(decisions_Harford, aes(x=DETNDECIDE_CODE, y=count/totalHarford)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Harford, 2002-2019")
harfordPlot

#--------------------Howard--------------------------
decisions_Howard <- all_offense %>% 
  filter(COUNTY=="Howard") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalHoward <- sum(decisions_Howard$count)

howardPlot <- ggplot(decisions_Howard, aes(x=DETNDECIDE_CODE, y=count/totalHoward)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Howard, 2002-2019")
howardPlot

#---------------------Kent--------------------------
decisions_Kent <- all_offense %>% 
  filter(COUNTY=="Kent") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalKent <- sum(decisions_Kent$count)

kentPlot <- ggplot(decisions_Kent, aes(x=DETNDECIDE_CODE, y=count/totalKent)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Kent, 2002-2019")
kentPlot

#-----------------------Montgomery-------------------
decisions_Montgomery <- all_offense %>% 
  filter(COUNTY=="Montgomery") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalMontgomery <- sum(decisions_Montgomery$count)

montgomeryPlot <- ggplot(decisions_Montgomery, aes(x=DETNDECIDE_CODE, y=count/totalMontgomery)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Montgomery, 2002-2019")

plot(montgomeryPlot)

#------------------Prince George's-------------------
decisions_PrinceGeorge <- all_offense %>% 
  filter(COUNTY=="Prince George`s") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalPrinceGeorge <- sum(decisions_PrinceGeorge$count)

princeGeorgePlot <- ggplot(decisions_PrinceGeorge, aes(x=DETNDECIDE_CODE, y=count/totalPrinceGeorge)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Prince George's, 2002-2019")

plot(princeGeorgePlot)

#--------------------Queen Anne's---------------------
#Apostrophe problem -- not an actual apostrophe but this works
decisions_QueenAnnes <- all_offense %>% 
  filter(COUNTY=="Queen Anne`s") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalQueenAnnes <- sum(decisions_QueenAnnes$count)

queenAnnesPlot <- ggplot(decisions_QueenAnnes, aes(x=DETNDECIDE_CODE, y=count/totalQueenAnnes)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Queen Anne's, 2002-2019")

plot(queenAnnesPlot)

#----------------------Somerset------------------------
decisions_Somerset <- all_offense %>% 
  filter(COUNTY=="Somerset") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalSomerset <- sum(decisions_Somerset$count)

somersetPlot <- ggplot(decisions_Somerset, aes(x=DETNDECIDE_CODE, y=count/totalSomerset)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Somerset, 2002-2019")

plot(somersetPlot)

#----------------------St. Mary's--------------------
decisions_StMary <- all_offense %>% 
  filter(COUNTY=="St. Mary`s") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalStMary <- sum(decisions_StMary$count)

stMaryPlot <- ggplot(decisions_StMary, aes(x=DETNDECIDE_CODE, y=count/totalStMary)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, St. Mary's, 2002-2019")

plot(stMaryPlot)

#----------------------Talbot-----------------------
decisions_Talbot <- all_offense %>% 
  filter(COUNTY=="Talbot") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalTalbot <- sum(decisions_Talbot$count)

talbotPlot <- ggplot(decisions_Talbot, aes(x=DETNDECIDE_CODE, y=count/totalTalbot)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Talbot, 2002-2019")

plot(talbotPlot)

#--------------------Washington--------------------
decisions_Washington <- all_offense %>% 
  filter(COUNTY=="Washington") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalWashington <- sum(decisions_Washington$count)

washingtonPlot <- ggplot(decisions_Washington, aes(x=DETNDECIDE_CODE, y=count/totalWashington)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Washington, 2002-2019")

plot(washingtonPlot)

#------------------Wicomico-----------------------
decisions_Wicomico <- all_offense %>% 
  filter(COUNTY=="Wicomico") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalWicomico <- sum(decisions_Wicomico$count)

wicomicoPlot <- ggplot(decisions_Wicomico, aes(x=DETNDECIDE_CODE, y=count/totalWicomico)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Wicomico, 2002-2019")

plot(wicomicoPlot)

#-------------------Worcester----------------------
decisions_Worcester <- all_offense %>% 
  filter(COUNTY=="Worcester") %>%
  select(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  group_by(DETNDECIDE_CODE, DECISIONINTK_TEXT) %>%
  summarize(count = n()) %>%
  arrange(DETNDECIDE_CODE)

totalWorcester <- sum(decisions_Worcester$count)

worcesterPlot <- ggplot(decisions_Worcester, aes(x=DETNDECIDE_CODE, y=count/totalWorcester)) +
  geom_bar(stat="identity", color="black", fill="#f2ca47") +
  geom_text(aes(label=scales::number(count)), vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") + 
  xlab("Outcome") +
  labs(title="Disposition type frequency, Worcester, 2002-2019")
worcesterPlot

#currently having issue with the tiling, I think the error is with the function. Come back to this
calculate_percent <- function(code) {
  dcodecount <- tally(all_offense$DETNDECIDE_CODE == code)
  percent <- dcodecount/tally(all_offense)
  return(percent)
}

#all county tile
all_county <- ggplot(all_offense, aes(x=DETNDECIDE_CODE, y=calculate_percent(x))) +
  geom_tile() +
  geom_bar(stat="identity", color="black", fill="#f2ca27") +
  facet_wrap(~ all_offense$COUNTY, nrow=6) + 
  ylim(0, NA)
all_county




