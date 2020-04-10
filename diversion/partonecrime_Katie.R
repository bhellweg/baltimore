library(tidyverse)
library(readxl)
library(formattable)
library(lubridate)
library(leaflet)
library(dplyr)
library(scales)
library(formattable)
library(delt)

partonecrime <- read_excel("~/Documents/OPI/covid/partone20200410.xlsx")

commercial_rob <- partonecrime %>%
  filter(Description == "ROBBERY - COMMERCIAL") %>%
  filter(`Crime Date` > (startdate - (today() - startdate)),
         `Crime Date` < today()) %>%
  group_by(Date = ifelse(`Crime Date` > as.Date(startdate),
                         "After Shutdown",
                         "Before Shutdown")) %>%
  group_by(`Police District`, Date) %>%
  mutate(District = tolower(`Police District`))


clean_commercial <- commercial_rob %>%
  mutate(PD = case_when(District == "northeast" ~ "northeastern",
                        District == "southeast" ~ "southeastern",
                        District == "southwest" ~ "southwestern",
                        District == "central" ~ "central",
                        District == "eastern" ~ "eastern",
                        District == "northeastern" ~ "northeastern",
                        District == "northern" ~ "northern",
                        District == "northwestern" ~ "northwestern",
                        District == "southeastern" ~ "southeastern",
                        District == "southern" ~ "southern",
                        District == "southwestern" ~ "southwestern",
                        District == "western" ~ "western")) %>%
  mutate(`Day Type` = ifelse(`Day Of Week` == 'FRIDAY' | `Day Of Week` == 'SATURDAY',
                             "Weekend", "Weekday")) %>%
  group_by(PD, Date, `Day Type`) %>%
  summarize(count = n()) %>%
  na.omit()
  

district_comm <- ggplot(clean_commercial, aes(x=Date, y=count)) +
  geom_bar(fill="lightblue", stat="identity") +
  labs(title = "Number of commercial robberies per day before/after shutdown", y="Number of Calls", x=element_blank())+
  facet_wrap(~ PD, nrow=3, scales="free_y")
district_comm  


twplot <- clean_commercial %>% 
  ggplot(aes(y = PD, 
             x = `Day Type`)) +
  geom_tile(aes(fill = count)) +  
  scale_fill_gradient(low = "white", 
                      high = "purple",
                      na.value = "grey50",
                      guide="colorbar") +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.6), 
        legend.title = element_blank(), 
        legend.position="top", 
        legend.direction="horizontal", 
        legend.key.width=unit(2, "cm"), 
        legend.key.height=unit(0.25, "cm")) +
  labs(y = "Hour of Arrest", x = "Day of Week", 
       title = "Number of Commercial Robberies") +
  scale_x_discrete(expand = c(0,0) , 
                   position = "top") + 
  geom_text(aes(label = count), 
            size = 3) +
  facet_wrap(~ Date)
twplot


