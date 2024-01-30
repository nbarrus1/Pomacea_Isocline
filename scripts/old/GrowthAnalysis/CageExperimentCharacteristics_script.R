rm(list = ls())

####libraries####

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)

####load data####

#growthdata
growthdata <- read_excel("Growth_DataSheet_v5.xlsx", sheet = 2)

cagesurv <- growthdata %>% 
  filter(Season == "wet") %>% 
  filter(Fate == "alive" | Fate == "dead") %>% 
  group_by(Cage,Season, Fate) %>% 
  summarise(n = n()) %>% 
  mutate(tot = sum (n),
         surv = n/tot) %>% 
  filter(Fate == "alive") %>%
  select(Cage, Season, surv)
  
growthdata <- growthdata %>% 
  select(StartDate,EndDate, Cell,Season,Cage) %>% 
  group_by(StartDate,EndDate, Cell,Season,Cage) %>% 
  summarise(n = n()) %>% 
  select(-n) %>% 
  ungroup()
#treatment codes
treatmentcode <- read_excel("Growth_DataSheet_v5.xlsx", sheet = 4) %>% 
  rename(EndDate = Date)
#maculata length
maclen <- read_excel("Growth_DataSheet_v5.xlsx", sheet = 5) %>% 
  group_by(Season,Cage) %>% 
  summarise(ave.maclength = mean(mac_size_mm, na.rm = T)) %>% 
  mutate(ave.maclength = if_else(is.na(ave.maclength), true = 0, false = ave.maclength))
#CageTP
TPdata <- read_excel("CageExperiment_TPdata.xlsx", sheet = 3)


cagecharacter <- growthdata %>% 
  filter(Season == "wet") %>% 
  left_join(treatmentcode, by = c("Cage", "EndDate", "Season")) %>% 
  left_join(maclen, by = c("Season", "Cage")) %>% 
  left_join(TPdata, by = c("Cage", "Cell", "Season","Code")) %>% 
  left_join(cagesurv, by = c("Cage", "Season"))  %>% 
  mutate(ave.maclength = if_else(is.na(ave.maclength),
                                 true = 0,
                                 false = ave.maclength)) %>% 
  filter(Cell == "M2") %>% 
  select(-Cell, -Season, -Code, -StartDate, -EndDate)

write_csv(cagecharacter, "table_cagecharact.csv")

cagecharacter %>% 
  filter(Periphyton_TP > 50 & Periphyton_TP < 400) %>% 
  ggplot(aes(x = Treatment, y = Periphyton_TP)) +
  geom_boxplot()+
  theme_classic()
