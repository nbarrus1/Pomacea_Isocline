rm(list = ls())

####libraries####

library(tidyverse)
library(readxl)
library(ggpubr)
library(lemon)
library(cowplot)
library(gridExtra)
library(ggthemes)

#####load data####

dietdata.raw <- read_excel("PredatorDiets_v1.5.xls", sheet = 2)

###clean up the data####

#lines 18-22 create a new variable with lowest taxonomy of family

dietdata.clean <- dietdata.raw %>%  #create a new tibble called clean using the raw data
  unite(col = "pred.genus_species", c(Pred.Genus,Pred.Species), sep = " ") %>%   #combine predator genus and species names
  mutate(low.taxonomy = if_else(!is.na(Prey.Family), true = Prey.Family,
                                false = if_else(!is.na(Prey.Order), true = Prey.Order,
                                                false = if_else(!is.na(Prey.Class), true = Prey.Class,
                                                                false = if_else(!is.na(Prey.Phylum), true = Prey.Phylum,
                                                                                false = Prey.Kingdom)))))
#####summarise data####

#summarise the diet data using counts for each individual sample
#can change the variable "low.taxonomy" to any other of the prey.taxonomy to gain or lose prey specificity 

individual.summary <- dietdata.clean %>%  #create a new tibble called dietsummary from our cleaned up dietdata
  group_by(pred.genus_species,Sample, low.taxonomy) %>% #use the three variables as groupings
  summarise(Count = sum(Count)) %>% #find the sum of Count for each group
  mutate(tot = sum(Count),          #find the sum of Count for each Diet Sample
         prop = Count/tot)          #find the propotion contribution of each diet item

#summarise the diet data using counts for each predator species individuals pooled
#can change the variable "low.taxonomy" to any other of the prey.taxonomy to gain or lose prey specificity 

predator.summary <- dietdata.clean %>% #create a new tibble called dietsummary from our cleaned up dietdata
  drop_na(Prey.Class) %>% 
  group_by(pred.genus_species, Prey.Class) %>% #use the three variables as groupings
  summarise(Count = sum(Count)) %>% #find the sum of Count for each group
  mutate(tot = sum(Count),          #find the sum of Count for each predator species
         prop = Count/tot,
         n.predators = if_else(pred.genus_species == "Mayahero uropthalmus",
                               true = 98, false = 34),
         stand.count = round(Count/n.predators, digits = 2))          #find the propotion contribution of each diet item

#visualize results from predator.summary
#if "low.taxonomy" was changed previously then it will need to be changes here as well

dietplota <- predator.summary %>%  #specify the data to visualize
  group_by(pred.genus_species) %>% 
  mutate(reorder = fct_reorder(Prey.Class, stand.count)) %>%
  drop_na(reorder) %>% 
  ggplot(aes(x = reorder, y = stand.count))+ #specify which variables to use on which axis
  theme_classic()+                         #set theme
  facet_wrap(~pred.genus_species, scales = "free")+         #create seperate pannel plots for each predator
  geom_col(color = "black", fill = "#666666")+
  coord_flip()+
  labs(x = "Prey Class",
       y = NULL)+
  theme(strip.text = element_text(face = "italic"))+
  geom_text(aes(label = stand.count), nudge_y = 1.6)+
  scale_y_continuous(limits = c(0,20), breaks = c(0,5,10,15,20))


#summarise with gastropod sized

gastropod.summary <- dietdata.clean %>% 
  filter(Prey.Class == "Gastropoda") %>% 
  mutate(SL_cat = if_else(Length_mm <= 2, true = "SL < 2 mm",
                          false = if_else(Length_mm > 2 & Length_mm <= 3,
                                          true = "2 mm < SL < 3 mm",
                                          false = if_else(Length_mm > 3 & Length_mm <= 4,
                                                          true = "3 mm < SL < 4 mm",
                                                          false = if_else(Length_mm > 4 & Length_mm <= 5,
                                                                          true = "4 mm < SL < 5 mm",
                                                                          false = if_else(Length_mm > 5 & Length_mm <= 6,
                                                                                          true = "5 mm < SL < 6 mm",
                                                                                          false = if_else(Length_mm > 6 & Length_mm <= 7,
                                                                                                          true = "6 mm < SL < 7 mm",
                                                                                                          false =if_else(Length_mm > 7 & Length_mm <= 8,
                                                                                                                         true = "7 mm < SL < 8 mm",
                                                                                                                         false = if_else(Length_mm > 8 & Length_mm <= 9,
                                                                                                                                         true = "8 mm < SL < 9 mm",
                                                                                                                                         false = if_else(Length_mm > 9 & Length_mm <= 10,
                                                                                                                                                         true = "9 mm < SL < 10 mm",
                                                                                                                                                         false = if_else(Length_mm > 10 & Length_mm <= 11,
                                                                                                                                                                         true = "10 mm < SL < 11 mm",
                                                                                                                                                                         false =if_else(Length_mm > 11 & Length_mm <= 12,
                                                                                                                                                                                        true = "11 mm < SL < 12 mm",
                                                                                                                                                                                        false = "12 mm < SL < 13 mm")))))))))))) 

gastropod.summary <- gastropod.summary %>% 
  drop_na(SL_cat) %>% 
  group_by(pred.genus_species,SL_cat) %>% 
  summarise(Count = sum(Count)) %>% 
  ungroup() %>% 
  complete(pred.genus_species,SL_cat, fill = list(Count = 0)) %>% 
  group_by(pred.genus_species) %>% 
  mutate(tot = sum(Count),
         prop = Count/tot) 

cat10_11 <- tibble(pred.genus_species = c("Mayahero uropthalmus",
                                          "Siren lacertina",
                                          "Mayahero uropthalmus",
                                          "Siren lacertina",
                                          "Mayahero uropthalmus",
                                          "Siren lacertina"),
                   SL_cat = c("10 mm < SL < 11 mm",
                              "10 mm < SL < 11 mm",
                              "13 mm < SL < 14 mm",
                              "13 mm < SL < 14 mm",
                              "14 mm < SL < 15 mm",
                              "14 mm < SL < 15 mm"),
                   Count = c(0,0,0,0,0,0),
                   tot = c(56,558,56,558,56,558),
                   prop = c(0.00,0.00,0.00,0.00,0.00,0.00))

gastropod.summary <- gastropod.summary %>% 
  bind_rows(cat10_11)

dietplotc <- gastropod.summary %>%  #specify the data to visualize
  mutate(reorder = fct_relevel(SL_cat, "SL < 2 mm", "2 mm < SL < 3 mm",
                               "3 mm < SL < 4 mm","4 mm < SL < 5 mm",
                               "5 mm < SL < 6 mm","6 mm < SL < 7 mm",
                               "7 mm < SL < 8 mm","8 mm < SL < 9 mm",
                               "9 mm < SL < 10 mm","10 mm < SL < 11 mm",
                               "11 mm < SL < 12 mm","12 mm < SL < 13 mm",
                               "13 mm < SL < 14 mm","14 mm < SL < 15 mm")) %>% 
  mutate(labels = sprintf("%.3f",round(prop,3))) %>% 
  ggplot(aes(x = reorder, y = prop))+ #specify which variables to use on which axis
  theme_classic()+                         #set theme
  facet_wrap(~pred.genus_species, scales = "free")+         #create seperate pannel plots for each predator
  geom_col(color = "black", fill = "#666666")+
  coord_flip()+
  labs(x = "Shell Length",
       y = "Proportion")+
  theme(strip.text = element_text(face = "italic"))+
  scale_y_continuous(limits = c(0.0, 0.8), 
                     breaks = c(0.0,0.2,0.4,0.6,0.8))+
  geom_text(aes(label = labels), nudge_y = 0.1)
  

gastropod.summary <- dietdata.clean %>% 
  filter(Prey.Class == "Gastropoda") %>% 
  drop_na(Prey.Family) %>% 
  group_by(pred.genus_species,Prey.Family) %>% 
  summarise(Count = sum(Count)) %>% 
  ungroup() %>% 
  complete(pred.genus_species,Prey.Family, fill = list(Count = 0)) %>% 
  group_by(pred.genus_species) %>% 
  mutate(tot = sum(Count),
         prop = Count/tot)

dietplotb <- gastropod.summary %>% 
  group_by(pred.genus_species) %>% 
  mutate(reorder = fct_reorder(Prey.Family, prop),
         labels = sprintf("%.3f",round(prop,3)))%>%
  ggplot(aes(x = reorder, y = prop))+ #specify which variables to use on which axis
  facet_wrap(~pred.genus_species, scales = "free")+       #create seperate pannel plots for each predator
  geom_col(color = "black", fill = "#666666")+
  coord_flip()+
  labs(x = "Gastropod Family",
       y = NULL)+
  theme_classic()+
  theme(strip.text = element_text(face = "italic"))+
  scale_y_continuous(limits = c(0.0,0.8), 
                     breaks = c(0.0,0.2,0.4,0.6,0.8))+
  geom_text(aes(label = labels), nudge_y = 0.085)

dietplot <- ggarrange(dietplota,dietplotb,dietplotc, ncol = 1, labels = c("A)","B)","C)"))

ggsave(filename = "dietplot.png", plot = dietplot, device = "png",
       units = "in", width = 9, height = 12)


gastropod.summary <- dietdata.clean %>% 
  filter(Prey.Class == "Gastropoda") %>% 
  drop_na(Length_mm) %>% 
  group_by(pred.genus_species,Sample) %>% 
  summarise(n = n()) %>% 
  mutate(place = 1) %>% 
  group_by(pred.genus_species) %>% 
  summarise(n.pred = sum(place))

  