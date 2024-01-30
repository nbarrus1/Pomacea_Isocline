rm(list = ls())
####libraries####

library(tidyverse)
library(readxl)
library(ggpubr)
library(lemon)
library(cowplot)
library(gridExtra)
library(qpcR)

#####load in data####
tetherdata <- read_excel("tetheringdata.xlsx", sheet = 4)                                            #we do not know how to interpret these deaths so lets remove from analysis

lengthcodes <- read_excel("tetheringdata.xlsx", sheet = 2)      

tetherdata <- tetherdata %>% 
  left_join(lengthcodes, by = c("Season", "Box", "Bin", "Box_day")) 


### quick summary of our data ###

tether.summ <- tetherdata %>% 
  filter(Length < 10) %>%
  mutate(count = 1) %>% 
  group_by(Fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  mutate(n = sum(sum.fate),
         rate = sum.fate/n)

tether.summ

#Season specific survival

tether.summ <- tetherdata %>%
  filter(Length < 10) %>% 
  mutate(count = 1,
         Fate = if_else(Fate == "a", true = "s",
                        false = "m")) %>% 
  group_by(Season, Fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(Season) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))))

tether.summ

#cumulative survival

combined <- tetherdata %>%
  filter(Length < 10) %>% 
  mutate(count = 1,
         Fate = if_else(Fate == "a", true = "s",
                        false = "m")) %>% 
  group_by(Fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         Season = "combined")

s.paradata <- tether.summ %>% 
  bind_rows(combined) %>% 
  filter(Fate == "s") %>%
  rename(season = Season,
         value = CJS) %>% 
  mutate(parameter = "CJS") %>% 
  dplyr::select(low,upp,season,value,parameter)


g.paradata<- read_csv(file = "C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/DornLabMeeting/ExperimentDesign_Growth/growth_est.csv")

para.data <- g.paradata %>% 
  bind_rows(s.paradata) %>% 
  mutate(upp = round(upp, digits = 3),
         low = round(low, digits = 3),
         value = round(value,digits = 3))

write_csv(para.data, file = "para_data.csv")

####season differences####
 
###### Logistic Regression ######

tetherdata <- tetherdata %>% 
  mutate(survival = if_else(Fate == "a", true = 1, false = 0))

length(tetherdata$survival)- sum(tetherdata$survival)


enclosuredata <- read_excel("predatorfree_survival.xlsx", sheet = 2)

summ_encl <- enclosuredata %>% 
  group_by(season) %>% 
  summarise(ave = mean(proportion),
         n = n(),
         sd = sd(proportion),
         se = sd/sqrt(n),
         upp = ave + (1.96*se),
         low = ave - (1.96*se),
         upp = if_else(upp >1, true = 1.00, false = upp)) 

Survplotc <- enclosuredata %>% 
  ggplot(aes(x = day, y = proportion))+
  geom_line()+
  geom_hline(data = summ_encl, aes(yintercept = ave), color = "darkred", size = 1.5)+
  geom_hline(data = summ_encl, aes(yintercept = upp), linetype = "dashed",color = "darkred")+
  geom_hline(data = summ_encl, aes(yintercept = low), linetype = "dashed",color = "darkred")+
  geom_point(size = 3, pch = 21, fill = "#666666")+
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27))+
  facet_rep_wrap(~season)+
  theme_classic()+
  labs( y = "Daily Survival Probablility", x = "Day")
  
ggsave("C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/SizeDependentMortality_manuscript/Figures&Tables/supp_cagesurvivalplot.png",
       plot = Survplotc, device = "png", units = "in",
       width = 8, height = 4)

Survplota <- tetherdata %>% 
  ggplot(aes(x = Length, y = survival, linetype = Season)) +
  theme_classic()+
  geom_smooth(aes(fill = Season), color = "black", method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), show.legend = F)+
  coord_cartesian(ylim =  c(0.55, 1.00))+
  labs(y = "Daily Survival Probability",
       x = "Shell Length (mm)",
       title = "A)")+
  scale_fill_manual(values = c("#333333", "#999999"))

Survplotb <- tetherdata %>% 
  ggplot(aes(x = Length, y = survival, linetype = Season)) +
  theme_classic()+
  geom_smooth(color = "black", method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), show.legend = F)+
  coord_cartesian(ylim =  c(0.55, 1.00),
                  xlim = c(3,15))+
  scale_x_continuous(breaks = c(3,6,9,12,15))+
  labs(y = NULL,
       x = "Shell Length (mm)",
       title = "B)")

Survplotleg <- tetherdata %>% 
  filter(Length < 15) %>% 
  ggplot(aes(x = Length, y = survival, linetype = Season)) +
  theme_classic()+
  geom_smooth(color = "black", method="glm", se=F, fullrange=TRUE, 
              method.args = list(family=binomial))+
  coord_cartesian(ylim =  c(0.55, 1.00),
                  xlim = c(3,15))+
  labs(y = NULL,
       x = "Shell Length (mm)",
       title = "B)")

Survplotleg <- cowplot::get_legend(Survplotleg)

survplot <- grid.arrange(Survplota,Survplotb,Survplotleg, 
             ncol = 20, nrow = 1,
             layout_matrix = rbind(c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3)))

ggsave("C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/SizeDependentMortality_manuscript/Figures&Tables/figure1_logregplot.png",
       plot = survplot, device = "png", units = "in",
       width = 8, height = 4)



survival_summary <- tetherdata %>% 
  
  mutate(count = 1) %>% 
  group_by(Season, Wetland, Sizeclass) %>% 
  summarise(n.surv = sum(survival),
            n.tot = sum(count),
            per.survival = n.surv/n.tot*100)

survival_summary

##equation is p(x) = e^(b0+b1*x)/(1 - e^(b0+b1*x))

logregfit1 <- glm(survival~Length, data = tetherdata, family = "binomial")
logregfit2 <- glm(survival~Length + Wetland, data = tetherdata, family = "binomial")
logregfit3 <- glm(survival~Length + Season, data = tetherdata, family = "binomial")
logregfit4 <- glm(survival~Length + Wetland + Season, data = tetherdata, family = "binomial")
logregfit5 <- glm(survival~Season + Wetland, data = tetherdata, family = "binomial")
logregfit6 <- glm(survival~Wetland, data = tetherdata, family = "binomial")
logregfit7 <- glm(survival~Length*Wetland, data = tetherdata, family = "binomial")
logregfit8 <- glm(survival~Length*Wetland + Season, data = tetherdata, family = "binomial")
logregfit9 <- glm(survival~Season, data = tetherdata, family = "binomial")
logregfit10 <- glm(survival~Length*Season, data = tetherdata, family = "binomial")
logregfit11 <- glm(survival~Length*Season + Wetland, data = tetherdata, family = "binomial")
logregfit12 <- glm(survival~Season*Wetland, data = tetherdata, family = "binomial")
logregfit13 <- glm(survival~Season*Wetland + Length, data = tetherdata, family = "binomial")
logregfit14 <- glm(survival~Transect, data = tetherdata, family = "binomial")
logregfit15 <- glm(survival~Transect + Wetland, data = tetherdata, family = "binomial")
logregfit16 <- glm(survival~Transect + Season, data = tetherdata, family = "binomial")
logregfit17 <- glm(survival~Transect + Length, data = tetherdata, family = "binomial")
logregfit18 <- glm(survival~Transect*Season, data = tetherdata, family = "binomial")
logregfit19 <- glm(survival~Transect*Length, data = tetherdata, family = "binomial")
logregfit20 <- glm(survival~Transect*Wetland, data = tetherdata, family = "binomial")
logregfit21 <- glm(survival~Transect + Wetland + Season + Length, data = tetherdata, family = "binomial")
logregfit22 <- glm(survival~Transect + Wetland + Season, data = tetherdata, family = "binomial")
logregfit23 <- glm(survival~Transect + Wetland + Length, data = tetherdata, family = "binomial")
logregfit24 <- glm(survival~Transect +Season + Length, data = tetherdata, family = "binomial")
logregfit26 <- glm(survival~Length*Season + Transect, data = tetherdata, family = "binomial")
logregfit27 <- glm(survival~Length*Season + Transect + Wetland, data = tetherdata, family = "binomial")


logregs <- list(logregfit1,logregfit2,logregfit3,logregfit4,logregfit5,logregfit6,logregfit7,logregfit8,
             logregfit9,logregfit10,logregfit11,logregfit12,logregfit13,logregfit14,logregfit15,
             logregfit16,logregfit17,logregfit18, logregfit19,logregfit20,logregfit21,logregfit22,
             logregfit23,logregfit24, logregfit26,logregfit27)

AIC(logregfit1)
log.AIC <- tibble(model = 1:26,
                  aic = sapply(logregs, AIC)) 

log.AIC <- log.AIC %>% 
  mutate(deltaAIC = aic-min(aic),
         w = akaike.weights(log.AIC$aic)$weights)

write_csv(log.AIC, file = "table1_AIC.csv")

summary(logregfit10)
summary(logregfit11)
summary(logregfit26)
summary(logregfit27)  ###models 11-27 have nonsignificant contibuters of transects and wetlands

tetherdata <- tetherdata %>% 
  mutate(mort.pred = predict(logregfit1, type = "response"))



####Mortality####

mort_summary <- tetherdata %>% 
  filter(Length < 10) %>% 
  filter(Fate != "a") %>% 
  filter(Fate != "d") %>% 
  mutate(count = 1) %>% 
  group_by(Season, Fate) %>% 
  summarise(n = sum(count)) %>% 
  mutate(n.tot =sum(n),
         perc.contr = n/n.tot*100,
         prop.contr = n/n.tot,
         upp = (prop.contr + (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = (prop.contr - (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = if_else(low > 0, true = low, false = 0.00))

DryMortCont <- mort_summary %>% 
  ggplot(aes(y = n, x = Season))+
  geom_bar(aes(fill = Fate), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+

  scale_fill_manual(values = c("#333333","#999999", "white"),
                    labels = c("crushed", "empty", "missing"))+
  theme(legend.title = element_blank())+
  labs(y = "Count",
      title = "A) Tethering Mortality Type by Season")

######Predator data from TT and CPUE
library(lubridate)

predatordata <- read_excel("PredatorDiets_v1.4.xlsx", sheet = 3) %>% 
  mutate(juvcray = if_else(SpeciesCode == "Profal" & StandLen_mm < 14, true = 0,
                           false = 1))

invpredatorsumm <- predatordata %>% 
  mutate(year = year (D.O.C),
         juvcray = if_else(SpeciesCode == "Profal" & StandLen_mm < 14, true = 0,
                           false = 1)) %>% 
  filter(juvcray != 0) %>% 
  drop_na(D.O.C) %>% 
  filter(SiteCode == "M4" | SiteCode == "M2") %>% 
  filter(Phylum == "Arthropoda") %>% 
  mutate(count = 1) %>% 
  group_by(Season,SpeciesCode) %>% 
  summarise(count= sum(count)) %>%
  ungroup() %>% 
  complete(Season,SpeciesCode, fill = list(count = 0)) %>% 
  group_by(Season) %>% 
  mutate(total = sum(count),
         prop = count/total,
         percent = prop*100,
         upp = (prop + (1.96 * sqrt(((1 - prop)*prop)/(total + 4))))*100,
         low = (prop - (1.96 * sqrt(((1 - prop)*prop)/(total + 4))))*100,
         upp = if_else(upp > 100, true = 100.00, false = upp),
         low = if_else(low < 0, true = 0.00, false = low))

invpredator <- invpredatorsumm %>% 
  ggplot(aes(y = count, x = Season))+
  geom_bar(aes(fill = SpeciesCode), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  #facet_rep_grid(~Season)+
  scale_fill_manual(values = c("#999999","#333333"),
                    labels = c("Belostoma", "Crayfish"))+
  #geom_errorbar(aes(ymin = low, ymax = upp, group = SpeciesCode),
  #              position = position_dodge(width = 0.9),
  #              width = 0.1, alpha = 0.9)+
  theme(legend.title = element_blank())+
  labs(y = "Count",
       x = "Season",
       title = "B) Invertebrate Predators by Season")#+
  #scale_y_continuous(breaks = c(0,20,40,60,80,100))
  

vertpredatorsumm <- predatordata %>% 
  mutate(year = year (D.O.C)) %>% 
  drop_na(D.O.C) %>% 
  filter(SiteCode == "M4" | SiteCode == "M2") %>% 
  filter(Phylum == "Chordata") %>% 
  mutate(count = 1) %>% 
  group_by(Season,SpeciesCode) %>% 
  summarise(count= sum(count)) %>%
  ungroup() %>% 
  complete(Season,SpeciesCode, fill = list(count = 0)) %>% 
  group_by(Season) %>% 
  mutate(total = sum(count),
         prop = count/total,
         percent = prop*100,
         upp = (prop + (1.96 * sqrt(((1 - prop)*prop)/(total + 4))))*100,
         low = (prop - (1.96 * sqrt(((1 - prop)*prop)/(total + 4))))*100,
         upp = if_else(upp > 100, true = 100.00, false = upp),
         low = if_else(low < 0, true = 0.00, false = low))

vertpredator <- vertpredatorsumm %>% 
  ggplot(aes(y = count, x = Season))+
  geom_bar(aes(fill = SpeciesCode), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  #facet_rep_grid(~SpeciesCode)+
  scale_fill_manual(values = c("#999999","white","#333333"),
                    labels = c("Mayan Cichlid", "Redear Sunfish","Greater Siren"))+
  #geom_errorbar(aes(ymin = low, ymax = upp, group = SiteCode),
  #              position = position_dodge(width = 0.9),
  #              width = 0.1, alpha = 0.9)+
  theme(legend.title = element_blank())+
  labs(y = "Count",
       title = "C) Vertebrate Predators by Season")#+
  #coord_cartesian(ylim = c(0,100))#+
  #scale_y_continuous(breaks = c(0,20,40,60,80,100))


Predatorcompplot <- grid.arrange(DryMortCont,invpredator,vertpredator,
                     ncol = 8, nrow = 4,
                     layout_matrix = rbind(c(1,1,1,1,1,1,1,1,1),
                                           c(1,1,1,1,1,1,1,1,1),
                                           c(2,2,2,2,3,3,3,3,3),
                                           c(2,2,2,2,3,3,3,3,3)))

save_plot(filename = "C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/SizeDependentMortality_manuscript/Figures&Tables/figure2_Predatorcompplot.png", plot = Predatorcompplot, device = png(),
          units = "in", base_width = 11, base_height = 10)

mort_summary <- tetherdata %>% 
  filter(Fate != "a") %>% 
  mutate(count = 1) %>% 
  group_by(Sizeclass, Fate) %>%
  summarise(n = sum(count)) %>% 
  mutate(n.tot =sum(n),
         perc.contr = n/n.tot*100,
         prop.contr = n/n.tot,
         upp = (prop.contr + (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = (prop.contr - (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = if_else(low > 0, true = low, false = 0.00))

mort_summary %>% 
  filter(Sizeclass != ">25mm") %>% 
  ggplot(aes(y = perc.contr, x = Sizeclass))+
  geom_bar(aes(fill = Fate), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  scale_fill_manual(values = c("#333333","#999999", "white"),
                    labels = c("crayfish", "invert", "vertebrate"))+
  geom_errorbar(aes(ymin = low, ymax = upp, group = Fate),
                position = position_dodge(width = 0.9),
                width = 0.1, alpha = 0.9)+
  theme(legend.title = element_blank())+
  ylab(label = "Percent Contribution to Mortality")


library(tidyverse)
para_est <- read.csv("growth_est.csv")

mort_para <- tetherdata %>% 
  filter(Length < 16) %>% 
  mutate(count = 1,
         Fate = if_else(Fate == "a", true = "a", false = "d")) %>% 
  group_by(Season, Fate) %>% 
  summarise(count = sum(count)) %>% 
  mutate(total = sum(count),
         surv.prop = count/total,
         surv.upp = (surv.prop + (1.96 * sqrt(((1 - surv.prop)*surv.prop)/(total + 4)))),
         surv.low = (surv.prop - (1.96 * sqrt(((1 - surv.prop)*surv.prop)/(total + 4))))) %>% 
  filter(Fate == "a") %>% 
  select(-Fate,-count,-total)

mort_para
para_est <- para_est %>% 
  left_join(mort_para, by = "Season")
para_est

write.csv(para_est, file = "para_est.csv")

library(lubridate)

tetherdata <- tetherdata %>% 
  mutate(int = interval(start = DateTimePl, end = DateTimeCh),
         dur.min = int_length(int)/60) 

hist(tetherdata$dur.min)


advstat_data <- tetherdata %>% 
  select(survival, Length, dur.min, Season, Wetland) %>% 
  mutate(Season = if_else(Season == "dry", true = 1, false = 0), #dry = 1, wet = 0
         Wetland = if_else(Wetland == "M2", true = 1, false = 0)) #M2 = 1, M4 = 0

write_csv(advstat_data, file = "SnailSurvival_advstat_data.csv")


logregfit1.1 <- glm(survival~Length, data = tetherdata[tetherdata$Season == "wet",], family = "binomial")
summary(logregfit1.1)
logregfit1.2 <- glm(survival~Length, data = tetherdata[tetherdata$Season == "dry",], family = "binomial")
summary(logregfit1.2)

chisq.test(x = tetherdata$Season[tetherdata$Fate != "a" & tetherdata$Fate != "d" & tetherdata$Length < 16],
           y = tetherdata$Fate[tetherdata$Fate != "a" & tetherdata$Fate != "d"& tetherdata$Length < 16]) #got warning check assumption

###check assumption

mortcontdata <- tetherdata %>% 
  filter(Fate != "a" & Fate != "d") %>% 
  group_by(Season, Fate) %>% 
  summarise(obs = n()) %>% 
  mutate(rtot = sum(obs)) %>% 
  ungroup() %>% 
  group_by(Fate) %>% 
  mutate(ctot = sum(obs)) %>% 
  ungroup() %>% 
  mutate(tot = sum(obs),
         exp = (rtot*ctot)/tot,
         test = sum((obs-exp)^2/exp),
         pval = 1-pchisq(test, df = (length(unique(mortcontdata$Season))-1)*(length(unique(mortcontdata$Fate)))-1)) 
  
mortcontdata

tetherdata <- tetherdata %>% 
  filter(Length < 10)

fisher.test(x = tetherdata$Season[tetherdata$Fate != "a" ],
            y = tetherdata$Fate[tetherdata$Fate != "a" ])
 #pairwise

mortcontdata <- tetherdata %>% 
  filter(Fate != "a" & Fate != "d") %>% 
  filter(Length < 10) %>% 
  group_by(Season) %>% 
  summarise(obs = n()) %>% 
  mutate(tot = sum(obs),
         exp = 0.5*tot, 
         test = sum((obs-exp)^2/exp),
         pval = 1-pchisq(test, df = length(unique(mortcontdata$Season))-1))

mortcontdata
#dry-crushed -> wet-crushed: chi = 0.091: p = 0.763
#dry-empty -> wet-empty: chi = 9.966: p = 0.002
#dry-missing -> wet-missing: Chi = 7.76: p = 0.005

#assumptions look good 

##vertebrate predators

predatordata_summary <- predatordata %>% 
  filter(juvcray != 0) %>% 
  mutate(verttype = if_else(SpeciesCode == "Sirlac", true = "Siren",
                            false = "fish")) %>% 
  filter(SiteCode == "M2" | SiteCode == "M4") %>% 
  filter(Phylum == "Chordata") %>% 
  group_by(Season, verttype) %>% 
  summarise(obs = n()) %>% 
  mutate(rtot = sum(obs)) %>% 
  ungroup() %>% 
  group_by(verttype) %>% 
  mutate(ctot = sum(obs)) %>% 
  ungroup() %>% 
  mutate(tot = sum(obs),
         exp = (rtot*ctot)/tot,
         test = sum((obs-exp)^2/exp),
         pval = 1-pchisq(test, df = 1))

predatordata_summary

#seasonal differences
predatordata_summary <- predatordata %>% 
  filter(juvcray != 0) %>% 
  filter(SiteCode == "M2" | SiteCode == "M4") %>% 
  filter(Phylum == "Chordata") %>%
  mutate(verttype = if_else(SpeciesCode == "Sirlac", true = "Siren",
                            false = "fish")) %>% 
  filter(verttype == "Siren") %>% 
  group_by(Season) %>% 
  summarise(obs = n()) %>% 
  mutate(tot = sum(obs),
         exp = 0.5*tot,
         test = sum((obs-exp)^2/exp),
         pval = 1-pchisq(test, df = 1))

predatordata_summary

#dry-fish -> wet-fish: chi = 1.690: p = 0.194
#dry-siren -> wet-siren: chi 17.2: p < 0.001



predatordata_summary <- predatordata %>% 
  filter(juvcray != 0) %>% 
  mutate(verttype = if_else(SpeciesCode == "Sirlac", true = "Siren",
                            false = "fish")) %>% 
  filter(SiteCode == "M2" | SiteCode == "M4") %>% 
  filter(Phylum == "Arthropoda") %>% 
  group_by(Season, SpeciesCode) %>% 
  summarise(obs = n()) %>% 
  mutate(rtot = sum(obs)) %>% 
  ungroup() %>% 
  group_by(SpeciesCode) %>% 
  mutate(ctot = sum(obs)) %>% 
  ungroup() %>% 
  mutate(tot = sum(obs),
         exp = (rtot*ctot)/tot,
         test = sum((obs-exp)^2/exp),
         pval = 1-pchisq(test, df = 1)) 

predatordata_summary

#seasonal differences
predatordata_summary <- predatordata %>% 
  filter(juvcray != 0) %>% 
  filter(SiteCode == "M2" | SiteCode == "M4") %>% 
  filter(Phylum == "Arthropoda") %>%
  filter(SpeciesCode == "Profal") %>% 
  group_by(Season) %>% 
  summarise(obs = n()) %>% 
  mutate(tot = sum(obs),
         exp = 0.5*tot,
         test = sum((obs-exp)^2/exp),
         pval = 1-pchisq(test, df = 1))

predatordata_summary

#dry-belspp -> wetbelspp: chi = 6.545: p = 0.011
#dry-profal -> wetprofal: chi = 12.4: p < 0.001