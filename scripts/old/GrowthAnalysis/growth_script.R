rm(list = ls())

####libraries####

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(cowplot)
library(gridExtra)

####load data####

#growthdata
growthdata <- read_excel("Growth_DataSheet_v5.xlsx", sheet = 2)
#treatment codes
treatmentcode <- read_excel("Growth_DataSheet_v5.xlsx", sheet = 4)
#maculata length
maclen <- read_excel("Growth_DataSheet_v5.xlsx", sheet = 5) %>% 
  group_by(Cage) %>% 
  summarise(ave.maclength = mean(mac_size_mm, na.rm = T))

#check data
growthdata
table(growthdata$Cell)
table(growthdata$Cage)
table(growthdata$Color)
table(growthdata$Fate)
table(is.na(growthdata$Start_mm))
table(is.na(growthdata$End_mm))

#add size classes and treatments

growthdata <- growthdata %>% 
  left_join(treatmentcode, by = c("Cage","Season")) 

table(growthdata$SizeClass)
table(growthdata$Code)
summary(growthdata$SGR)


####Exposure experiment results dry season####

fit_me <- lmer(SGR ~ Start_mm*Code + (1|Cage), data = growthdata[growthdata$Season == "dry",])
summary(fit_me)

treatment_growthplot_dry <- growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "dry") %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = if_else(Treatment == "100%_exposure",
                                             true = "100% exposure",
                                             false = if_else(Treatment == "11%_exposure",
                                                             true = "11% exposure",
                                                             false = "22% exposure")))) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = "Starting Length (mm)")+
  scale_y_continuous(limits = c(0.000,0.015),
                     breaks = c(0.000,0.005,0.010,0.015))+
  scale_x_continuous(limits = c(3.0,10.0),
                     breaks = c(2.5,5.0,7.5,10.0))+
  scale_color_manual(values = c("black", "#333333","#666666", "black"))

ggsave(filename = "C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/SizeDependentMortality_manuscript/Figures&Tables/FigureS1.1_dryseasonexperiment.png",
       plot = treatment_growthplot_dry, device = "png",
       units = "in", height = 4, width = 6)

####Exposure experiment results wet seasons

TP_data <- read_csv("table_cagecharact.csv")

t.test(TP_data$TP[TP_data$Treatment == "0%_exposure"],
       TP_data$TP[TP_data$Treatment == "17%_exposure"])

treatment_growthplot_a <- TP_data %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Treatment, y = TP))+
  theme_classic()+
  geom_boxplot()+
  labs(x = NULL,
       title = "A)")+
  annotate(geom = "text", label = expression(paste(italic("t"), " = -0.367")), x = "0% exposure", y = 400)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.727")), x = "0% exposure", y = 365)

fit_me <- lmer(SGR ~ Start_mm*Code + (1|Cage), data = growthdata[growthdata$Season == "wet",])
summary(fit_me)


treatment_growthplot_b <- growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "wet") %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = NULL,
       title = "B)")+
  scale_y_continuous(limits = c(0.001,0.025),
                     breaks = c(0.005,0.010,0.015,0.020,0.025))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("black", "#333333","#666666"))+
  theme(legend.position = c(0.8,0.85),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.background = element_rect(colour = "black", linetype = "solid"))+
  annotate(geom = "text", label = "interaction:", x = 5.0 , y =0.006)+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 1.270")), x = 5.0, y = 0.004)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.212")), x = 5.0, y = 0.002)


t.test(TP_data$TP[TP_data$Treatment == "0%_exposure"],
       TP_data$TP[TP_data$Treatment == "17%_exposure" & TP_data$Cage != 5])  

treatment_growthplot_c <- TP_data %>% 
  filter(Cage != 5) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Treatment, y = TP))+
  theme_classic()+
  geom_boxplot()+
  labs(x = NULL,
       title = "C)")+
  annotate(geom = "text", label =expression(paste(italic("t"), " = 1.384")), x = "0% exposure", y = 50)+
  annotate(geom = "text", label =expression(paste(italic("p"), " = 0.204")), x = "0% exposure", y = 35)

fit_me <- lmer(SGR ~ Start_mm*Code + (1|Cage), data = growthdata[growthdata$Season == "wet"&growthdata$Cage != 5,])
summary(fit_me)

treatment_growthplot_d <- growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "wet") %>% 
  filter(Cage != 5) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = NULL,
       title = "D)")+
  scale_y_continuous(limits = c(0.001,0.025),
                     breaks = c(0.005,0.010,0.015,0.020,0.025))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("black", "#333333","#666666"))+
  theme(legend.position = c(0.8,0.85),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.background = element_rect(colour = "black", linetype = "solid"))+
  annotate(geom = "text", label = "interaction:", x = 5.0 , y =0.006)+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 2.089")), x = 5.0, y = 0.004)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.044")), x = 5.0, y = 0.002)

t.test(TP_data$TP[TP_data$Treatment == "0%_exposure"],
       TP_data$TP[TP_data$Treatment == "17%_exposure" & TP_data$Cage != 5 & TP_data$Cage != 3])  

treatment_growthplot_e <- TP_data %>% 
  filter(Cage != 5 & Cage != 3) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Treatment, y = TP))+
  theme_classic()+
  labs(title = "E)")+
  geom_boxplot()+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 0.914")), x = "17% exposure", y = 160)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.390")), x = "17% exposure", y = 150)

fit_me <- lmer(SGR ~ Start_mm*Code + (1|Cage), data = growthdata[growthdata$Season == "wet"&growthdata$Cage != 5 & growthdata$Cage !=3,])
summary(fit_me)

treatment_growthplot_f <- growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "wet") %>% 
  filter(Cage !=5 & Cage != 3) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = "Starting Length (mm)",
       title = "F)")+
  scale_y_continuous(limits = c(0.001,0.025),
                     breaks = c(0.005,0.010,0.015,0.020,0.025))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("black", "#333333","#666666"))+
  theme(legend.position = c(0.8,0.85),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.background = element_rect(colour = "black", linetype = "solid"))+
  annotate(geom = "text", label = "interaction:", x = 5.0 , y =0.006)+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 1.402")), x = 5.0, y = 0.004)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.170")), x = 5.0, y = 0.002)


treatment_growthplot <- grid.arrange(treatment_growthplot_a,treatment_growthplot_b,treatment_growthplot_c,
             treatment_growthplot_d,treatment_growthplot_e, treatment_growthplot_f
             )


ggsave(filename = "C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/SizeDependentMortality_manuscript/Figures&Tables/FigureS1.2_TPdriveexperimentresults.png",
       plot = treatment_growthplot, device = "png",
       units = "in", height = 8, width = 7)



#####mixed effect models#####


Dry_fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata[growthdata$Season == "dry",])
Dry_fit <- lm(SGR~Start_mm, data = growthdata[growthdata$Season == "dry",])

summary.lm(Dry_fit)
summary(Dry_fit_me)
pseudoR2 <- MuMIn::r.squaredGLMM(Dry_fit_me)  ###trigamma is recommended
pseudoR2

Dry <- confint(Dry_fit_me, parm = "(Intercept)", level = 0.95)
Dry <- as_tibble(Dry) %>% 
  mutate(season = "dry") 

Wet_fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata[growthdata$Season == "wet",])
Wet_fit <- lm(SGR~Start_mm, data = growthdata[growthdata$Season == "wet",])

summary.lm(Wet_fit)
summary(Wet_fit_me)

pseudoR2 <- MuMIn::r.squaredGLMM(Wet_fit_me)  ###trigamma is recommended
pseudoR2

Wet <- confint(Wet_fit_me, parm = "(Intercept)", level = 0.95)
Wet <- as_tibble(Wet) %>% 
  mutate(season = "wet")

Comb_fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata)
Comb_fit <- lm(SGR~Start_mm, data = growthdata)

summary.lm(Comb_fit)
summary(Comb_fit_me)

pseudoR2 <- MuMIn::r.squaredGLMM(Comb_fit_me)  ###trigamma is recommended
pseudoR2

Comb <- confint(Comb_fit_me, parm = "(Intercept)", level = 0.95)
Comb<- as_tibble(Comb) %>% 
  mutate(season = "combined")

growth.par <- Dry %>% 
  bind_rows(Wet) %>% 
  bind_rows(Comb) %>% 
  rename(upp = `97.5 %`,
         low = `2.5 %`) %>% 
  mutate(value= c(0.01113,0.02069,0.01587),
         parameter = "kgrowth")

write_csv(growth.par, file = "growth_est.csv")


#visualize results
growthplot <- growthdata %>% 
  filter(Fate == "alive") %>% 
  ggplot(aes(x = Start_mm, y = SGR))+
  geom_smooth(aes(linetype = Season),
              method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_smooth(method = "lm", fill = "#999999", color = "black")+
  geom_point(aes(color = Season, shape = Season))+
  theme_classic()+
  labs(y = "SGR (protional daily growth)",
       x = "Starting Length (mm)")+
  scale_y_continuous(limits = c(0.001,0.03),
                     breaks = c(0.005,0.010,0.015,0.020,0.025,0.03))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("black", "#333333"))+
  scale_linetype_manual(values = c("dotted", "dashed"))+
  annotate(geom = "text", label = "dry: y = -0.0004x + 0.0111", y = 0.030, x =11.0)+
  annotate(geom = "text", label = "wet: y = -0.0010x + 0.0207", y = 0.028, x =11.0)+
  annotate(geom = "text", label = "combined: y = -0.0007x + 0.0159", y = 0.026, x =10.52)

ggsave(filename = "C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/SizeDependentMortality_manuscript/Figures&Tables/FigureS1.3_seasonalgrowthparameters.png",
       plot = growthplot, device = "png",
       units = "in", height = 4, width = 6)

####cage survival####

survivaldata <- growthdata%>% 
  filter(Fate == "alive" | Fate == "dead") %>% 
  filter(Cage != 5) %>% 
  select(Season,Fate, Cell,Cage,Color,Start_mm, End_mm, Fate) 

drysurvival <- survivaldata %>% 
  filter(Season == "dry") 

drysurvival <- drysurvival %>% 
  mutate(exp_SGR = predict(Dry_fit_me, newdata = drysurvival, allow.new.levels = T),
         duration = round((log(End_mm)-log(Start_mm))/exp_SGR),
         duration = if_else(Fate == "alive", true = 27, false = duration),
         duration = if_else(duration > 30, true = 24, false = duration)) 

wetsurvival <- survivaldata %>% 
  filter(Season == "wet") 

wetsurvival <- wetsurvival %>% 
  mutate(exp_SGR = predict(Wet_fit_me, newdata = wetsurvival, allow.new.levels = T),
         duration = round((log(End_mm)-log(Start_mm))/exp_SGR),
         duration = if_else(Fate == "alive", true = 22, false = duration)) 

survivaldata <- drysurvival %>% 
  bind_rows(wetsurvival)

write_csv(survivaldata, file = "predatorfree_survival.csv")
