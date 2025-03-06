#### this script is to build isocline, estimate the parameters,  plot
#the parameters on the isocline, plot the different reproductive conditions used
#to build the isocline
#---------------
####libraries
#---------------

library(lme4)
library(lmerTest)
library(MuMIn)
library(patchwork)

#-----------------------------
####read in the threshold data from script 002####
#----------------------------

threshold.dataM2 <- read_csv(here("Pomacea/Isocline_manuscript/data","threshold100_data_2size.csv"))
threshold.dataM1 <- read_csv(here("Pomacea/Isocline_manuscript/data","threshold100_deep_data_2size.csv"))
threshold.dataopt<- read_csv(here("Pomacea/Isocline_manuscript/data","threshold100_optimal_data_2sizes.csv"))

#####load in environmental used to create isocline data #####

environment_data <- read_xlsx(here("Pomacea/Isocline_manuscript/data","EnvrionmentalData_M2&M1_1-1-20--12-31-20.xlsx"),sheet = 2)   

environment_data <- environment_data %>% 
  mutate(Depth_M1_cm = (Depth_M1_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         Depth_M2_cm = (Depth_M2_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         mon = months(environment_data$Date))        

#-------------------------------
####for loops to find thresholds####
#-------------------------------

#M2

ks <- as.character(seq(from = 0.01, to =  0.09, by = 0.005))

isodat_M2 <- tibble(k = as.character(ks),
                   Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_M2$k)) {
  
  temp <- threshold.dataM2 %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_M2$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}

#m3

isodat_M1 <- tibble(k = as.character(ks),
                    Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_M1$k)) {
  
  temp <- threshold.dataM1 %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_M1$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}

#optimal

isodat_opt <- tibble(k = as.character(ks),
                    Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_M2$k)) {
  
  temp <- threshold.dataopt %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_opt$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}

#------------------------------------
###empirical estimates of parameters####
#------------------------------------

###growth parameters old estimates

#fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata[growthdata$Season == "dry",])
#model_summ <- summary(fit_me)  #results suggest a nonsignificant effect on the treatment

#dry_est <- model_summ$coefficients[1,1]

#dry_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

#wet season LILA
#fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata[growthdata$Season == "wet",])
#model_summ <- summary(fit_me)

#wet_est <- model_summ$coefficients[1,1]

#wet_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

#combined LILA
#fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata)
#model_summ <- summary(fit_me)

#comb_est <- model_summ$coefficients[1,1]

#comb_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

LILA_k <- growthdata %>% 
  #filter(Treatment == "0%_exposure") %>% 
  filter(Start_mm < 6) |> 
  filter(Cell != "M3") |> 
  drop_na(k) |> 
  mutate(Cell = case_when(Cell == "M2"~"LILA",
                          Cell == "M4"~"LILA",
                          Cell == "WCA02"~"WCA02")) |> 
  group_by(Cell,Season) %>% 
  summarise(k_ave = mean(k, na.rm = T),
            n_obs = n(),
            k_sd = sd(k, na.rm = T)) %>% 
  mutate(k_se = k_sd/ sqrt(n_obs),
         upp = k_ave + (1.96*k_se),
         low = k_ave - (1.96*k_se))

####WCA Growth Rate###
#regression
growthvsTPregression <- lmer(Growth_length~species + TP + species:TP + (1| year),
                             data = growth.summ)

summary(growthvsTPregression)

b0 <- -0.0414728+0.3267086
b1 <- 0.0026002-0.0022298

#WCA old estimate

#WCA_TP_par <- WCA_TP %>% 
#  group_by(Site) %>% 
#  summarise(n_obs = n(),
#            TP_ave = mean(TP),
#            sd_ave = sd(TP)) %>% 
#  mutate(se = sd_ave/(sqrt(n_obs)),
#         upp = TP_ave + 1.96*se,
#         low = TP_ave - 1.96*se,
#         k = b0+b1*TP_ave,
#         k_upp = b0+b1*upp,
#         k_low = b0+b1*low,
#         season = "wet",
#         Site = if_else(Site == "WCA2", true = "WCA02",
#                        false = if_else(Site == "WCA3",
#                                        true = "WCA03",
#                                        false = "error")))
  

#combine all estimates into one

k <- tibble(Site = c("LILA","LILA","Site 2","Site 2","Site 3"),
             Season = c("dry","wet","dry","wet","wet"),
             k = c(LILA_k$k_ave,0.058528084,0.048985688),
             k_upp =as.double(c(LILA_k$upp, 0.059445,0.050099)),
             k_low = as.double(c(LILA_k$low,0.057604,0.047855)))
k

######CJS survival estimates####

CJS <- tetherdata %>%
  filter(length < 10) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m"),
         Site = if_else(wetland == "M2"|wetland == "M4",
                        true = "LILA", false = wetland)) %>% 
  group_by(Site,season, fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(Site,season) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         predator = "with",
         Site = case_when(Site == "WCA03"~"Site 3",
                          Site == "WCA02"~"Site 2",
                          Site == "LILA"~"LILA")) %>% 
  filter(fate == "s") %>% 
  dplyr::select(-fate,-sum.fate,-tot)



#####CJS w/o predators#### 

CJS_encl <- enclosuredata %>% 
  group_by(season) %>% 
  summarise(ave = mean(proportion),
            n = n(),
            sd = sd(proportion)) |> 
  mutate( se = sd/sqrt(n),
            upp = ave + (1.96*se),
            low = ave - (1.96*se),
            upp = if_else(upp >1, true = 1.00, false = upp),
            Site = "LILA",
            predator = "without") %>% 
  dplyr::select(Site, season,ave,upp,low,predator) |> 
  rename(CJS = ave)

CJS_encl

CJS <- CJS %>% 
  bind_rows(CJS_encl) |> 
  rename(CJS_upp = upp,
         CJS_low = low)

CJS

k <- k %>% 
  rename(season = Season)

#combine all inot one file called parameters####

parameter <- CJS %>% 
  left_join(k, by = c("Site","season"))

weights <- tibble(season = c("dry","wet"),
                  weight = c(0.7,0.3))

parameter_combined <- parameter |>
  gather(CJS,CJS_upp,CJS_low,k,k_upp, k_low, key = "parameter", value = value) |> 
  left_join(weights, by = "season") |> 
  mutate(weighted_value = weight*value) |> 
  group_by(Site,predator,parameter) |> 
  summarise(weighted_value = sum(weighted_value)) |> 
  mutate(season = "combined") |> 
  spread(key = parameter,value = weighted_value)

parameter_combined

parameter <- parameter |> 
  bind_rows(parameter_combined) |> 
  filter(k_upp > 0.0151)

parameter
#------------------------------------
####create the isocline####
#------------------------------------

p20 <- isodat_M2 %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
 # geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
 #            fill = "dark red")+
  geom_smooth(color = "#999999", size = 1.5, linetype = 1, se = F)+
  #geom_smooth(data = isodat_opt, color = "#999999", linewidth = 1.5, linetype = 1, se = F)+
  geom_smooth(data = isodat_M1, color = "black", linewidth = 1.5, linetype = 1, se = F)+
  geom_pointrange(data = parameter |> filter(Site != "Site 3") |> filter(predator == "with"),
                    aes(x = CJS, y = k, ymin = k_low, ymax = k_upp,
                                        color = season, shape = Site), show.legend = F)+
  geom_pointrange(data = parameter |> filter(Site != "Site 3")|> filter(predator == "with"),
                  aes(x = CJS, y = k, xmin = CJS_low, xmax = CJS_upp,
                                       color = season, shape = Site),show.legend = F)+
  geom_point(data = parameter |> filter(Site != "Site 3")|> filter(predator == "with"),
             aes(x = CJS, y = k,color = season, shape = Site), size = 3)+
  theme_classic()+
  coord_flip()+
  scale_color_manual(values = c("darkolivegreen","tan4","steelblue4"))+
  scale_y_continuous(breaks = c(0.015,0.03,0.045,0.06,0.075), expand = c(0,0))+
  scale_x_continuous(limits = c(0.69,1.04), breaks = c(0.75,0.8,0.85,0.9,0.95,1.00),
                     expand = c(0,0))+
  labs(x ="Survival (< 10 mm SL)", y ="Growth (k)")+
  annotate(geom = "text", x = 0.73, y = 0.018, label = "Declining", size = 8)+
  annotate(geom = "text", x = 0.925, y = 0.075, label = "Increasing", size = 8)+
  #annotate(geom = "text", x = 0.993, y = 0.05, label = "(model parameters)",
  #       size = 3, color = "dark red")+
  #annotate(geom = "text", x = 1.02, y = 0.030, label = "without predators")+
  #annotate(geom = "text", x = 0.82, y = 0.047, label = "with")+
  #annotate(geom = "text", x = 0.80, y = 0.047, label = "predators")+
  theme(legend.position = c(0.52,0.13),
        legend.box = "horizontal",
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20))

#--------------------------------------------
#####without empirical measurements#####


p21 <- isodat_M2 %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
  # geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
  #            fill = "dark red")+
  geom_smooth(color = "black", size = 1.5, linetype = 1, se = F)+
  #geom_smooth(data = isodat_opt, color = "#999999", linewidth = 1.5, linetype = 1, se = F)+
  #geom_smooth(data = isodat_M1, color = "black", linewidth = 1.5, linetype = 1, se = F)+
  #geom_pointrange(data = parameter, aes(x = CJS, y = k, ymin = k_low, ymax = k_upp,
 #                                       color = season, shape = Site), show.legend = F)+
  #geom_pointrange(data = parameter, aes(x = CJS, y = k, xmin = CJS_low, xmax = CJS_upp,
  #                                      color = season, shape = Site),show.legend = F)+
  #geom_point(data = parameter, aes(x = CJS, y = k,color = season, shape = Site), size = 3)+
  theme_classic()+
  coord_flip()+
  scale_color_manual(values = c("darkolivegreen","tan4","steelblue4"))+
  scale_y_continuous(breaks = c(0.015,0.03,0.045,0.06,0.075), expand = c(0,0))+
  scale_x_continuous(limits = c(0.70,1.04), breaks = c(0.75,0.8,0.85,0.9,0.95,1.00),
                     expand = c(0,0))+
  labs(x ="Survival (< 10 mm SL)", y ="Growth (k)")+
  annotate(geom = "text", x = 0.75, y = 0.03, label = "Declining", size = 5)+
  annotate(geom = "text", x = 0.95, y = 0.072, label = "Increasing", size = 5)+
  #annotate(geom = "text", x = 0.993, y = 0.05, label = "(model parameters)",
  #       size = 3, color = "dark red")+
  #annotate(geom = "text", x = 1.02, y = 0.030, label = "without predators")+
  #annotate(geom = "text", x = 0.82, y = 0.047, label = "with")+
  #annotate(geom = "text", x = 0.80, y = 0.047, label = "predators")+
  theme(legend.position = c(0.52,0.13),
        legend.box = "horizontal",
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))

#environmental data plot

p22 <- environment_data %>% 
  gather(Depth_Opt,Depth_M2_cm, Depth_M1_cm, key = "scenario", value = "Depth_cm") %>% 
  filter(scenario != "Depth_Opt") |> 
  ggplot(aes(x = Date, y = Depth_cm, color = scenario))+
  geom_rect(aes(xmin = as.POSIXct(as_date("2020-02-01")),
                xmax = as.POSIXct(as_date("2020-08-31")),
                ymin = -Inf, ymax = +Inf), fill = "tan", 
            show.legend = F,  color = "tan", alpha = 0.01)+
  geom_line(linewidth = 1.5, linetype = 1, show.legend = T)+
  labs(y = "Depth (cm)")+
  theme_classic()+
  scale_color_manual(values = c("black","#999999"),
                     labels = c("Poor Reproduction","Good Reproduction"))+
  theme(legend.position = c(0.38,0.85),
        legend.key = element_rect(fill = NA),
        #legend.position = "top",
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 60, vjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"))


p23 <- environment_data %>% 
  gather(Temp_nat,Temp_opt, key = "scenario", value = "Temp_c") %>% 
  ggplot(aes(x = Date, y = Temp_c, color = scenario))+
  geom_line(linewidth = 1.5, linetype = 1)+
  scale_color_manual(values = c("black","#999999"),
                     labels = c("Natural","Optimized"))+
  theme_classic()+
  labs(y = "Temperature (\u00B0C)")+
  theme(legend.position = c(0.5,0.3),
        legend.title = element_blank())

patch.isocline <- (p22+p21)
patch.isocline.annotate<- patch.isocline +
  plot_annotation(tag_levels = "A",tag_suffix = ")")

patch.isocline.annotate

ggsave(here("Pomacea/Isocline_manuscript/out","fig5_isocline.pdf"),
       p20, device = pdf,
       units = "in", width = 8, height = 8)

ggsave(here("Pomacea/Isocline_manuscript/out","figA3-1_hydrologic.png"),
       p22, device = png,
       units = "in", width = 5, height = 4)



ggsave(here("Pomacea/Isocline_manuscript/out/pdf","fig2_isoclinealone.pdf"),
       p21, device = "pdf",
       units = "in", width = 5, height = 4)

####optimized reproduction

p24 <- isodat_M2 %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
  # geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
  #            fill = "dark red")+
  geom_smooth(color = "#333333", size = 1.5, linetype = 1, se = F)+
  geom_smooth(data = isodat_opt, color = "#999999", linewidth = 1.5, linetype = 1, se = F)+
  geom_smooth(data = isodat_M1, color = "black", linewidth = 1.5, linetype = 1, se = F)+
  geom_pointrange(data = parameter, aes(x = CJS, y = k, ymin = k_low, ymax = k_upp,
                                        color = season, shape = Site), show.legend = F)+
  geom_pointrange(data = parameter, aes(x = CJS, y = k, xmin = CJS_low, xmax = CJS_upp,
                                        color = season, shape = Site),show.legend = F)+
  geom_point(data = parameter, aes(x = CJS, y = k,color = season, shape = Site), size = 3)+
  theme_classic()+
  coord_flip()+
  scale_color_manual(values = c("darkolivegreen","tan4","steelblue4"))+
  scale_y_continuous(breaks = c(0.015,0.03,0.045,0.06,0.075), expand = c(0,0))+
  scale_x_continuous(limits = c(0.69,1.04), breaks = c(0.75,0.8,0.85,0.9,0.95,1.00),
                     expand = c(0,0))+
  labs(x ="Cumulative Juvenile Survival", y ="Growth (k)")+
  annotate(geom = "text", x = 0.73, y = 0.018, label = "Declining", size = 8)+
  annotate(geom = "text", x = 0.925, y = 0.075, label = "Increasing", size = 8)+
  #annotate(geom = "text", x = 0.993, y = 0.05, label = "(model parameters)",
  #       size = 3, color = "dark red")+
  #annotate(geom = "text", x = 1.02, y = 0.030, label = "without predators")+
  #annotate(geom = "text", x = 0.82, y = 0.047, label = "with")+
  #annotate(geom = "text", x = 0.80, y = 0.047, label = "predators")+
  theme(legend.position = c(0.52,0.13),
        legend.box = "horizontal",
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20))


ggsave(here("Pomacea/Isocline_manuscript/out","FigS5_isoclinefull.png"),
       p24, device = png,
       units = "in", width = 8, height = 8)

p25 <- isodat_M2 %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
  # geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
  #            fill = "dark red")+
  geom_smooth(color = "#666666", size = 1.5, linetype = 1, se = F)+
  #geom_smooth(data = isodat_opt, color = "#999999", linewidth = 1.5, linetype = 1, se = F)+
  geom_smooth(data = isodat_M1, color = "black", linewidth = 1.5, linetype = 1, se = F)+
  #geom_pointrange(data = parameter, aes(x = CJS, y = k, ymin = k_low, ymax = k_upp,
  #                                       color = season, shape = Site), show.legend = F)+
  #geom_pointrange(data = parameter, aes(x = CJS, y = k, xmin = CJS_low, xmax = CJS_upp,
  #                                      color = season, shape = Site),show.legend = F)+
  #geom_point(data = parameter, aes(x = CJS, y = k,color = season, shape = Site), size = 3)+
  theme_classic()+
  coord_flip()+
  scale_color_manual(values = c("darkolivegreen","tan4","steelblue4"))+
  scale_y_continuous(breaks = c(0.015,0.03,0.045,0.06,0.075), expand = c(0,0))+
  scale_x_continuous(limits = c(0.70,1.04), breaks = c(0.75,0.8,0.85,0.9,0.95,1.00),
                     expand = c(0,0))+
  labs(x ="Survival (< 10 mm SL)", y ="Growth (k)")+
  annotate(geom = "text", x = 0.75, y = 0.03, label = "Declining", size = 5)+
  annotate(geom = "text", x = 0.95, y = 0.072, label = "Increasing", size = 5)+
  #annotate(geom = "text", x = 0.993, y = 0.05, label = "(model parameters)",
  #       size = 3, color = "dark red")+
  #annotate(geom = "text", x = 1.02, y = 0.030, label = "without predators")+
  #annotate(geom = "text", x = 0.82, y = 0.047, label = "with")+
  #annotate(geom = "text", x = 0.80, y = 0.047, label = "predators")+
  theme(legend.position = c(0.52,0.13),
        legend.box = "horizontal",
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))

ggsave(here("Pomacea/Isocline_manuscript/out","fig2_isoclineconditions.png"),
       p25, device = png,
       units = "in", width = 5, height = 4)
