rm(list = ls())

#####libraries#####

library(tidyverse)
library(ggrepel)
library(cowplot)
library(ggpubr)
####load data####


threshold.dat <- read_csv("threshold100_data_2size.csv")
threshold.dat <- threshold.dat %>% 
  mutate(k = as.character(k))

ks <- as.character(seq(from = 0.01, to =  0.09, by = 0.005))
isodat_g <- tibble(k = as.character(ks),
                 Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_g$k)) {
  
  temp <- threshold.dat %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_g$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])

}

para_est <- read.csv("C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/DornLabMeeting/Tethering/para_data.csv")

sw.para_est <- para_est %>% 
  filter(parameter == "CJS-with") %>% 
  rename(low.cjs.w = low,
         upp.cjs.w = upp,
         CJS.w = value) %>% 
  select(-parameter)

g.para_est <- para_est %>% 
  filter(parameter == "kgrowth") %>% 
  rename(low.g = low,
         upp.g = upp,
         kgrowth = value) %>% 
  select(-parameter)

swo.para_est <- para_est %>% 
  filter(parameter == "CJS-without") %>% 
  rename(low.cjs.wo = low,
         upp.cjs.wo = upp,
         CJS.wo = value) %>% 
  select(-parameter)

para_est <- g.para_est %>% 
  left_join(sw.para_est, by = "season") %>% 
  left_join(swo.para_est, by = "season")
  
thresholdplot_b <- isodat_g %>% 
ggplot(aes(x = Sint, y  = as.numeric(k)))+
  geom_smooth(color = "#333333", size = 1.5, linetype = 1, se = F)+
  geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
             fill = "dark red")+
  geom_pointrange(aes(y = kgrowth, x = CJS.w,
                    xmin = low.cjs.w, xmax = upp.cjs.w), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.w,
                    ymin = low.g, ymax = upp.g), data = para_est)+
  geom_point(aes(y = kgrowth, x = CJS.w), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.wo,
                      xmin = low.cjs.wo, xmax = upp.cjs.wo), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.wo,
                      ymin = low.g, ymax = upp.g), data = para_est)+
  geom_point(aes(y = kgrowth, x = CJS.wo), data = para_est)+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(breaks = c(0.01,0.03,0.05,0.07,0.09))+
  scale_x_continuous(limits = c(0.65,1.04), breaks = c(0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.00))+
  labs(x ="Cumulative Juvenile Survival", y = NULL, title = "B) Good Reproductive Conditions")+
  annotate(geom = "text", x = 0.75, y = 0.045, label = "Population declining")+
  annotate(geom = "text", x = 0.925, y = 0.07, label = "Population increasing")+
  annotate(geom = "text", x = 0.993, y = 0.05, label = "(EVERSNAIL parameters)",
           size = 3, color = "dark red")+
  annotate(geom = "text", x = 1.04, y = 0.022, label = "without predators")+
  annotate(geom = "text", x = 0.83, y = 0.028, label = "with")+
  annotate(geom = "text", x = 0.81, y = 0.028, label = "predators")+
  annotate(geom = "text", x = 1.013, y = 0.011, label = "D")+
  annotate(geom = "text", x = 1.017, y = 0.016, label = "C")+
  annotate(geom = "text", x = 1.02, y = 0.021, label = "W")+
  annotate(geom = "text", x = 0.804, y = 0.0134, label = "D")+
  annotate(geom = "text", x = 0.834, y = 0.0181, label = "C")+
  annotate(geom = "text", x = 0.913, y = 0.024, label = "W")
  
thresholdplot_b

ggsave("figure3_thresholdplot.png", plot = thresholdplot_a, units = "in", width = 6, height = 5.5,
       device = png())

##############Deep Data#############

threshold.dat <- read_csv("threshold100_deep_data_2size.csv")
threshold.dat <- threshold.dat %>% 
  mutate(k = as.character(k))

ks <- as.character(seq(from = 0.01, to =  0.09, by = 0.005))
isodat_p <- tibble(k = as.character(ks),
                 Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_p$k)) {
  
  temp <- threshold.dat %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_p$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}


thresholdplot_a <- isodat_p %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
  geom_smooth(color = "#333333", size = 1.5, linetype = 1, se = F)+
  geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
             fill = "dark red")+
  geom_pointrange(aes(y = kgrowth, x = CJS.w,
                      xmin = low.cjs.w, xmax = upp.cjs.w), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.w,
                      ymin = low.g, ymax = upp.g), data = para_est)+
  geom_point(aes(y = kgrowth, x = CJS.w), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.wo,
                      xmin = low.cjs.wo, xmax = upp.cjs.wo), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.wo,
                      ymin = low.g, ymax = upp.g), data = para_est)+
  geom_point(aes(y = kgrowth, x = CJS.wo), data = para_est)+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(breaks = c(0.01,0.03,0.05,0.07,0.09))+
  scale_x_continuous(limits = c(0.65,1.04), breaks = c(0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.00))+
  labs(x ="Cumulative Juvenile Survival", y =NULL, title = "A) Poor Reproductive Conditions")+
  annotate(geom = "text", x = 0.75, y = 0.045, label = "Population declining")+
  annotate(geom = "text", x = 0.925, y = 0.07, label = "Population increasing")+
  annotate(geom = "text", x = 0.993, y = 0.05, label = "(EVERSNAIL parameters)",
           size = 3, color = "dark red")+
  annotate(geom = "text", x = 1.04, y = 0.022, label = "without predators")+
  annotate(geom = "text", x = 0.83, y = 0.028, label = "with")+
  annotate(geom = "text", x = 0.81, y = 0.028, label = "predators")+
  annotate(geom = "text", x = 1.013, y = 0.011, label = "D")+
  annotate(geom = "text", x = 1.017, y = 0.016, label = "C")+
  annotate(geom = "text", x = 1.02, y = 0.021, label = "W")+
  annotate(geom = "text", x = 0.804, y = 0.0134, label = "D")+
  annotate(geom = "text", x = 0.834, y = 0.0181, label = "C")+
  annotate(geom = "text", x = 0.913, y = 0.024, label = "W")
thresholdplot_a

ggsave("figure3_thresholdplot_deep.png", plot = thresholdplot_b, units = "in", width = 6, height = 5.5,
       device = png())


#############Optimal Data#########

threshold.dat <- read_csv("threshold100_optimal_data_2sizes.csv")
threshold.dat <- threshold.dat %>% 
  mutate(k = as.character(k))

ks <- as.character(seq(from = 0.01, to =  0.09, by = 0.005))
isodat_o <- tibble(k = as.character(ks),
                 Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_o$k)) {
  
  temp <- threshold.dat %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_o$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}


thresholdplot_c <- isodat_o %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
  geom_smooth(color = "#333333", size = 1.5, linetype = 1, se = F)+
  geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
             fill = "dark red")+
  geom_pointrange(aes(y = kgrowth, x = CJS.w,
                      xmin = low.cjs.w, xmax = upp.cjs.w), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.w,
                      ymin = low.g, ymax = upp.g), data = para_est)+
  geom_point(aes(y = kgrowth, x = CJS.w), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.wo,
                      xmin = low.cjs.wo, xmax = upp.cjs.wo), data = para_est)+
  geom_pointrange(aes(y = kgrowth, x = CJS.wo,
                      ymin = low.g, ymax = upp.g), data = para_est)+
  geom_point(aes(y = kgrowth, x = CJS.wo), data = para_est)+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(breaks = c(0.01,0.03,0.05,0.07,0.09))+
  scale_x_continuous(limits = c(0.65,1.04), breaks = c(0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.00))+
  labs(x ="Cumulative Juvenile Survival", y ="Growth (k)", title = "C) Optimized Reproductive Conditions")+
  annotate(geom = "text", x = 0.75, y = 0.045, label = "Population declining")+
  annotate(geom = "text", x = 0.925, y = 0.07, label = "Population increasing")+
  annotate(geom = "text", x = 0.993, y = 0.05, label = "(EVERSNAIL parameters)",
           size = 3, color = "dark red")+
  annotate(geom = "text", x = 1.04, y = 0.022, label = "without predators")+
  annotate(geom = "text", x = 0.83, y = 0.028, label = "with")+
  annotate(geom = "text", x = 0.81, y = 0.028, label = "predators")+
  annotate(geom = "text", x = 1.013, y = 0.011, label = "D")+
  annotate(geom = "text", x = 1.017, y = 0.016, label = "C")+
  annotate(geom = "text", x = 1.02, y = 0.021, label = "W")+
  annotate(geom = "text", x = 0.804, y = 0.0134, label = "D")+
  annotate(geom = "text", x = 0.834, y = 0.0181, label = "C")+
  annotate(geom = "text", x = 0.913, y = 0.024, label = "W")

thresholdplot_c

ggsave("figure3_thresholdplot_optimal.png", plot = thresholdplot_c, units = "in", width = 6, height = 5.5,
       device = png())

threshold_together <- plot_grid(thresholdplot_a,thresholdplot_b,thresholdplot_c, ncol = 1)

ggsave("figure3_thresholdplot_together.png", plot = threshold_together, units = "in", width = 4, height = 11,
       device = png())

######scatter plot of simulation data

library(readxl)
library(ggpubr)
library(gridExtra)

dat <- read_excel("2sizecombineddata.xlsx")

dat %>% 
  rename(CJS = Sint) %>% 
  ggplot(aes(x = k, y = rs, color = CJS))+
  geom_point()+
  scale_color_viridis_c()+
  theme_classic()+
  facet_wrap(~regime)+
  labs(x = "k growth", y = "intrisic rate of increase (r)")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "#666666")+
  scale_x_continuous(breaks = c(0.02,0.04,0.06,0.08))
  





############

###talk plot

##########


thresholdplot_talkplot <- isodat_g %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
  geom_smooth(data = isodat_o, color = "light blue", size = 1.5, linetype = 1, se = F)+
   geom_point(aes(x = (0.987*.987), y = 0.05), size = 6, shape = 21, color = "black",
             fill = "dark red")+
  geom_smooth(color = "midnight blue", size = 1.5, linetype = 1, se = F)+
  #geom_pointrange(aes(y = kgrowth, x = CJS.w,
 #                    xmin = low.cjs.w, xmax = upp.cjs.w), data = para_est, size = 1)+
  #geom_pointrange(aes(y = kgrowth, x = CJS.w,
  #                    ymin = low.g, ymax = upp.g), data = para_est, size = 1)+
  #geom_point(aes(y = kgrowth, x = CJS.w), data = para_est, shape = 22, size = 6, color = "black", fill = "#666666")+
  #geom_pointrange(aes(y = kgrowth, x = CJS.wo,
  #xmin = low.cjs.wo, xmax = upp.cjs.wo), data = para_est,size = 1)+
 # geom_pointrange(aes(y = kgrowth, x = CJS.wo,
  #                    ymin = low.g, ymax = upp.g), data = para_est,size = 1)+
 # geom_point(aes(y = kgrowth, x = CJS.wo), data = para_est,shape = 21, size = 6, color = "black", fill = "#666666")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(breaks = c(0.01,0.03,0.05,0.07,0.09))+
  scale_x_continuous(limits = c(0.65,1.04), breaks = c(0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.00))+
  labs(x ="Cumulative Juvenile Survival", y ="Growth (k)")
  #annotate(geom = "text", x = 0.75, y = 0.045, label = "Population declining")+
  #annotate(geom = "text", x = 0.925, y = 0.07, label = "Population increasing")+
 # annotate(geom = "text", x = 0.993, y = 0.05, label = "(EVERSNAIL parameters)",
  #         size = 6, color = "dark red")+
 # theme(axis.title = element_text(size = 24, face = "bold"),
 #       text = element_text(size = 18, face = "bold"))+
  #annotate(geom = "text", x = 1.04, y = 0.022, label = "without predators")+
  #annotate(geom = "text", x = 0.83, y = 0.028, label = "with")+
 # #annotate(geom = "text", x = 0.81, y = 0.028, label = "predators")+
 # annotate(geom = "text", x = 1.013, y = 0.011, label = "D")+
 # annotate(geom = "text", x = 1.017, y = 0.016, label = "C")+
 # annotate(geom = "text", x = 1.02, y = 0.021, label = "W")+
 # annotate(geom = "text", x = 0.804, y = 0.0134, label = "D")+
 # annotate(geom = "text", x = 0.834, y = 0.0181, label = "C")+
 # annotate(geom = "text", x = 0.913, y = 0.024, label = "W")


thresholdplot_talkplot

ggsave(filename = "thresholdtalk_plot.png", plot = thresholdplot_talkplot,
       units = "in", device = "png", width = 8, height = 8)
