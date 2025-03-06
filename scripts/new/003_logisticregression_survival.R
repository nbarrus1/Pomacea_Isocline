#this code is dedicated to the daily survival probability using logistic regression for the
#tethering data.

#-----------------------------------
####libraries######
#-----------------------------------

library(patchwork)
library(jpeg)
library(ggpubr)
library(qpcR)

#-----------------------------------
####data summary####
#-----------------------------------

#all data of snails less than 10 mm

tether.summ <- tetherdata %>% 
  filter(length < 10) %>%
  mutate(count = 1) %>% 
  group_by(fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  mutate(n = sum(sum.fate),
         rate = sum.fate/n)

tether.summ #the NA is from a snail that we couldn't check its fate because it got
#pulled off by us

#look at the data by season

tether.summ <- tetherdata %>%
  filter(length < 10) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m")) %>% 
  group_by(season, fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(season) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))))

tether.summ

#look at the data by location

tether.summ <- tetherdata %>%
  filter(length < 10) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m")) %>% 
  group_by(wetland,season, fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(wetland,season) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))))

tether.summ

#-------------------------------------------------------------
#####figure one plot#####
#-------------------------------------------------------------

trans_img <- readJPEG(here("Pomacea/Isocline_manuscript/pics","TetheringTransect2.jpg"))

p1 <- ggplot(iris, aes(Species, Sepal.Length))+
  background_image(trans_img)+
  theme(axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  labs(x =NULL, y = NULL)

rm(list = "trans_img")

p2 <- ggplot(iris, aes(Species, Sepal.Length))+
  #background_image(trans_img)+
  theme(axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  labs(x =NULL, y = NULL)
#tether_img <- readJPEG(here("Pomacea/Isocline_manuscript/pics","TetheredSnails.jpg"))

#p2 <- ggplot(iris, aes(Species, Sepal.Length))+
#  background_image(tether_img)+
#  theme(axis.text = element_blank(),
#        plot.margin = unit(c(0,0,0,0), "cm"))+
#  labs(x =NULL, y = NULL)

#rm(list = "tether_img")

p3 <- tetherdata %>% 
  filter(wetland == "M2"|wetland=="M4") %>% 
  ggplot(aes(x = length, y = survival, color = season, fill = season)) +
  theme_classic()+
  geom_smooth( method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), show.legend = F)+
  geom_smooth(method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), fill = NA)+
  coord_cartesian(ylim =  c(0.55, 1.00),
                  xlim = c(3,30))+
  labs(y = "Daily Survival Probability",
       x = "Shell Length (mm)")+
  scale_x_continuous(breaks = c(3,10,20,30))+
  #annotate(geom = "text", x = 5.5, y = 1, label = "A)", size = 6)+
  scale_color_manual(values = c("tan4","steelblue4"))+
  scale_fill_manual(values = c("tan","steelblue1"))+
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = c(.8,.3),
        legend.background = element_rect(colour = "black"))

p4 <- tetherdata %>% 
  filter(wetland == "M2"|wetland=="M4") %>% 
  ggplot(aes(x = length, y = survival, color = season, fill = season)) +
  theme_classic()+
  geom_smooth(method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), show.legend = F)+
  geom_smooth(method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), fill = NA)+
  coord_cartesian(ylim =  c(0.55, 1.00),
                  xlim = c(3,15))+
  scale_x_continuous(breaks = c(3,6,9,12,15))+
  annotate(geom = "text", x = 3.5, y = 1, label = "B)", size = 6)+
  scale_color_manual(values = c("tan4","steelblue4"))+
  scale_fill_manual(values = c("tan","steelblue1"))+
  labs(y = "Daily Survival Probability",
       x = "Shell Length (mm)")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = c(.8,.3),
        legend.background = element_rect(colour = "black"))


surv_layout <- "
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAA
####################
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
#BBBBBBBBBBBBBBBBBB#
####################
"

patch.survival <- (p1+p3)+plot_layout(design = surv_layout)

ggsave(here("Pomacea/Isocline_manuscript/out","fig3_tethering.png"),
       patch.survival, device = ragg::agg_png,
       units = "in", width = 4, height = 6)

ggsave(here("Pomacea/Isocline_manuscript/out/pdf","fig3_tethering.pdf"),
       patch.survival, device = "pdf",
       units = "in", width = 4, height = 6)

#--------------------------------------------------------------------------
####logistic regression results just LILA#####
#--------------------------------------------------------------------------

tether_lila <- tetherdata %>% 
  filter(wetland == "M2"|wetland=="M4")

logregfit1 <- glm(survival~length, data = tether_lila, family = "binomial")
logregfit2 <- glm(survival~length + wetland, data = tether_lila, family = "binomial")
logregfit3 <- glm(survival~length + season, data = tether_lila, family = "binomial")
logregfit4 <- glm(survival~length + wetland + season, data = tether_lila, family = "binomial")
logregfit5 <- glm(survival~season + wetland, data = tether_lila, family = "binomial")
logregfit6 <- glm(survival~wetland, data = tether_lila, family = "binomial")
logregfit7 <- glm(survival~length*wetland, data = tether_lila, family = "binomial")
logregfit8 <- glm(survival~length*wetland + season, data = tether_lila, family = "binomial")
logregfit9 <- glm(survival~season, data = tether_lila, family = "binomial")
logregfit10 <- glm(survival~length*season, data = tether_lila, family = "binomial")
logregfit11 <- glm(survival~length*season + wetland, data = tether_lila, family = "binomial")
logregfit12 <- glm(survival~season*wetland, data = tether_lila, family = "binomial")
logregfit13 <- glm(survival~season*wetland + length, data = tether_lila, family = "binomial")
logregfit14 <- glm(survival~transect, data = tether_lila, family = "binomial")
logregfit15 <- glm(survival~transect + wetland, data = tether_lila, family = "binomial")
logregfit16 <- glm(survival~transect + season, data = tether_lila, family = "binomial")
logregfit17 <- glm(survival~transect + length, data = tether_lila, family = "binomial")
logregfit18 <- glm(survival~transect*season, data = tether_lila, family = "binomial")
logregfit19 <- glm(survival~transect*length, data = tether_lila, family = "binomial")
logregfit20 <- glm(survival~transect*wetland, data = tether_lila, family = "binomial")
logregfit21 <- glm(survival~transect + wetland + season + length, data = tether_lila, family = "binomial")
logregfit22 <- glm(survival~transect + wetland + season, data = tether_lila, family = "binomial")
logregfit23 <- glm(survival~transect + wetland + length, data = tether_lila, family = "binomial")
logregfit24 <- glm(survival~transect +season + length, data = tether_lila, family = "binomial")
logregfit25 <- glm(survival~length*season + transect, data = tether_lila, family = "binomial")
logregfit26 <- glm(survival~length*season + transect + wetland, data = tether_lila, family = "binomial")

logregs <- list(logregfit1,logregfit2,logregfit3,logregfit4,logregfit5,logregfit6,logregfit7,logregfit8,
                logregfit9,logregfit10,logregfit11,logregfit12,logregfit13,logregfit14,logregfit15,
                logregfit16,logregfit17,logregfit18, logregfit19,logregfit20,logregfit21,logregfit22,
                logregfit23,logregfit24, logregfit25,logregfit26)

logregfit_names <- c("length",
                     "length + wetland",
                     "length + season",
                     "length + wetland + season",
                     "season + wetland",
                     "wetland",
                     "length*wetland",
                     "length*wetland + season",
                     "season",
                     "length*season",
                     "length*season + wetland",
                     "season*wetland",
                     "season*wetland + length",
                     "transect",
                     "transect + wetland",
                     "transect + season",
                     "transect + length",
                     "transect*season",
                     "transect*length",
                     "transect*wetland",
                     "transect + wetland + season + length",
                     "transect + wetland + season",
                     "transect + wetland + length",
                     "transect +season + length",
                     "length*season + transect",
                     "length*season + transect + wetland")

AIC(logregfit1)
log.AIC <- tibble(model.num = 1:26,
                  model = logregfit_names,
                  AICc = sapply(logregs, AIC)) 


log.AIC <- log.AIC %>% 
  mutate(AICc = round(AICc, digits = 3),
         deltaAIC = round(AICc-min(AICc),digits = 3),
         w = round(akaike.weights(log.AIC$AICc)$weights, digits = 3)) %>% 
  dplyr::select(-model.num)

         
write_csv(log.AIC, file = here("Pomacea/Isocline_manuscript/out","table1_AICc.csv"))

summary(logregfit10)
summary(logregfit11)
summary(logregfit25)
summary(logregfit26)  ###models 11-27 have nonsignificant contibuters of transects and wetlands

#-----------------------------------------------------------------------------
####logistic regression results lengths < 12mm lila - WCAs
#-----------------------------------------------------------------------------


tether.summ <- tetherdata %>%
  filter(length < 12) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m"),
         wetland = if_else(wetland == "M2" | wetland == "M4",
                           true = "LILA",
                           false = wetland)) %>% 
  drop_na(fate) %>% 
  group_by(season, wetland,fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(season,wetland) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))))

tether.summ %>% 
  filter(fate == "s") %>% 
  ggplot(aes(x = wetland, y = CJS, color = season))+
  geom_pointrange(aes(ymin = low, ymax = upp))
