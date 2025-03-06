#this code is for the analysis of the predator artifact data

#---------------------------------------------------
####libraries####
#---------------------------------------------------

library(patchwork)
library(jpeg)
library(png)

#---------------------------------------------------
#data management####
#---------------------------------------------------

#artifact data

mort_summary <- tetherdata %>% 
  filter(length < 10) %>% 
  filter(fate != "a") %>% 
  filter(fate != "d") %>% 
  filter(region == "LILA") |> 
  mutate(count = 1) %>% 
  group_by(season, fate) %>% 
  summarise(n = sum(count)) %>% 
  mutate(n.tot =sum(n),
         perc.contr = n/n.tot*100,
         prop.contr = n/n.tot,
         upp = (prop.contr + (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = (prop.contr - (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = if_else(low > 0, true = low, false = 0.00))


#predator data

invpredatorsumm <- predatordata %>% 
  mutate(year = year (`D.O.C`),
         juvcray = if_else(SpeciesCode == "Profal" & StandLen_mm < 14, true = 0,
                           false = 1),
         juvbel = if_else(SpeciesCode == "Belspp" & StandLen_mm < 10, true = 0,
                          false = 1)) %>% 
  filter(juvcray != 0) %>% 
  filter(juvbel != 0) |> 
  drop_na(`D.O.C`) %>% 
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


predatorsumm <- invpredatorsumm |> 
  bind_rows(vertpredatorsumm) |>
  filter(SpeciesCode %in% c("Belspp","Profal","Sirlac"))

#-----------------------------------------------------
####plot the data####
#-----------------------------------------------------

#artifact images

img_crush <- readJPEG(here("Pomacea/Isocline_manuscript/pics","crushed_cutout.jpg"), native = T)
img_empty <- readJPEG(here("Pomacea/Isocline_manuscript/pics","emptyshell.jpg"), native = T)
img_miss <- readJPEG(here("Pomacea/Isocline_manuscript/pics","missing.jpg"),native = T)

#artifact data

p5 <- mort_summary %>% 
  ggplot(aes(y = n, x = season))+
  geom_bar(aes(fill = fate), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  
  scale_fill_manual(values = c("steelblue4","mediumpurple4", "tan4"),
                    labels = c("crushed (Crayfish)", "empty (Giant Water Bug)", "missing (Greater Siren)"))+
  theme(legend.title = element_blank(),
        legend.position = c(.75,.9))+
  labs(y = "Artifact Count",
       x = "Season",
       title = " ")+
  annotate(geom = "text", x = .5, y = 22, label = "A)", size = 6)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"))

#put the images on the plot

patch.artifact <- p5 + 
  inset_element(p = img_crush,
                left = 0.06,
                bottom = 0.28,
                right = 0.20,
                top = 0.48) +
  inset_element(p = img_crush,
                left = 0.52,
                bottom = 0.20,
                right = 0.65,
                top = 0.40)+
  inset_element(p = img_empty,
                left = 0.21,
                bottom = 0.98,
                right = 0.33,
                top = 1.16)+
  inset_element(p = img_empty,
                left = 0.66,
                bottom = 0.20,
                right = 0.78,
                top = 0.38)+
  inset_element(p = img_miss,
                left = 0.81,
                bottom = 0.30,
                right = .92,
                top = 0.48)+
  inset_element(p = img_miss,
                left = 0.36,
                bottom = 0.95,
                right = 0.47,
                top = 1.13)

patch.artifact

#invertebrate predator plot

 p6 <- predatorsumm %>% 
  mutate(SpeciesCode = case_when(SpeciesCode == "Belspp" ~ "Giant Water Bug",
                                 SpeciesCode == "Profal" ~ "Crayfish",
                                 SpeciesCode == "Sirlac" ~ "Greater Siren")) |> 
  ggplot(aes(y = count, x = Season))+
  geom_bar(aes(fill = SpeciesCode), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  scale_fill_manual(values = c("steelblue4","orchid4","tan4"),
                    labels = c("Crayfish","Giant Water Bug","Greater Siren"))+
  theme(legend.title = element_blank(),
        legend.position = c(.75,.8))+
  labs(y = "Predator Abundance (n)",
       x = "Season",
       title = " ")+
   annotate(geom = "text", x = .5, y = 24, label = "B)", size = 6)+
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 12, face = "bold"))

#invert images
 
img_cray <- readJPEG(here("Pomacea/Isocline_manuscript/pics","crayfishcut.jpg"),native = T)
img_belo <- readJPEG(here("Pomacea/Isocline_manuscript/pics","belostomacut.jpg"),native = T)
img_siren <- readJPEG(here("Pomacea/Isocline_manuscript/pics","sirencut.jpg"),native = T)

#put images on plot

patchinvert <- p6 +
   inset_element(p = img_cray,
                 left = 0.50,
                 bottom = 0.25,
                right = 0.63,
                top = 0.37)+
  inset_element(p = img_cray,
                left = 0.07,
                bottom = .97,
                right = 0.20,
                top = 1.09)+
  inset_element(p = img_belo,
                left = 0.21,
                bottom = .36,
                right = 0.34,
                top = .51)+
  inset_element(p = img_belo,
                left = 0.66,
                bottom = 0.18,
                right = 0.79,
                top = 0.33)+
  inset_element(p = img_siren,
                left = 0.81,
                bottom = .09,
                right = 0.91,
                top = .21)+
  inset_element(p = img_siren,
                left = 0.36,
                bottom = .81,
                right = 0.46,
                top = .93)

patchinvert
#plot vertebrate predator counts

#p7 <- vertpredatorsumm %>% 
 # ggplot(aes(y = count, x = Season))+
 # geom_bar(aes(fill = SpeciesCode), stat = "identity", color = "black",
#           position = position_dodge())+
#  theme_classic()+
#  scale_fill_manual(values = c("tan4","tan2","tan"),
#                    labels = c("Mayan Cichlid", "Redear Sunfish","Greater Siren"))+

#  theme(legend.title = element_blank(),
#        legend.position = c(.87,.8),
#        legend.text = element_text(size = 7))+
 # labs(y = "Predator Abundance (n)",
#       title = " ")+
 # annotate(geom = "text", x = .6, y = 100, label = "C)", size = 6)+
#theme(axis.text = element_text(size = 12),
#       axis.title = element_text(size = 12, face = "bold"),
#        legend.text = element_text(size = 9))



#vertebrate predator pictures

#img_mayan <- readPNG(here("Pomacea/Isocline_manuscript/pics","Mayan-Cichlid_transparent.png"),native = T)
#img_redear <- readPNG(here("Pomacea/Isocline_manuscript/pics","Redear-Knockout-DR.png"),native = T)

#put pictures on plot

#patchvert <- p7+
#  inset_element(p = img_mayan,
#                left = 0.52,
#                bottom = .87,
#                right = 0.67,
#                top = .99)+
#  inset_element(p = img_mayan,
#                left = 0.06,
#                bottom = .75,
#                right = 0.21,
#                top = .87)+
#  inset_element(p = img_redear,
#                left = 0.22,
#                bottom = .07,
#                right = 0.32,
#                top = .19)+
#  inset_element(p = img_redear,
#                left = 0.68,
#                bottom = .12,
#                right = 0.78,
#                top = .24)+
#  inset_element(p = img_siren,
#                left = 0.81,
 #               bottom = .09,
#                right = 0.91,
#                top = .21)+
#  inset_element(p = img_siren,
#                left = 0.36,
#                bottom = .28,
#                right = 0.46,
#                top = .40)

#
p_percap <- tibble(Season = c("dry", "dry", "dry", "wet", "wet", "wet"),
                   percap = c(5/24,22/8,21/20,3/5,3/3,6/1),
                   spp = c("Crayfish", "Giant Water Bug", "Greater Siren", "Crayfish", "Giant Water Bug", "Greater Siren"))%>% 
  ggplot(aes(y = percap, x = Season))+
  geom_bar(aes(fill = spp), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  scale_fill_manual(values = c("steelblue4","orchid4", "tan4"),
                    labels = c("Crayfish", "Giant Water Bug", "Greater Siren"))+
  theme(legend.title = element_blank(),
        legend.position = c(.25,.85),
        legend.text = element_text(size = 7))+
  labs(y = "predation rate (artifact/abundance)",
       title = " ")+
  annotate(geom = "text", x = .5, y = 6, label = "C)", size = 6)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 9))


#put pictures on plot

patchpercap <- p_percap+
  inset_element(p = img_cray,
                left = 0.5,
                bottom = 0.15,
                right = 0.64,
                top = 0.27)+
  inset_element(p = img_cray,
                left = 0.04,
                bottom = .08,
                right = 0.18,
                top = 0.20)+
  inset_element(p = img_belo,
                left = 0.21,
                bottom = .48,
                right = 0.35,
                top = .63)+
  inset_element(p = img_belo,
                left = 0.66,
                bottom = 0.22,
                right = 0.79,
                top = 0.37)+
  inset_element(p = img_siren,
                left = 0.81,
                bottom = 0.96,
                right = 0.91,
                top = 1.08)+
  inset_element(p = img_siren,
                left = 0.36,
                bottom = .22,
                right = 0.46,
                top = .34)

patchpercap

#create pannel of the three plots with the artifact pannel on top

patch.predator <- patch.artifact / patchinvert /patchpercap

#view the plot
patch.predator


#save to the out folder
ggsave(here("Pomacea/Isocline_manuscript/out","fig4_artifact.png"),
       patch.predator, device = ragg::agg_png,
       units = "in", width = 8, height = 9)

ggsave(here("Pomacea/Isocline_manuscript/out/pdf","fig4_artifact.pdf"),
       patch.predator, device = "pdf",
       units = "in", width = 8, height = 9)
