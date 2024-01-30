#this script is to load in data for all the analyses performed in the isocline manuscript
#this code should be run prior to all the other code

rm(list = ls())

#-------------------------------------------
####Libraries#####
#-------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(lubridate)

#--------------------------------------------
#####Tethering Data#####
#--------------------------------------------

#####load in data####
tetherdata.raw <- read_excel(here("Pomacea/Isocline_manuscript/data","tetheringdata.xlsx"), sheet = 4)

lengthcodes <- read_excel(here("Pomacea/Isocline_manuscript/data","tetheringdata.xlsx"), sheet = 2)      

tetherdata <- tetherdata.raw %>% 
  left_join(lengthcodes, by = c("season", "box", "bin", "box.id","box.day")) %>% 
  dplyr::select(-entered.by.x, -entered.by.y, -checked.by.x, -checked.by.y) %>% 
  mutate(survival = if_else(fate=="a", true = 1, false = 0))             #this adds a variable called survival with 1 = survival, and 0 = dead/eaten

rm(list = c("lengthcodes","tetherdata.raw"))

#--------------------------------------------
#####predator data####
#--------------------------------------------

predatordata <- read_excel(here("Pomacea/Isocline_manuscript/data","PredatorDiets_v1.5.xls"), sheet = 3,
                           na = "UNK") %>% 
  mutate(juvcray = if_else(SpeciesCode == "Profal" & StandLen_mm < 14, true = 0,
                           false = 1),
         juvbel = if_else(SpeciesCode == "Belspp" & StandLen_mm < 10, true = 0,
                          false = 1))

#---------------------------------------------
####predator diets####
#---------------------------------------------

#####load data####

dietdata.raw <- read_excel(here("Pomacea/Isocline_manuscript/data","PredatorDiets_v1.5.xls"), sheet = 2)

###clean up the data####

#lines 18-22 create a new variable with lowest taxonomy of family

dietdata.clean <- dietdata.raw %>%  #create a new tibble called clean using the raw data
  unite(col = "pred.genus_species", c(Pred.Genus,Pred.Species), sep = " ") %>%   #combine predator genus and species names
  mutate(low.taxonomy = if_else(!is.na(Prey.Family), true = Prey.Family,
                                false = if_else(!is.na(Prey.Order), true = Prey.Order,
                                                false = if_else(!is.na(Prey.Class), true = Prey.Class,
                                                                false = if_else(!is.na(Prey.Phylum), true = Prey.Phylum,
                                                                                false = Prey.Kingdom)))))

rm(list = "dietdata.raw")

##--------------------------------------------
####growth data####
##--------------------------------------------

####load data####

#growthdata
growthdata <- read_excel(here("Pomacea/Isocline_manuscript/data","Growth_DataSheet_v5.xlsx"), sheet = 2)
#treatment codes
treatmentcode <- read_excel(here("Pomacea/Isocline_manuscript/data","Growth_DataSheet_v5.xlsx"), sheet = 4)
#maculata length
maclen <- read_excel(here("Pomacea/Isocline_manuscript/data","Growth_DataSheet_v5.xlsx"), sheet = 5) %>% 
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

table(growthdata$Size_Class)
table(growthdata$Code)
summary(growthdata$SGR)

rm(list = c("maclen","treatmentcode"))

###cage experiment total phosphorus

TP_data <- read_csv(here("Pomacea/Isocline_manuscript/data","table_cagecharact.csv"))

#---------------------------------------------
####predator free survival####
#---------------------------------------------

predatorfree <- read_excel(here("Pomacea/Isocline_manuscript/data","predatorfree_survival.xlsx"), sheet = 2)

#--------------------------------------------
####Water and Air Temps####
#--------------------------------------------

#read in water temperatures add cell names as grouping variables

WATERTEMP_M2 <- read_csv(here("Pomacea/Isocline_manuscript/data", "HOBO_20423783_M2_12-17-2020--8-16-2021.csv"), skip = 1) %>%  #read in the M2 data
  mutate(type = "water_M2")                                                                  #name as M2 in new variable called "type"

WATERTEMP_M3 <- read_csv(here("Pomacea/Isocline_manuscript/data", "HOBO_20423785_M3_12-17-2020--8-16-2021.csv"), skip = 1) %>%  #read in the M3 data
  mutate(type = "water_M3")                                                                  #name as M3 in new variable called "type"

AIRTEMP <- read_csv(here("Pomacea/Isocline_manuscript/data","DBHYDRO_airtemp_12-18-2020--8-11-2021.csv"), skip = 3) %>%  #read in the air temp data
  rename(date.time =`Daily Date`,                                 #change daily date to date.time                 
         temp.c = `Data Value`) %>%                               #change data value to air.temp.c
  dplyr::select(date.time,temp.c) %>%                                #only use these variables
  mutate(date.time = as_datetime(dmy(date.time)),
         type = "air_WestPalm")                                   #format the date and time to r's structure

#combine the data, fix variable names, set date structure, remove setting day and ending day

TEMP <- WATERTEMP_M2 %>%                                          #save WATERTEMP_M2 as a different tibble 
  bind_rows(WATERTEMP_M3) %>%                                     #combine WATERTEMP_M3 using the column names
  rename(obs = "#",                                               #change "#" into obs
         date.time = `Date Time, GMT-05:00`,                      #change the large variable name to date.time
         temp.c = `Avg: Temperature`) %>%                         #change Avg: Temp to air.temp.c
  mutate(date.time = as_datetime(mdy_hm(date.time))) %>%          #format date.time to R's date time structure
  filter(date.time %within% interval(start = ymd("2020-12-18"),   #filter the data within the time intervals specified
                                     end = ymd("2021-8-11"))) %>% # 
  bind_rows(AIRTEMP) %>% 
  dplyr::select(date.time, type, temp.c) %>% 
  mutate(season = if_else(date.time %within% interval(start = ymd("2020-12-18"),
                                                      end = ("2021-5-31")),
                          true = "dry", false = "wet"))

#check the data

table(is.na(TEMP$date.time))          # NAs

TEMP <- TEMP %>%                      #remove the one NA
  drop_na(date.time)

table(is.na(TEMP$date.time))          #no NAs
table(is.na(TEMP$temp.c))             #no NAs
table(is.na(TEMP$season))             #no NAs
table(TEMP$type)                      #looks good each type as the same number of observations
table(TEMP$season)                    #as expected there are more observatin in dry than wet

rm(list = c("AIRTEMP","WATERTEMP_M2","WATERTEMP_M3"))

#----------------------------------------
######WCA Periphyton TP####
#----------------------------------------

WCA_TP <- read_excel(here("Pomacea/Isocline_manuscript/data","20221014_TPData_DornLab.xlsx")) %>% 
  mutate(Site = c(rep("WCA2", times = 2),
                  rep("WCA3", times = 2)))

#data to create regression to predict growth rates

growthexpdata <- read_excel(here("Pomacea/Isocline_manuscript/data","FieldGrowthExperiments-2019-2020.xlsx"))

nutrientdata <- read_excel(here("Pomacea/Isocline_manuscript/data","RawTP_2019&2020.xlsx"), sheet = 5)

growth.summ <- growthexpdata %>% 
  group_by(year,cell,treatment,cage,species) %>% 
  summarise(fl_mean = mean(length_sl, na.rm = T),
            fm_mean = mean(mass, na.rm = T)) %>% 
  mutate(cage = as.character(cage))

####obtain initial masses 

#create regression to get dry weights
#P.paludosa length weight regression
ppal.lengthweight <- function(SL) {
  ln.tot.drymass <- 2.64*log(SL) - 1.84
}

#P.maculata length weight regression

pmac.lengthweight <- function(SL) {
  ln.tot.drymass <- 2.70*log(SL) - 2.21
}

####get initial lengths
initialdata <- tibble(year = c(2019,2019,2020,2020),
                      species = c("P.paludosa","P.maculata","P.paludosa","P.maculata"),
                      il_mean = c(3.56,3.61,4.40,4.60)) %>% 
  mutate(im_mean = pmac.lengthweight(SL = il_mean))

growth.summ <- growth.summ %>% 
  left_join(initialdata, by = c("year", "species"))

####Nutrient Data####

nutrientdata <- nutrientdata %>% 
  select(Year, Cell, Cage, 'TP_ug/g') %>% 
  rename(cell = Cell,
         cage = Cage,
         TP = 'TP_ug/g',
         year = Year)

head(nutrientdata)

growth.summ <- growth.summ %>% 
  left_join(nutrientdata, by = c("year", "cell", "cage")) 

growth.summ <-growth.summ%>% 
  mutate(SGR_mass = (log(fm_mean)-log(im_mean))/35) %>% 
  mutate(Growth_mass = (fm_mean - im_mean)/35,
         SGR_length = (log(fl_mean)-log(il_mean))/35,
         Growth_length = (fl_mean - il_mean)/35) 

TP_mean <- mean(growth.summ$TP, na.rm = T)
TP_sd <- sd(growth.summ$TP, na.rm = T)

growth.summ <- growth.summ %>% 
  mutate(z_TP = (TP-TP_mean)/TP_sd)

growth.summ <- growth.summ %>% 
  drop_na(TP) %>% 
  filter(TP < 1000)


###----------------------
####predator free survival####
#--------------------------

enclosuredata <- read_excel(here("Pomacea/Isocline_manuscript/data","predatorfree_survival.xlsx"), sheet = 2)


####--------------------------
###WCA Depths environmental averages###
#----------------------------

###please note the measured depths are taken from our WCA3A tethering experiments sites
###we used the site coordinates to get depths from EDEN and we corrected them using the 
###measured depths

WCA_correction_temp <- tetherdata %>% 
  filter(region == "WCA3A") %>% 
  select(wetland,depth,date.placed) %>% 
  drop_na(depth) %>% 
  group_by(wetland) %>% 
  summarise(depth = mean(depth, na.rm = T)) %>% 
  mutate(date = mdy("7/18/2022")) %>% 
  rename(measured_depth = depth)

dat2010_2020 <- read_table(here("Pomacea/Isocline_manuscript/data","waterdepths_eden_isocline_2010-2020.txt"),
                           skip = 5, col_names = c("date","WCA02","WCA03")) %>% 
  gather(WCA02,WCA03, key = "wetland", value = "eden_depth")

dat2021_2022 <-  read_table(here("Pomacea/Isocline_manuscript/data","waterdepths_eden_isocline_2021-2022.txt"),
                            skip = 5, col_names = c("date","WCA02","WCA03"))  %>% 
  gather(WCA02,WCA03, key = "wetland", value = "eden_depth") 

WCA_correction <- dat2021_2022 %>% 
  left_join(WCA_correction_temp,by = c("wetland","date")) %>% 
  drop_na(measured_depth) %>% 
  mutate(correction = eden_depth - measured_depth) %>% 
  select(wetland,correction)

WCA_depth_raw <- dat2010_2020 %>% 
  bind_rows(dat2021_2022) %>% 
  left_join(WCA_correction, by = "wetland") %>% 
  mutate(depth_corrected = eden_depth-correction,
         year = year(date),
         month = month(date),
         day = day(date))

WCA_depth_summ <- WCA_depth_raw %>%
  group_by(wetland,month,day) %>% 
  summarise(depth_ave = mean(depth_corrected)) %>% 
  ungroup() %>% 
  mutate(year = 2020) %>% 
  unite(month,day,year, col = date, sep = "/") %>% 
  mutate(date = mdy(date)) %>% 
  filter(date != mdy("2/29/2020"))
  
rm(list = c("dat2010_2020","dat2021_2022","WCA_correction_temp"))

###please note that these temps are taken from the meterologic station in WCA3A
###the name is 3AS3WX lat (255106.215) long (804558.543) dbkey (LA373)

WCA_temp_raw <- read_csv(here("Pomacea/Isocline_manuscript/data","WCA3A_temp.csv"),
                           skip = 3) %>% 
  rename(date = `Daily Date`,
         temp = `Data Value`) %>% 
  mutate(date = dmy(date),
         year = year(date),
         month = month(date),
         day = day(date))

WCA_temp_summ <- WCA_temp_raw %>%
  group_by(month,day) %>% 
  summarise(temp_ave = mean(temp)) %>% 
  ungroup() %>% 
  mutate(year = 2020) %>% 
  unite(month,day,year, col = date, sep = "/") %>% 
  mutate(date = mdy(date)) %>% 
  filter(date != mdy("2/29/2020")) %>% 
  select(date,temp_ave)


#####combine the two 

WCA_environment <- WCA_depth_summ %>% 
  left_join(WCA_temp_summ, by = "date")

