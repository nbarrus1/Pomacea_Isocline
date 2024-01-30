#There are two purposes to this code: (1) to compare water temperatures
#between seasons and (2) explore the relationship between air temperature
#and water temeprature

#key to understanding some of the details in my code:
#matrices,dataframes,tibbles -> all caps and spaces are underscored: e.g. DATA_2
#vectors,variables,constants -> lowercase and spaces are periods: e.g. variable.2
#statistical models -> termed fit and type sepearted by underscores: e.g. fit_lm
#                   -> note, on occasion I omit the model type for simple linear models
#                   -> e.g., fit = fit_lm

rm(list = ls())  #remove global environment

#-------------------------------------------------------
######## Load in Packages ########
#-------------------------------------------------------

library(tidyverse)  #contains  dplyr ggplot2 and a host of other packages
library(lubridate)  #used for date and time data
library(agricolae)
library(rsample)

#-------------------------------------------------------
###### Load in and clean data#########
#-------------------------------------------------------

#read in water temperatures add cell names as grouping variables

WATERTEMP_M2 <- read_csv(file = "HOBO_20423783_M2_12-17-2020--8-16-2021.csv", skip = 1) %>%  #read in the M2 data
  mutate(type = "water_M2")                                                                  #name as M2 in new variable called "type"

WATERTEMP_M3 <- read_csv(file = "HOBO_20423785_M3_12-17-2020--8-16-2021.csv", skip = 1) %>%  #read in the M3 data
  mutate(type = "water_M3")                                                                  #name as M3 in new variable called "type"

AIRTEMP <- read_csv(file = "DBHYDRO_airtemp_12-18-2020--8-11-2021.csv", skip = 3) %>%  #read in the air temp data
  rename(date.time =`Daily Date`,                                 #change daily date to date.time                 
         temp.c = `Data Value`) %>%                               #change data value to air.temp.c
  select(date.time,temp.c) %>%                                #only use these variables
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
  select(date.time, type, temp.c) %>% 
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

#---------------------------------------------------------------------
#####Data Exploration and Analysis#####
#---------------------------------------------------------------------


#histogram air temps

TEMP %>%                                #specify the data 
  ggplot(aes(x = temp.c))+              #initiate a plot with temp.c as the x variable
  geom_histogram()+                     #perform a histogram
  facet_wrap(~type)+                    #create two plots one for each cell
  theme_classic()                       #use this theme so background is white

#time series of air temp

TEMP %>%                                            #specify data                                            
  ggplot(aes(x = date.time, y =temp.c,              #x variable is date, y variable water temp, by cell
             color = type))+                        #groups the cells using different colors
  geom_line(size = 1)+                              #initiate line plot
  geom_point()+                                     #add points
  theme_classic()                                   #use the white background theme

#boxplot by type then by season and both

TEMP %>%                                         #specify the data 
  ggplot(aes(x = type, y = temp.c))+             #x is cell, y is water temp
  geom_boxplot()+                                #initiate boxplot
  geom_jitter()+                                 #overlay points 
  theme_classic()                                #use the white background theme

TEMP %>%                                         #specify the data 
  ggplot(aes(x = season, y = temp.c))+             #x is cell, y is water temp
  geom_boxplot()+                                #initiate boxplot
  geom_jitter()+                                 #overlay points 
  theme_classic()                                #use the white background theme

TEMP %>%                                         #specify the data 
  ggplot(aes(x = type, y = log(temp.c)))+             #x is cell, y is water temp
  geom_boxplot()+                                #initiate boxplot
  geom_jitter()+                                 #overlay points 
  theme_classic()+                               #use the white background theme
  facet_wrap(~season)

###bootstrap to get confidence intervals####

BOOT <- TEMP %>%
  crossing(rep = seq(10000)) %>% 
  group_by(rep,type,season) %>% 
  sample_n(n(),temp.c, replace = TRUE) %>% 
  summarise(ave.temp.c = mean(temp.c, na.rm = TRUE))

BOOT_results <- BOOT %>% 
  group_by(type,season) %>% 
  summarise(med = median(ave.temp.c),
            low = quantile(ave.temp.c, probs = 0.025),
            upp = quantile(ave.temp.c, probs = 0.975))

#air temp vs water temp
TEMP_SPLIT <- TEMP %>%                                                     #specify the data
  pivot_wider(names_from = type, values_from = temp.c)

TEMP_SPLIT %>%                                               #specify the data
  ggplot(aes(x = air_WestPalm, y = log(water_M2), color = season))+          #initiate plot 
  geom_point()+                                              #create scatter plot
  theme_classic()+                                           #change to white background theme
  geom_smooth(method = "lm")


fit_lm_M2 <- lm(log(water_M2) ~ air_WestPalm+ season , data = TEMP_SPLIT )     #fit a lm
summary(fit_lm_M2)                                                    #extract R2 etc
plot(fit_lm_M2)                                                       #run diagnostic plots

TEMP_SPLIT %>%                                               #sepcify the data
  ggplot(aes(x = air_WestPalm, y = log(water_M3)))+          #initiate plot
  geom_point()+                                              #create scatter plot
  theme_classic()+                                           #change to white background theme
  geom_smooth(method = "lm")                                 #add a line of fit

fit_lm_M3 <- lm(log(water_M3) ~ air_WestPalm, data = TEMP_SPLIT )      #fit a lm
summary(fit_lm_M3)                                                     #extract R2 etc
plot(fit_lm_M3)                                                        #run diagnostic plots



