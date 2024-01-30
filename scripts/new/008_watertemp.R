#There are two purposes to this code: (1) to compare water temperatures
#between seasons and (2) explore the relationship between air temperature
#and water temeprature

#key to understanding some of the details in my code:
#matrices,dataframes,tibbles -> all caps and spaces are underscored: e.g. DATA_2
#vectors,variables,constants -> lowercase and spaces are periods: e.g. variable.2
#statistical models -> termed fit and type sepearted by underscores: e.g. fit_lm
#                   -> note, on occasion I omit the model type for simple linear models
#                   -> e.g., fit = fit_lm

#------------------------------
#####libraries####
#------------------------------

library(rsample)
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
  geom_jitter()+ 
  geom_boxplot()+                                #initiate boxplot                                #overlay points 
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


fit_lm_M2 <- lm(log(water_M2) ~ air_WestPalm + season , data = TEMP_SPLIT )     #fit a lm
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

