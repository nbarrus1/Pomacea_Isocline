#this code is to conduct the simulations for development of the isocline
#as of right now I've not optimized this code for run speed and will take well over
#3 hours to run.

#-----------------------------------------------------------------------------------#
###### load libraries ########
#-----------------------------------------------------------------------------------#

library(demogR) #for building transition matrix


#-----------------------------------------------------------------------------------#
##### Make Functions for growth, survival, and fertility assumtions #####
#-----------------------------------------------------------------------------------#

####Growth Function####

#growth is assumed to be detrministic and follow the bottom logistic equation.
#All parameters are assumed to stay the same throughout the year.
#Model initial parameters,include:
#int.size = intitial size measured in mm; this value is 3mm (see Table A1-1)
#fin.size = final size measured in mm; this value is 50mm (see Table A1-1)
#growth.rate = growth rate; this value is 0.05 (see Table A1-1)
#essentially this function takes a vector of ages and gives us the sizes of those age classes

growth <- function(age, int.size = 3, fin.size = 50, growth.rate = 0.05){
  size.mm = (int.size*exp(growth.rate*age))/(1+((int.size/fin.size)*(exp(growth.rate*age)-1)))
}

#test to see if this function works how we want it to (see figure A1-1 in supplementary material)
plot(x = 1:600, y = growth(age = 1:600), xlab = "age (days)", ylab = "size (mm)")
#yep looks good!

#####Survival Function ####

#survival was measured at differenct size classes in wet and dry conditions
#depths are measured in cm, size measured in mm, and age measured in days

#we're taking a single depth value, a vector of sizes and a vector of ages to get survival probabilities
#now if our depths are greater than 0 cms we should use wet condition values, if its lower than 0 cm  
#we need to use dry conditions

#However there are no adult measures of survival so they assume that they stay the same as their largest
#age class. Yet, snails only have roughly a 500 day life span (a year and half) so our additional equation,
#gives us a rapid die off of snails around 500 days.

#size specific survival values are given in Table A1-1.
#the age of mortality is 500 days (table A1-1) is given in the equation for rapid die off
#the k(age) value is 0.1 (table A1-1) is in the equation for rapid die off

survival <- function(depth, size, age, Surv1 = 0.987, Surv2 = 0.987) {
  if(depth > 0) {
    surv = if_else(size <= 6, 
                   true = Surv1,
                   false = if_else(size > 6 & size <= 10,
                                   true = Surv2,
                                   false = if_else(size <= 16, true = 0.987, false = 0.99/(1+exp(-0.1*(500 - age))))))
  }
  else { surv = if_else(size <= 6, 
                        true = 0.976,
                        false = if_else(size > 6 & size <= 10,
                                        true = 0.984,
                                        false = if_else(size > 10 & size<= 16,
                                                        true = 0.989,
                                                        false = 0.99/(1+exp(-0.1*(500 - age))))))
  }}

#lets test to see how our function works

plot(x = 1:600, y = survival(depth = 1, size = growth(age = 1:600), age = 1:600), xlab = "age (days)",
     ylab = "probability of survival")

#rapid die off is working, now lets see how the size classes are working (see Figure A1-2)

plot(x = 1:600, y = survival(depth = 1, size = growth(age = 1:600), age = 1:600), xlab = "age (days)",
     ylab = "probability of survival", xlim = c(0,60), ylim = c(0.972,1), type = "l")
lines(x = 1:600, y = survival(depth = 0, size = growth(age = 1:600), age = 1:600), col = "red")
#yep everything is working like it should!

####fertility functions####

#now EverSnail multiplies 5 functions and 2 value together to get the overall fertility 
#rates of each age class

#value 1) sex ratio
#apple snails are dioecious thus there is a 1 to 1 sex ratio, we need all females in populaiton (See table A1-1)

frac.females <- 0.5

#value 2) number of eggs in an egg mass they assume it 30 which is roughly the average (See table a1-1)
#however this number needs to be standardized to egg/day over a females reproductive history. 
#21 clutches being the number a female snail laid in her lifetime in the lab.

egg.number <- (30*21)/501

#function 1) we need a function to the fraction of snails that are sexually mature. 
#this is a function of size.  The size at maturity is typically at 27.5 mm  (table A1-1) 
#so this function gives varibility round that size.

sex.mature.ratio <- function (size) {
  f.sex.mat <- (exp(1*(size - 27.5)))/(1+exp(1*(size - 27.5)))
}

#lets test if this is working. See Figure (A1-3)

size.mm = seq(from = 3, to = 50, by = 0.1)
plot(x = size.mm, y = sex.mature.ratio(size = size.mm), xlab = "size", ylab = "cumulative fraction of females sexually mature")

#yep looks good!

#function 2) create a function of the effect of depth on fertility.
#fecundity is influenced by depth as desctibed below. So the funtion input is going to be depth and it is 
#going to give us a value between 1 and 0 that describes the probability of laying eggs at a given depth
#depth is measured in cm
#for the equateion the minimumn depth was 10 cm and the max depth was 90 cm (See Table A1-1), the middle depth
#was 50 cm and W(h) was 40 cm See Table (A1-1) these values were added into the equation.

rep.depth <- function (depth) {
  rep <- if_else(depth >= 10 & depth <= 90,
                 true = 1.0 - ((depth - 50)^2 / 40^2),
                 false = 0.0)
}

#lets see if this funciton is working like it should (See Figure A1-5)
plot(x = 1:120, y = rep.depth(depth = 1:120), xlab = "depth", ylab = "probability of reproduction")
#yep its working good!

#function 3) create a function of the effect of temperature on fertility.
#fertility is influenced by temeprature as desctibed below. So the funtion input is going to be temperatrue
#and it is going to give us a value between 1 and 0 that describes the probability of laying eggs at a 
#given temperature. Temperature is measured in degrees celsius.  The k (temperature) for the equation was 1,
#and the trheshold for temperature was 17 degrees celsius (see Table A1-1).

rep.temp <- function (temp) {
  rep <- 1/(1 + (exp(-1*(temp - 17)))) 
}

#lets test if our function is working 
temp.c <- seq(from = 1, to = 30, by = 0.1)
plot(x = temp.c, y = rep.temp(temp = temp.c))

#function 4) create a function for seasonality aspect of apple snail reproduction.
#in EverSnail they used days which specified the season. Here I'm doing it similarly but with months.
#The function takes as an input a vector of months (as characters so spelling matters) then assigns a value
#of 1 in february march april and may (the peak reproduction months) and 0.3 for june july and august
#(where some reproduction happens but not a lot). and 0 for any other month where there is no reproduction.
# I will show how this is working when I imput the Date data from our environmental data
rep.season <- function(month){
  if_else(month == 2| month == 3| month == 4 |month == 5,
          true = 1,
          false = if_else(month == 6| month==7| month == 8,
                          true = 0.3,
                          false = 0))
}

#--------------------------------------------------------------------------------------#
#####Build our Matrices using 1 depth measurement and 1 temp measurement#####
#--------------------------------------------------------------------------------------#

####transition matrix####

#depth measurement = 15cm
#temperature measuremnt = 15 degrees celsius

#using our survival funtion create a vector of survivals for all age classes 1 through 500
Sx <- survival(depth = 15, size = growth(age = 1:500), age = 1:500)

#using our functions and values for fecundity we multiply all of the fuctions and values together, 
#and round the values we get to 3 decimal places
Fx <- c(0,round(x = (frac.females *
                       egg.number * 
                       rep.depth(depth = 15) *
                       rep.temp (temp = 15)*
                       sex.mature.ratio(growth(age = 1:500))),
                digits = 3))

#from the dmorgR package it gives us this function odiag
#function notation odiag(A, at = 0), where A is a vector of numbers. At = 0 is position in a matrix
#this function takes our vector of probabilities creates a square matrix by the length of our vector,
#and puts the values on the diagonal at a position

L <- odiag(Sx, at = -1)  #we use -1 to put start the values one row below (see A 2) 

#now we need to put vector of fecundities in the first row, the result is a 501 by 501 matrix
L[1,] <- Fx

#lets check to see if it is working how we want it
L[1:12, 1:16]           #the survival are going down at a diagonal correctly
L[1,]                   #the fecundities were added to the first row correctly

#####state matrix####

#lets add an initial amount of 10000 age 1 snails to see if the model is working
N0 <- matrix(data = c(10000,rep.int(0,times = 500)))

#multiply our tranition matrix by our population broken into age classes
N1 = L %*% N0                        #day 1 we need to use matrix multiplication which is why %*% is used
sum(N1)                              #population size after day one is working because all snails age 0
#have no reproduction

N2 = L %*% N1                        #day 2 with same depth measurements
sum(N2)                              #populaiton size after day 2 again no reproduction matrix is working

#---------------------------------------------------------------------------------------#
#####Make transition matrix that constantly updates each day based on environmental variables #####
#---------------------------------------------------------------------------------------#

#####load in environmental data #####

environment_data <- read_xlsx(here("Pomacea/Isocline_manuscript/data","EnvrionmentalData_M2&m1_1-1-20--12-31-20.xlsx"), sheet = 2)   
environment_data
#IT reads the dates into the file as dates as well!! 
#convert our depths to cm, let also create a variable called mon for the month that each day 
#is in this will be used for our seaonal aspect on fertility
environment_data <- environment_data %>% 
  mutate(Depth_M1_cm = (Depth_M1_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         Depth_M2_cm = (Depth_M2_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         mon = month(environment_data$Date))            #our vector of months

#take a quick look at the data
#temperature
plot(Temp_nat~Date, data = environment_data, type = "l")
#depth
plot(Depth_M2_cm~Date, data = environment_data, type = "l")   #M2 Depths

#chech fertility seasonality see figure A1-4
plot(x = environment_data$mon, y = rep.season(month = environment_data$mon), type = "l")

#####Create model that constantly updates with environmental variables####

#create data frame for model results
length(environment_data$Depth_M2_cm)

results <- tibble(pop_size_M2 = rep.int(0,times = length(environment_data$Depth_M2_cm))) %>% 
  mutate(no.mature_M2 = rep.int(0,times = length(environment_data$Depth_M2_cm)))

results <- as.matrix(results)
#give our starting number of snails and our population vector
N_M2 <- matrix(data = c(100,rep.int(0,times = 500)))

#Below here there are two loops, one fro M2 and another for M4, that take our daily measures of 
#depth and temperature and creates a Leslie matrix for that given day. Then its going to multiply
#our population vector above by that days Lesilie matrix for the transition between that day and 
#the next day. Its going to do this for all our depth and temperature in order from our input data.
#Then we are going to save the population size and number mature snails after the transition
#into our data frame of results.


#loop for M2
for(i in 1:length(environment_data$Depth_M2_cm)) {
  Sx <- survival(depth = environment_data$Depth_M2_cm[i], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = environment_data$Depth_M2_cm[i]) *
                         rep.temp (temp = environment_data$Temp_nat[i])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = environment_data$mon[i])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_M2 <- L %*% N_M2
  
  results[i,1] <- sum(N_M2)
  results[i,2] <- sum(N_M2[60:501])
}


#here is what our results look like from the simulation
results <- as.data.frame(results)

#lets plot the values we get by date to visualize the results
plot(x=environment_data$Date, y = results$pop_size_M2/80000)
plot(x=environment_data$Date, y = results$no.mature_M2/80000)




#----------------------------------------------------------------------------
####Simulation for initial population size####
#----------------------------------------------------------------------------

#---------step 1) input data-------

#Initial size
N_100 <- matrix(data = c(100,rep.int(0,times = 500)))
N_1000 <- matrix(data = c(1000,rep.int(0,times = 500)))
N_10000 <- matrix(data = c(10000,rep.int(0,times = 500)))

#matrix of environmental data

environment_10yr <- tibble(depth = rep(environment_data$Depth_M2_cm, times = 10),
                           temp = rep(environment_data$Temp_nat, times = 10),
                           year = c(rep(1, times = 366),
                                    rep(2, times = 366),
                                    rep(3, times = 366),
                                    rep(4, times = 366),
                                    rep(5, times = 366),
                                    rep(6, times = 366),
                                    rep(7, times = 366),
                                    rep(8, times = 366),
                                    rep(9, times = 366),
                                    rep(10, times = 366)),
                           month = rep(month(environment_data$Date), times = 10),
                           day = rep(day(environment_data$Date), times = 10),
                           julian = rep(yday(environment_data$Date), times = 10))

environment_10yr <- as.matrix(environment_10yr)

#matrix for results

results_int_100 <- tibble(year = environment_10yr[,3],
                  month = environment_10yr[,4],
                  day = environment_10yr[,5],
                  julian = environment_10yr[,6],
                  pop_size = rep.int(0,times = length(environment_10yr[,1])))

results_int_100 <- results_int_100
results_int_1000 <- results_int_100
results_int_10000 <- results_int_100

#----------Step 2) Loops for each initial size --------------#

###100

for(i in 1:length(environment_10yr[,1])) {
  Sx <- survival(depth = environment_10yr[i,1], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = environment_10yr[i,1]) *
                         rep.temp (temp = environment_10yr[i,2])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = environment_10yr[i,4])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_100 <- L %*% N_100
  
  results_int_100$pop_size[i] <- sum(N_100)
}


###1000

for(i in 1:length(environment_10yr[,1])) {
  Sx <- survival(depth = environment_10yr[i,1], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = environment_10yr[i,1]) *
                         rep.temp (temp = environment_10yr[i,2])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = environment_10yr[i,4])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx

  N_1000 <- L %*% N_1000
  
  results_int_1000$pop_size[i] <- sum(N_1000)
}

###10000

for(i in 1:length(environment_10yr[,1])) {
  Sx <- survival(depth = environment_10yr[i,1], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = environment_10yr[i,1]) *
                         rep.temp (temp = environment_10yr[i,2])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = environment_10yr[i,4])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_10000 <- L %*% N_10000
  
  results_int_10000$pop_size[i] <- sum(N_10000)
}

#--------------Step 3) calculate population growth ---------------

#100

lambda_100 <- mean(results_int_100$pop_size[results_int_100$julian == 32]/lag(results_int_100$pop_size[results_int_100$julian == 32]),na.rm = T)
lambda_100

r_100 <- log(lambda_100)
r_100

#1000

lambda_1000 <- mean(results_int_1000$pop_size[results_int_1000$julian == 32]/lag(results_int_1000$pop_size[results_int_1000$julian == 32]),na.rm = T)
lambda_1000

r_1000 <- log(lambda_1000)
r_1000

#10000

lambda_10000 <- mean(results_int_10000$pop_size[results_int_10000$julian == 32]/lag(results_int_10000$pop_size[results_int_10000$julian == 32]),na.rm = T)
lambda_10000

r_10000 <- log(lambda_10000)
r_10000


###no differences in initial size so I will just use 100


#----------------------------------------------------------------------------
####Simulation to chech stable size structure####
#----------------------------------------------------------------------------

#---------step 1) input data-------

N_100 <- matrix(data = c(100,rep.int(0,times = 500)))

environment_10yr <- tibble(depth = rep(environment_data$Depth_M2_cm, times = 10),
                           temp = rep(environment_data$Temp_nat, times = 10),
                           year = c(rep(1, times = 366),
                                    rep(2, times = 366),
                                    rep(3, times = 366),
                                    rep(4, times = 366),
                                    rep(5, times = 366),
                                    rep(6, times = 366),
                                    rep(7, times = 366),
                                    rep(8, times = 366),
                                    rep(9, times = 366),
                                    rep(10, times = 366)),
                           month = rep(month(environment_data$Date), times = 10),
                           day = rep(day(environment_data$Date), times = 10),
                           julian = rep(yday(environment_data$Date), times = 10))

results_int_100 <- tibble(year = environment_10yr[,3],
                          month = environment_10yr[,4],
                          day = environment_10yr[,5],
                          julian = environment_10yr[,6],
                          pop_size = rep.int(0,times = length(environment_10yr[,1])))

#----------Step 2) Loops for each year --------------#

par(mfrow = c(3,4))

###100

for(j in 1:length(unique(environment_10yr$year))) {
  
  env_temp <- environment_10yr %>% filter(year == j)

for(i in 1:366) {
  Sx <- survival(depth = env_temp$depth[i], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = env_temp$depth[i]) *
                         rep.temp (temp = env_temp$temp[i])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = env_temp$month[i])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_100 <- L %*% N_100
}
print(sum(N_100))
plot(N_100)
}

par(mfrow = c(1,1))

###stable size structure has been met

#----------------------------------------------------------------------------
####Isocline Simulations data####
#----------------------------------------------------------------------------


#------------------Step 1) input data --------------------#

#population vector
N_LILA_int <- matrix(data = c(100,rep.int(0,times = 500)))
N_WCA02_int <- matrix(data = c(100,rep.int(0,times = 500)))
N_WCA03_int <- matrix(data = c(100,rep.int(0,times = 500)))

#Environmental Data

LILA_10yr <- tibble(depth = rep(environment_data$Depth_M2_cm, times = 10),
                           temp = rep(environment_data$Temp_nat, times = 10),
                           year = c(rep(1, times = 366),
                                    rep(2, times = 366),
                                    rep(3, times = 366),
                                    rep(4, times = 366),
                                    rep(5, times = 366),
                                    rep(6, times = 366),
                                    rep(7, times = 366),
                                    rep(8, times = 366),
                                    rep(9, times = 366),
                                    rep(10, times = 366)),
                           month = rep(month(environment_data$Date), times = 10),
                           day = rep(day(environment_data$Date), times = 10),
                           julian = rep(yday(environment_data$Date), times = 10))

LILA_10yr <- as.matrix(LILA_10yr)

WCA02_10yr<- tibble(depth = rep(WCA_depth_summ$depth_ave[WCA_depth_summ$wetland == "WCA02"], times = 10),
                    temp = rep(WCA_temp_summ$temp_ave, times = 10),
                    year = c(rep(1, times = 365),
                             rep(2, times = 365),
                             rep(3, times = 365),
                             rep(4, times = 365),
                             rep(5, times = 365),
                             rep(6, times = 365),
                             rep(7, times = 365),
                             rep(8, times = 365),
                             rep(9, times = 365),
                             rep(10, times = 365)),
                    month = rep(month(WCA_temp_summ$date), times = 10),
                    day = rep(day(WCA_temp_summ$date), times = 10),
                    julian = rep(yday(WCA_temp_summ$date), times = 10))

WCA02_10yr <- as.matrix(WCA02_10yr)

WCA03_10yr<- tibble(depth = rep(WCA_depth_summ$depth_ave[WCA_depth_summ$wetland == "WCA03"], times = 10),
                    temp = rep(WCA_temp_summ$temp_ave, times = 10),
                    year = c(rep(1, times = 365),
                             rep(2, times = 365),
                             rep(3, times = 365),
                             rep(4, times = 365),
                             rep(5, times = 365),
                             rep(6, times = 365),
                             rep(7, times = 365),
                             rep(8, times = 365),
                             rep(9, times = 365),
                             rep(10, times = 365)),
                    month = rep(month(WCA_temp_summ$date), times = 10),
                    day = rep(day(WCA_temp_summ$date), times = 10),
                    julian = rep(yday(WCA_temp_summ$date), times = 10))

WCA03_10yr <- as.matrix(WCA03_10yr)

LILA_5yr <- tibble(depth = rep(environment_data$Depth_M2_cm, times = 5),
                    temp = rep(environment_data$Temp_nat, times = 5),
                    year = c(rep(1, times = 366),
                             rep(2, times = 366),
                             rep(3, times = 366),
                             rep(4, times = 366),
                             rep(5, times = 366)),
                    month = rep(month(environment_data$Date), times = 5),
                    day = rep(day(environment_data$Date), times = 5),
                    julian = rep(yday(environment_data$Date), times = 5))

LILA_5yr <- as.matrix(LILA_5yr)

WCA02_5yr<- tibble(depth = rep(WCA_depth_summ$depth_ave[WCA_depth_summ$wetland == "WCA02"], times = 5),
                    temp = rep(WCA_temp_summ$temp_ave, times = 5),
                    year = c(rep(1, times = 365),
                             rep(2, times = 365),
                             rep(3, times = 365),
                             rep(4, times = 365),
                             rep(5, times = 365)),
                    month = rep(month(WCA_temp_summ$date), times = 5),
                    day = rep(day(WCA_temp_summ$date), times = 5),
                    julian = rep(yday(WCA_temp_summ$date), times = 5))

WCA02_5yr <- as.matrix(WCA02_5yr)

WCA03_5yr<- tibble(depth = rep(WCA_depth_summ$depth_ave[WCA_depth_summ$wetland == "WCA03"], times = 5),
                    temp = rep(WCA_temp_summ$temp_ave, times = 5),
                    year = c(rep(1, times = 365),
                             rep(2, times = 365),
                             rep(3, times = 365),
                             rep(4, times = 365),
                             rep(5, times = 365)),
                    month = rep(month(WCA_temp_summ$date), times = 5),
                    day = rep(day(WCA_temp_summ$date), times = 5),
                    julian = rep(yday(WCA_temp_summ$date), times = 5))

WCA03_5yr <- as.matrix(WCA03_5yr)


results_temporary_LILA <- tibble(year = LILA_5yr[,3],
                           month = LILA_5yr[,4],
                           day = LILA_5yr[,5],
                           julian = LILA_5yr[,6],
                           pop_size = rep.int(0,times = length(LILA_5yr[,1])))

results_temporary_WCA <- tibble(year = WCA02_5yr[,3],
                       month = WCA02_5yr[,4],
                       day = WCA02_5yr[,5],
                       julian = WCA02_5yr[,6],
                       pop_size = rep.int(0,times = length(WCA02_5yr[,1])))

#------------------------------step 2) combinations of parameters for simulations ---------------------------#

kgrowths <- seq(0.01,0.09, by = 0.005)
S1 <- 0.987 - (c(0.05,0.1,0.15,0.2,0.3,0.5,0)*0.987)
S2 <- 0.987 - (c(0.05,0.1,0.15,0.2,0.3,0.5,0)*0.987)

threshold_data <- as_tibble(expand.grid(k = kgrowths,S1 = S1,S2 = S2)) %>% 
  mutate(r_LILA = 0,
         r_WCA02 = 0,
         r_WCA03 = 0)

threshold_data <- as.matrix(threshold_data)

#--------------------------step 3) For loops  for initial population size#----------------------

###initial 10 year simulation for LILA

for(i in 1:length(LILA_10yr[,1])) {
  Sx <- survival(depth = LILA_10yr[i,1], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = LILA_10yr[i,1]) *
                         rep.temp (temp = LILA_10yr[i,2])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = LILA_10yr[i,4])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_LILA_int <- L %*% N_LILA_int
}

###initial 10 year simulation for WCA02

for(i in 1:length(WCA02_10yr[,1])) {
  Sx <- survival(depth = WCA02_10yr[i,1], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = WCA02_10yr[i,1]) *
                         rep.temp (temp = WCA02_10yr[i,2])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = WCA02_10yr[i,4])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_WCA02_int <- L %*% N_WCA02_int
}

###initial 10 year simulation for WCA03

for(i in 1:length(WCA03_10yr[,1])) {
  Sx <- survival(depth = WCA03_10yr[i,1], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = WCA03_10yr[i,1]) *
                         rep.temp (temp = WCA03_10yr[i,2])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = WCA03_10yr[i,4])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_WCA03_int <- L %*% N_WCA03_int
}

#check the populaiton vectors
plot(N_LILA_int)
plot(N_WCA02_int)
plot(N_WCA03_int)


#--------------------------step 4) For loop for population simulations to get isocline data#----------------------


###For LILA

for(j in 1:length(threshold_data[,1])) {
  
  #restarts the similuation using our inital population structure
  N_LILA <- N_LILA_int 
  
  for(i in 1:1830) {
    Sx <- survival(depth = LILA_5yr[i,1], size = growth(age = 1:500,growth.rate = threshold_data[j,1]), age = 1:500,
                   Surv1 = threshold_data[j,2],Surv2 = threshold_data[j,3])
    Fx <- c(0,round(x = (frac.females *
                           egg.number * 
                           rep.depth(depth = LILA_5yr[i,1]) *
                           rep.temp (temp = LILA_5yr[i,2])*
                           sex.mature.ratio(growth(age = 1:500,growth.rate = threshold_data[j,1]))*
                           rep.season(month = LILA_5yr[i,4])),
                    digits = 3))
    
    L <- odiag(Sx, at = -1)
    L[1,] <- Fx
    
    N_LILA <- L %*% N_LILA
    results_temporary_LILA$pop_size[i] <- sum(N_LILA)
  }
  threshold_data[j,4] <- log(mean(results_temporary_LILA$pop_size[results_temporary_LILA$julian == 32]/lag(results_temporary_LILA$pop_size[results_temporary_LILA$julian == 32]),na.rm = T))
  print(j)
}


###For WCA02

for(j in 1:length(threshold_data[,1])) {
  
  #restarts the similuation using our inital population structure
  N_WCA02 <- N_WCA02_int 
  
  for(i in 1:1825) {
    Sx <- survival(depth = WCA02_5yr[i,1], size = growth(age = 1:500,growth.rate = threshold_data[j,1]), age = 1:500,
                   Surv1 = threshold_data[j,2],Surv2 = threshold_data[j,3])
    Fx <- c(0,round(x = (frac.females *
                           egg.number * 
                           rep.depth(depth = WCA02_5yr[i,1])*
                           rep.temp (temp = WCA02_5yr[i,2])*
                           sex.mature.ratio(growth(age = 1:500,growth.rate = threshold_data[j,1]))*
                           rep.season(month = WCA02_5yr[i,4])),
                    digits = 3))
    
    L <- odiag(Sx, at = -1)
    L[1,] <- Fx
    
    N_WCA02 <- L %*% N_WCA02
    results_temporary_WCA$pop_size[i] <- sum(N_WCA02)
  }
  threshold_data[j,5] <- log(mean(results_temporary_WCA$pop_size[results_temporary_WCA$julian == 32]/lag(results_temporary_WCA$pop_size[results_temporary_WCA$julian == 32]),na.rm = T))
  print(j)
}

###For WCA03

for(j in 1:length(threshold_data[,1])) {
  
  #restarts the similuation using our inital population structure
  N_WCA03 <- N_WCA03_int 
  
  for(i in 1:1825) {
    Sx <- survival(depth = WCA03_5yr[i,1], size = growth(age = 1:500,growth.rate = threshold_data[j,1]), age = 1:500,
                   Surv1 = threshold_data[j,2],Surv2 = threshold_data[j,3])
    Fx <- c(0,round(x = (frac.females *
                           egg.number * 
                           rep.depth(depth = WCA03_5yr[i,1])*
                           rep.temp (temp = WCA03_5yr[i,2])*
                           sex.mature.ratio(growth(age = 1:500,growth.rate = threshold_data[j,1]))*
                           rep.season(month = WCA03_5yr[i,4])),
                    digits = 3))
    
    L <- odiag(Sx, at = -1)
    L[1,] <- Fx
    
    N_WCA03 <- L %*% N_WCA03
    results_temporary_WCA$pop_size[i] <- sum(N_WCA03)
  }
  threshold_data[j,6] <- log(mean(results_temporary_WCA$pop_size[results_temporary_WCA$julian == 32]/lag(results_temporary_WCA$pop_size[results_temporary_WCA$julian == 32]),na.rm = T))
  print(j)
}


threshold_data <- as_tibble(threshold_data)

write_csv(threshold_data, file = here("Pomacea/Isocline_manuscript/out","threshold_data.csv"))
