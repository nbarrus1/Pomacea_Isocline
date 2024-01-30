#-----------------------------------------------------------------------------------#
######Install packages (if not installed)#####
#----------------------------------------------------------------------------------#
install.packages("tidyverse")
install.packages("demogR")

#-----------------------------------------------------------------------------------#
###### load libraries ########
#-----------------------------------------------------------------------------------#

library(tidyverse)                        #tidyverse contains readxl
library(demogR)                           #for building transition matrix

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

survival <- function(depth, size, age) {
  if(depth > 0) {
                surv = if_else(size <= 16, true = 0.987, false = 0.99/(1+exp(-0.1*(500 - age))))
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
  if_else(month == "February"| month == "March"| month == "April"|month == "May",
          true = 1,
          false = if_else(month == "June"| month=="July"| month == "August",
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

#from the readxl package this function reads in files with format ~.xlsx, sheet specifies which sheet
#is used. 
library(readxl)

environment_data <- read_xlsx("EnviroData_DBHYDRO_SnailModel.xlsx", sheet = 3)   
environment_data
#IT reads the dates into the file as dates as well!! 
#convert our depths to cm, let also create a variable called mon for the month that each day 
#is in this will be used for our seaonal aspect on fertility
environment_data <- environment_data %>% 
  mutate(Depth_M2_cm = (Depth_M2_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         Depth_M4_cm = (Depth_M4_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         mon = months(environment_data$Date))            #our vector of months

#take a quick look at the data
#temperature
plot(Temp_c~Date, data = environment_data, type = "l")
#depth
plot(Depth_M2_cm~Date, data = environment_data, type = "l")   #M2 Depths
lines(Depth_M4_cm~Date, data = environment_data, col = "red") #M4 Depths

#chech fertility seasonality see figure A1-4
plot(x = environment_data$Date, y = rep.season(month = environment_data$mon), type = "l")

#####Create model that constantly updates with environmental variables####

#create data frame for model results
length(environment_data$Depth_M2_cm)

results <- tibble(pop_size_M2 = rep.int(0,times = 518)) %>% 
  mutate(no.mature_M2 = rep.int(0,times = 518),
         pop_size_M4 = rep.int(0,times = 518),
         no.mature_M4 = rep.int(0,times = 518))

#give our starting number of snails and our population vector
N_M2 <- matrix(data = c(100,rep.int(0,times = 500)))
N_M4 <- matrix(data = c(100,rep.int(0,times = 500)))

#Below here there are two loops, one fro M2 and another for M4, that take our daily measures of 
#depth and temperature and creates a Leslie matrix for that given day. Then its going to multiply
#our population vector above by that days Lesilie matrix for the transition between that day and 
#the next day. Its going to do this for all our depth and temperature in order from our input data.
#Then we are going to save the population size and number mature snails after the transition
#into our data frame of results.


#loop for M2
for(i in 1:518) {
  Sx <- survival(depth = environment_data$Depth_M2_cm[i], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = environment_data$Depth_M2_cm[i]) *
                         rep.temp (temp = environment_data$Temp_c[i])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = environment_data$mon[i])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_M2 <- L %*% N_M2
  
  results$pop_size_M2[i] <- sum(N_M2)
  results$no.mature_M2[i] <- sum(N_M2[60:501])
}

####loop for M4
for(i in 1:518) {
  Sx <- survival(depth = environment_data$Depth_M4_cm[i], size = growth(age = 1:500), age = 1:500)
  Fx <- c(0,round(x = (frac.females *
                         egg.number * 
                         rep.depth(depth = environment_data$Depth_M4_cm[i]) *
                         rep.temp (temp = environment_data$Temp_c[i])*
                         sex.mature.ratio(growth(age = 1:500))*
                         rep.season(month = environment_data$mon[i])),
                  digits = 3))
  
  L <- odiag(Sx, at = -1)
  L[1,] <- Fx
  
  N_M4 <- L %*% N_M4
  
  results$pop_size_M4[i] <- sum(N_M4)
  results$no.mature_M4[i] <- sum(N_M4[60:501])
}

#here is what our results look like from the simulation
results

#lets plot the values we get by date to visualize the results
plot(x=environment_data$Date, y = results$pop_size_M2/80000)
plot(x=environment_data$Date, y = results$no.mature_M2/80000)
plot(x=environment_data$Date, y = results$pop_size_M4/80000)
plot(x=environment_data$Date, y = results$no.mature_M4/80000)


