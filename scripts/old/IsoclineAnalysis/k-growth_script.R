rm(list = ls())

growth <- function(age, int.size = 3, fin.size = 50, growth.rate = 0.05){
  size.mm = (int.size*exp(growth.rate*age))/(1+((int.size/fin.size)*(exp(growth.rate*age)-1)))
}

data <- data.frame(fast = growth(age = 1:500), slow = growth(age = 1:500, growth.rate = 0.01),
                   med = growth(age = 1:500, growth.rate = 0.03))
data$per.fast <- c((data$fast[-1]/data$fast[-length(data$fast)])-1, NA)
data$per.slow <- c((data$slow[-1]/data$slow[-length(data$slow)])-1, NA)
data$per.med <- c((data$med[-1]/data$med[-length(data$med)])-1, NA)

plot(per.fast~fast, data = data, type = "l", xlab = "initial size", ylab = "daily proportional growth")
lines(per.slow~slow, data = data, col = "red")
lines(per.med~med, data = data, col = "green")
legend("topright",
       legend = c("k = 0.05", "k = 0.03", "k = 0.01"),
       col = c("black", "green", "red"),
       lwd = 1)
data$ln.fast <- c((log(data$fast[-1])-log(data$fast[-length(data$fast)])),NA)


library(tidyverse)


t.week <- as_tibble(data) %>% 
  filter(row_number() %% 28 == 1) %>% 
  mutate(per.fast = c(fast[-1]/fast[-length(fast)]-1, NA),
         per.med = c(med[-1]/med[-length(med)]-1, NA),
         per.slow = c(slow[-1]/slow[-length(slow)]-1, NA),
         week.fast = per.fast/28,
         week.slow = per.slow/28,
         week.med = per.med/28,
         ln.fast = c((log(fast[-1])-log(fast[-length(fast)]))/28, NA))

plot(week.fast~fast, data = t.week)
points(week.slow~slow, data = t.week, col = "red")
points(week.med~med, data = t.week, col = "green")

par(mfrow = c(2,2))

plot(per.fast~fast, data = data, type = "l")
lines(week.fast~fast, data = t.week, pch = 3, col = "red")
lines(ln.fast~fast, data = t.week, col = "green")

fit <- lm(data$per.fast~data$fast)
fit.2 <- lm(week.fast~fast, data = t.week)
fit.2lnh <- lm(t.week$ln.fast[1:16]~t.week$fast[1:16])
fit.2ln <- lm(t.week$ln.fast~t.week$fast)

summary(fit)
summary(fit.2)
summary(fit.2ln)
summary(fit.2lnh)
#slow
t.week <- as_tibble(data) %>% 
    filter(row_number() %% 7 == 1) %>% 
  mutate(per.fast = c(fast[-1]/fast[-length(fast)]-1, NA),
         per.med = c(med[-1]/med[-length(med)]-1, NA),
         per.slow = c(slow[-1]/slow[-length(slow)]-1, NA),
         week.fast = per.fast/7,
         week.slow = per.slow/7,
         week.med = per.med/7,
         ln.fast = c((log(slow[-1])-log(slow[-length(slow)]))/7, NA))

plot(per.slow~slow, data = data, type = "l")
lines(week.slow~slow, data = t.week, pch = 3, col = "red")
lines(ln.fast~fast, data = t.week, col = "green")

fit <- lm(data$per.slow~data$slow)
fit.2 <- lm(week.slow~slow, data = t.week)
fit.2lnh <- lm(t.week$ln.fast[1:3]~t.week$fast[1:3])
fit.2ln <- lm(t.week$ln.fast~t.week$fast)

summary(fit)
summary(fit.2)
summary(fit.2ln)
summary(fit.2lnh)

#med
t.week <- as_tibble(data) %>% 
  filter(row_number() %% 7 == 1) %>% 
  mutate(per.fast = c(fast[-1]/fast[-length(fast)]-1, NA),
         per.med = c(med[-1]/med[-length(med)]-1, NA),
         per.slow = c(slow[-1]/slow[-length(slow)]-1, NA),
         week.fast = per.fast/7,
         week.slow = per.slow/7,
         week.med = per.med/7,
         ln.fast = c((log(med[-1])-log(med[-length(med)]))/7, NA))

plot(per.med~med, data = data, type = "l")
lines(week.med~med, data = t.week, pch = 3, col = "red")
lines(ln.fast~fast, data = t.week, col = "green")

fit <- lm(data$per.med~data$med)
fit.2 <- lm(week.med~med, data = t.week)
fit.2lnh <- lm(t.week$ln.fast[1:13]~t.week$fast[1:13])
fit.2ln <- lm(t.week$ln.fast~t.week$fast)

summary(fit)
summary(fit.2)
summary(fit.2ln)
summary(fit.2lnh)

#vfast
data$fast <- growth(age = 1:500, growth.rate = 0.09)
data$per.fast <- c((data$fast[-1]/data$fast[-length(data$fast)])-1, NA)

t.week <- as_tibble(data) %>% 
  filter(row_number() %% 14 == 1) %>% 
  mutate(per.fast = c(fast[-1]/fast[-length(fast)]-1, NA),
         per.med = c(med[-1]/med[-length(med)]-1, NA),
         per.slow = c(slow[-1]/slow[-length(slow)]-1, NA),
         week.fast = per.fast/14,
         week.slow = per.slow/14,
         week.med = per.med/14,
         ln.fast = c((log(fast[-1])-log(fast[-length(fast)]))/14, NA))

plot(per.fast~fast, data = data, type = "l")
lines(week.fast~fast, data = t.week, pch = 3, col = "red")
lines(ln.fast~fast, data = t.week, col = "green")

fit <- lm(data$per.fast~data$fast)
fit.2 <- lm(week.fast~fast, data = t.week)
fit.2lnh <- lm(t.week$ln.fast[1:3]~t.week$fast[1:3])
fit.2ln <- lm(t.week$ln.fast~t.week$fast)

summary(fit)
summary(fit.2)
summary(fit.2ln)
summary(fit.2lnh)


growth <- function(age, int.size = 3, fin.size = 50, growth.rate = 0.05){
  size.mm = (int.size*exp(growth.rate*age))/(1+((int.size/fin.size)*(exp(growth.rate*age)-1)))
}

data2 <- data.frame(small = growth(age = 1:500), med1 = growth(age = 1:500, int.size = 5),
                   med2 = growth(age = 1:500, int.size = 7), large = growth(age = 1:500, int.size = 9))
data2$per.small <- c((data2$small[-1]/data2$small[-length(data2$small)])-1, NA)
data2$per.med1 <- c((data2$med1[-1]/data2$med1[-length(data2$med1)])-1, NA)
data2$per.med2 <- c((data2$med2[-1]/data2$med2[-length(data2$med2)])-1, NA)
data2$per.large <- c((data2$large[-1]/data2$large[-length(data2$large)])-1, NA)

plot(per.small~small, data = data2, xlab = "initial size", ylab = "daily proportional growth")
points(per.med1~med1, data = data2, col = "red")
points(per.med2~med2, data = data2, col = "green")
points(per.large~large, data = data2, col = "blue")

t.week2 <- as_tibble(data2) %>% 
  filter(row_number() %% 28 == 1) %>% 
  mutate(per.small = c(small[-1]/small[-length(small)]-1, NA),
         per.med1 = c(med1[-1]/med1[-length(med1)]-1, NA),
         per.med2 = c(med2[-1]/med2[-length(med2)]-1, NA),
         per.large = c(large[-1]/large[-length(large)]-1, NA),
         week.small = per.small/28,
         week.med1 = per.med1/28,
         week.med2 = per.med2/28,
         week.large = per.large/28,
         ln.small = c((log(small[-1])-log(small[-length(small)]))/28, NA),
         ln.med1= c((log(med1[-1])-log(med1[-length(med1)]))/28, NA),
         ln.med2 = c((log(med2[-1])-log(med2[-length(med2)]))/28, NA),
         ln.large = c((log(large[-1])-log(large[-length(large)]))/28, NA))

plot(week.small~small, data = t.week2)
points(week.med1~med1, data = t.week2, col = "red")
points(week.med2~med2, data = t.week2, col = "green")
points(week.large~large, data = t.week2, col = "blue")
points(ln.small~small, data = t.week2, col = "black", pch = 3)
points(ln.med1~med1, data = t.week2, col = "red", pch = 3)
points(ln.med2~med2, data = t.week2, col = "green",pch = 3)
points(ln.large~large, data = t.week2, col = "blue",pch = 3)

X.all <- c(t.week2$small, t.week2$med1,t.week2$med2,t.week2$large)
X.smallest <- c(t.week2$small[1], t.week2$med1[1],t.week2$med2[1],t.week2$large[1])
y.all <- c(t.week2$ln.small, t.week2$ln.med1,t.week2$ln.med2,t.week2$ln.large)
y.smallest <- c(t.week2$ln.small[1], t.week2$ln.med1[1],t.week2$ln.med2[1],t.week2$ln.large[1])

fit.all <- lm(y.all~X.all)
fit.smallest <- lm(y.smallest~X.smallest)

summary(fit.all)
summary(fit.smallest)
