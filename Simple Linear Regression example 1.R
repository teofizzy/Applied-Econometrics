require(tidyverse)
require(ggplot2)
require(askpass)
library(ggpubr)
library(dplyr)
library(broom)

week<-c(1,2,3,4,5,6,7,8)
USD<-c(19.2,20.5,19.7,21.3,20.8,19.9,17.8,17.2)
sales<-c(25.4,14.7,18.6,11.4,11.1,15.7,29.2,35.2)
whisk<-data.frame(week, USD, sales)
whisk
plot(USD~sales, data = whisk)
hist(whisk$USD)
reg1<-lm(sales~USD, data = whisk)
reg1
predict(lm(sales~USD, data = whisk))
fitted(lm(sales~USD, data = whisk))
anova(lm(sales~USD, data = whisk))
summary(lm(sales~USD, data = whisk))
summary(reg1)
mean(sales)
mp<-mean(USD)
-6.0247-(mean(USD))
mean(sales)--6.0247*(mean(USD))
residuals(reg1)
sum(residuals(reg1))
res<-(residuals(reg1))^2
res
sum(res)
res/6
sum(res)/6
x_sq<-(USD-mean(USD))
x_sq
(x_sq)^2
sum((x_sq)^2)
