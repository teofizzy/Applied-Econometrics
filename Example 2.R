require(tidyverse)
require(ggplot2)
require(moments)
Rate_of_return<-c(-20,-10,10,25,30)
F_x<-c(0.10,0.15,0.45,0.25,0.05)
mean(Rate_of_return)
mean(x_F_x)
x_F_x<-(Rate_of_return*F_x)
x_F_x
sum(x_F_x)
summary(x_F_x)
skewness(x_F_x)
kurtosis(x_F_x)
var(x_F_x)
sd(x_F_x)
var(Rate_of_return)
sum(Rate_of_return-y)
y<-Rate_of_return-sum(x_F_x)
y
F_x * y^2
F_x * y^3
F_x * y^4
moment_4<-sum(F_x * y^4)
moment_4
moment_2<-sum(F_x * y^2)
moment_2
moment_3<-sum(F_x * y^3)
moment_3
moment(x_F_x, absolute = TRUE)
rt_1<-sqrt(moment_1)
rt_1
skew_1<-moment_3/(rt_1)^3
skew_1
kurt_1<-moment_4/(rt_1)^4
kurt_1
