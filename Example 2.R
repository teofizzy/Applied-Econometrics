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
require(ggplot2)
require(tidyverse)
library(urca)
library(rmarkdown)
library(vars)
library(forecast)
rp.lm<-lm(gdp~inflation + pop + interest, data = rp_df)
rp.lm
summary(rp.lm)
rp_df<-data.frame(rp$inflation, rp$gdp, rp$population, rp$interest)
rp_df
names(rp_df)<-c("inflation", "gdp", "pop", "interest")
rp_df
cor(rp_df)

# declare the variables as time series objects
gdp<-ts(rp_df$gdp)
inf<-ts(rp_df$inflation)
pop<-ts(rp_df$pop)
int<-ts(rp_df$interest)


# by default frequqnecy is 1 because the data is annual.

rp_d.set<-cbind(gdp, inf, pop, int) # bind into a system
rp_d.set

# lag selection
lagselect <- VARselect(rp_d.set, lag.max = 7, type = "const") # say we subject it to 7 lags
#| and since this is a typical lag selection, use const
lagselect
#6 appears the most times, thus R suggests it is the optimal no. of lags
# but we use 6-1 = 5 lags, thus lag order = 5 lags in this case

# Johansen testing
# trace approach

ctest1t <- ca.jo(rp_d.set, type = "trace", ecdet = "const", K = 5)
summary(ctest1t)
# the critical values for the levels are shown below, choose 5pct for social sciences
# when r = 0, the test stat is 190.99 > 53.12(5pct level), when r = 0 we reject the null 
#|hypothesis there is at least 1 cointegrating relation, we state that r should be at least 1.
# when r <= 1? we also reject the hypothesis that r = 1, implying that there are at least 2 
#|cointegrating relationships in our system
# when r <= 2? we reject the hypothesis that r=2, implying that there are at least 3 cointegrating
#|relationships in our system.
# when r <= 3? We reject null, implying that there are at least 4 cointegrating relationships in 
#|our system

# maximum eigen-value approach
ctest1e <- ca.jo(rp_d.set, type = "eigen", ecdet = "const", K = 5)
summary(ctest1e)
# when r = 0, 112.83 > 28.14 (5pct level), thus we reject null
# when r <= 1, 50.86 > 22.00, we reject null
# when r <=...3, similar implications as the trace approach
