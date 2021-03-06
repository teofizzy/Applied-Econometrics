require(ggplot2)
require(tidyverse)
require(broom)
require(ggpubr)
X1649256667723_collegetown
scatter1<-qplot(price,sqft, data = X1649256667723_collegetown)
scatter1
slr1<-lm(price~sqft, data = X1649256667723_collegetown)
slr1
summary(slr1)
abline(slr1)
abline(lm(X1649256667723_collegetown$price~X1649256667723_collegetown$sqft))
plotly<-plot(X1649256667723_collegetown$price, X1649256667723_collegetown$sqft,
     pch =20,
     col = "red",
     main = "scatterplot of price against square feet",
     xlab = "Price",
     ylab = "square feet")
abline(lm(X1649256667723_collegetown$price~X1649256667723_collegetown$sqft))
geom_abline(plotly)
geom_line(plotly)
ggplot(X1649256667723_collegetown, aes(x = sqft, y = price)) +
        geom_point(shape = 19) +
        geom_smooth(method = "lm", se = FALSE, colour = "red")

## conducting a granger causality test, first granger then reverse granger
library(lmtest)
grangertest(price ~ sqft, order = 3, data = X1649256667723_collegetown)
grangertest(sqft ~ price, order = 3, data = X1649256667723_collegetown)

##cointegration test

#step 1, run a regression
reg_long_run<-lm(price~sqft, data = X1649256667723_collegetown)
reg_long_run
resid_reg_long_run<-resid(reg_long_run)

#step2, conduct a stationarity test on the residuals
library(urca)
install.packages("urca")
library(urca)
#urca-unit root and cointegration package, df = dickey-fuller
#use none because we have done a regresiion already, select lags = AIC
y<-ur.df(resid_reg_long_run, type = "none", selectlags = "AIC")
y
summary(y)
y@teststat
y@cval
#from the above, -7 is less than -1, thus we reject the null hypothesis of non-stationarity
#|and conclude that the residuals are stationary which implies that these two variables are
#|cointegrated whiich means that there exists some long-run relationship between them.
?urca
