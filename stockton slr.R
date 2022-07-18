price_agst_age<-qplot(sprice, age, data = stockton5_small)
price_agst_age
reg_stockton<-lm(sprice ~ age, data = stockton5_small)
reg_stockton
summary(reg_stockton)
require(ggplot2)
require(dplyr)
require(broom)
require(ggpubr)
scale<-mutate(stockton5_small,sprice_new = sprice/1000 )
scale
view(scale)
price_new_agst_age<-lm(sprice_new~age, data= scale)
price_new_agst_age
summary(price_new_agst_age)
scatter_price_new<-qplot(sprice_new, age, data = scale)
scatter_price_new
#The value of the R sqd remains unchanged

## Properties of Regression coefficients
##estimating the standard errors of the reg coefficents
usd_value<-c(19.2,20.5,19.7,21.3,20.8, 19.9,17.8,17.2)
usd_value
week_sales
week_sales<-c(25.4,14.7,18.6,11.4,11.1,15.7,29.2,35.2)
usd_sales<-data.frame(usd_value, week_sales)
usd_sales
reg_usd_sales<-lm(week_sales~usd_value, data =usd_sales)
reg_usd_sales
summary(reg_usd_sales)
sd(usd_value)
sd(week_sales)
residuals(reg_usd_sales)
sum(residuals(reg_usd_sales))
res_sq<-(residuals(reg_usd_sales))^2
res_sq
sum(res_sq)
## conducting a test of significance, use the function t.test
t.test(usd_sales$week_sales, mu = 0, alt = "two.sided")
t.test(usd_sales$usd_value, mu = 0)
##two populations, var not equal, the two groups are not paired, they are independent
##t.test(pop1~pop2, mu = 0, alt = two.sided, conf = 0.95, var=F, paired=F)
-6.0247/0.5317
qt()
coef(summary(reg_usd_sales))
predict(reg_usd_sales)
y.fitted <- reg_usd_sales$fitted.values # Extract the fitted values of y
y.fitted
sse <- sum((week_sales - y.fitted)^2)
sse
mse <- sse / (8 - 2)#n=8, df
mse
qt(p=0.025, df = 6)
?confint
b0 <- reg_usd_sales$coefficients[1]
b1 <- reg_usd_sales$coefficients[2]
b0
b1
t.val <- qt(0.975, 6) # Critical value of t
t.val
require(ggplot2)
require(askpass)
require(tidyverse)
b1.conf.upper <- b1 + t.val * sqrt(mse) / sqrt(sum((usd_value - mean(usd_value))^2))
b1.conf.lower <- b1 - t.val * sqrt(mse) / sqrt(sum((usd_value - mean(usd_value))^2))
b1.conf.lower
b1.conf.upper

## Example mlr
qt(0.975,1198)
2.39968/0.1354
qt(0.025,1198)
0.1354*1.9695
2.3969+0.1354*1.9695
1-0.2353
q_dem<-c(100,75,80,70,50,65,90,100,110,60)
pri<-c(5,7,6,6,8,7,5,4,3,9)
con_in<-c(1000,600,1200,500,300,400,1300,1100,1300,300)
length(q_dem)
length(con_in)
length(pri)
q_d_df<-data.frame(q_dem, pri, con_in)
q_d_df
qd_fitted<-lm(q_dem~pri+con_in, data = q_d_df)
qd_fitted
summary(qd_fitted)
residuals(qd_fitted)
(residuals(qd_fitted))^2
su_sqd<-(sum((residuals(qd_fitted))^2))/7
su_sqd
mean(pri)
sum((as.matrix(pri-mean(pri)))^2)
s_x2_x3<-(sum(((as.matrix(con_in-mean(con_in))))*((as.matrix(pri-mean(pri))))))/9
s_x2_x3
s_x2<-sqrt((sum((as.matrix(pri-mean(pri)))^2))/9)
s_x2
s_x3<-sqrt((sum((as.matrix(con_in-mean(con_in)))^2))/9)
s_x3
cov_x2_x3<-s_x2_x3/(s_x2*s_x3)
(cov_x2_x3)^2
cov.wt(q_d_df)

##teo's code for s.e, bj, in mlr computations
su_sq_d<-(sum((residuals(y.fitted))^2))/n-k
sx2_x3<-(sum(((as.matrix(x2-mean(x2))))*((as.matrix(x3-mean(x3))))))/n-1
sx2<-sqrt((sum((as.matrix(x2-mean(x2)))^2))/n-1)
sx3<-sqrt((sum((as.matrix(x3-mean(x3)))^2))/n-1)
covx2_x3<-sx2_x3/(sx2*sx3)
(covx2_x3)^2
##to use covx2_x3 in our formula for s.e(b2), we find its square
s.e_b2<-sqrt((su_sq_d/sum((as.matrix(x2-mean(x2)))^2))*1/(1-(covx2_x3)^2))
s.e_b2
qt(0.025,7)
qt(0.975,7)
