## Objective: Model wage = B1 + B2Educ + Ui
wagei<-c(44.44,16.00,15.38,13.54,25.00,24.05,40.95,26.45,30.76,34.57)
length(wagei)
educi<-c(13,14,18,13,13,16,16,18,21,14)
length(educi)
wage_as_df<-data.frame(wagei,educi)
wage_model<-lm(wagei~educi, data = wage_as_df)
wage_model
require(ggplot2)
require(dplyr)
require(broom)
mean(wagei)
mean(educi)

##b2-correct
educi-mean(educi)
as.matrix(educi-mean(educi))
mean_diff_sqd<-(as.matrix(educi-mean(educi)))^2
mean_diff_sqd
b2_denom_correct<-sum(mean_diff_sqd)
wagei-mean(wagei)
as.matrix(wagei-mean(wagei))
yhat<-as.matrix(wagei-mean(wagei))
xhat<-as.matrix(educi-mean(educi))
(yhat*xhat)
-2.6*17.326
b2_num_correct<-sum(yhat*xhat)
b2_denom_correct
b2_num_correct/b2_denom_correct
b2<-b2_num_correct/b2_denom_correct
b2
 ## teo's code for b2 in slr
x-mean(x)
as.matrix(x-mean(x))

xi_minus_mean_of_x<-as.matrix(x-mean(x))
sq_of_x_minus_mean_of_x<-(x_minus_mean_of_x<-as.matrix(x-mean(x)))^2

yi_minus_mean_of_y<-as.matrix(y-mean(y))

num_of_b2<-sum(yi_minus_mean_of_y*xi_minus_mean_of_x)

denom_of_b2<-sum(sq_of_x_minus_mean_of_x)

value_of_b2<-num_of_b2/denom_of_b2

## teo's code for b1
b1<-mean(y)-(b2*(mean(x)))

b1<-mean(wagei)-(b2*(mean(educi))) ## trial with assignment
b1                    
qt(0.975,1198)
1.9615*0.1354
2.3968+0.2655871
0.2352/2
0.1183/((1-0.2366)/1197)
qf(0.975, 2, 1197)
