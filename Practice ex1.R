38/5993267
Prob <- data.frame(age = c(0,1,2,3,4), Male = c(552508,580856,614005,619989,638986), Female = c(552528,573920,610705,621941,627675), intersex = c(38,29,33,26,28), total = c(1105074, 1154805,1224743,1241956,1266689))
Prob
Prob [1,2]/sum(Prob[,5])
Prob [1,3]/sum(Prob[,5])
Prob [1,4]/sum(Prob[,5])
Total_prob<-sum(Prob[,5])
Total_prob
Total<-Total_prob
Total
P0_Male<-Prob[1,2]/Total
P0_Female<-Prob[1,3]/ Total
P0_inter<-Prob[1,4]/Total
Joint_probs_Age_0 <- matrix(c(Prob[1,2]/Total,Prob[1,3]/ Total,Prob[1,4]/Total), ncol = 1)
Joint_probs_Age_0
P1_Male<-Prob[2,2]/Total
P1_Female<-Prob[2,3]/ Total
P1_inter<-Prob[2,4]/ Total
P2_Male<-Prob[3,2]/Total
P2_Female<-Prob[3,3]/ Total
P2_inter<-Prob[3,4]/Total
P3_Male<-Prob[4,2]/ Total
P3_Female<-Prob[4,3]/ Total
P3_inter<-Prob[4,4]/ Total
P4_Male<-Prob[5,2]/ Total
P4_Female<-Prob[5,3]/ Total
P4_inter<-Prob[5,4]/ Total
Joint_Probs<- matrix(c(P0_Male,P0_Female,P0_inter,P1_Male, P1_Female,P1_inter,P2_Male,P2_Female,P2_inter,P3_Male,P3_Female,P3_inter,P4_Male,P4_Female,P4_inter),ncol =3)
Joint_probs_df<-as.data.frame(Joint_Probs)
names(Joint_probs_df)<-c("male", "female", "intersex")
Joint_probs_df
sum(Joint_Probs[,1:3])
Marginal_probs_of_male <-sum(Joint_Probs[,1])
Marginal_probs_of_male
Marginal_probs_of_female <-sum(Joint_Probs[,2])
Marginal_probs_of_intersex<-sum(Joint_Probs[,3])
Marginal_Probs_of_Age_0<-sum(Joint_Probs[1,])
Marginal_probs_of_Age_1<-sum(Joint_Probs[2,])
Marginal_Probs_of_Age_2<-sum(Joint_Probs[3,])
Marginal_Probs_of_Age_3<-sum(Joint_Probs[4,])
Marginal_Probs_Of_Age_4<-sum(Joint_Probs[5,])
Marginal_Probs_by_Pronouns<-matrix(c(Marginal_probs_of_male,Marginal_probs_of_female,Marginal_probs_of_intersex), ncol=1)
marginal.probs.by.pronouns<-as.list(Marginal_Probs_by_Pronouns)
marginal.probs.by.pronouns
names(marginal.probs.by.pronouns)<-c("male", "female", "intersex")
marginal.probs.by.pronouns
Marginal_Probs_by_Age<-matrix(c(Marginal_Probs_of_Age_0, Marginal_probs_of_Age_1,Marginal_Probs_of_Age_2,Marginal_Probs_of_Age_3,Marginal_Probs_Of_Age_4),ncol = 1)
Marginal_Probs_by_Age
sum(Marginal_Probs_of_Age_0,Marginal_probs_of_Age_1,Marginal_Probs_of_Age_2,Marginal_Probs_of_Age_3,Marginal_Probs_Of_Age_4)
sum(Marginal_probs_of_female,Marginal_probs_of_intersex,Marginal_probs_of_male)
P0_Male
Prob
Prob_age_0<-Prob[1,5]/Total
Prob_age_0
Prob_age_1<-Prob[2,5]/Total
Prob_age_2<-Prob[3,5]/Total
Prob_age_3<-Prob[4,5]/Total
Prob_age_4<-Prob[5,5]/Total
Condprob_age_0_g_male<-Joint_Probs[1,1]/sum(Joint_Probs[,1])
Prob_age_1
Condprob_age_0_g_male
Prob
Prob_male<-sum(Prob[,2])/Total
Prob_male
Prob_female<-sum(Prob[,3])/Total
Prob_female
Prob_intersex<-sum(Prob[,4])/Total
Prob_intersex
