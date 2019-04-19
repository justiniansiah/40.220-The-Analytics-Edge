#Q1)

#1)
software <- read.csv("software.csv")
str(software)
hist(software$spending,100)
qqnorm(software$spending)
# Use histogram or qqplot to see that the data doe not seem to be normally distributed
# Some people have very high spending amounts

#2)
tapply(software$spending,software$gender,mean) 
tapply(software$spending,software$gender,sd) 
# Mean - 209.8801 (female) 200.5620 (male)
# Standard deviation - 223.0011 (female) 218.7635 (male)
# Variance - 49729.48 (female) 47857.48 (male) 
#3)
t.test(software$spending[software$gender==1],software$spending[software$gender==0])
# H0: mu_male = mu_female, H1: mu_male is not equal to mu_female
# p value = 0.5052, You cannot reject the null hypothesis

#4)
m1 <- lm(spending~freq+last_update+first_update+web_order+gender+address_res+address_us,data=subset(software,partition=="t"))
summary(m1)
# freq, address_res

#5)
# beta_freq = 92.07, beta_address_res = -92.76
# scenario(A): 1 unit decrease in freq implies 92.07 decrease in spending
# scenario(B): move from 0 in address_res to 1 implies 92.76 decreasing in spending
# scenario (B) has a larger decrease in spending than scenario (A)

#6)
m2 <- lm(spending~freq+address_res,data=subset(software,partition=="t"))
summary(m2)
# Adjr2 for model 1 is 0.4949 and for model 2 is 0.4864. In terms of adjusted r squared
# model 1 (in question 4) is preferred

#7)
library(leaps)
m3 <- regsubsets(spending~freq+last_update+first_update+web_order+gender+address_res+address_us,data=subset(software,partition=="t"),method="backward")
summary(m3)
summary(m1)
# web_order is the first variable dropped. This has the largest p-value in the first model.

#8) 
which.max(summary(m3)$adjr2)
# Max adjr2 is for 4 variables - includes freq, last_update, address_res and address_us

#9)
m4 <-  lm(spending~freq+last_update+address_res+address_us,data=subset(software,partition=="t"))
p1 <- predict(m1,newdata=subset(software,partition=="v"))
p2 <- predict(m2,newdata=subset(software,partition=="v"))
p4 <- predict(m4,newdata=subset(software,partition=="v"))
sum((p1-subset(software,partition=="v")$spending)^2)
sum((p2-subset(software,partition=="v")$spending)^2)
sum((p4-subset(software,partition=="v")$spending)^2)
# model 1(4) - 10208258, model 2(6) - 10106876, model 4(8) -  10203214
# model 2(6) is the most preferred

#10)
p1a <- predict(m1,newdata=subset(software,partition=="test"))
p2a <- predict(m2,newdata=subset(software,partition=="test"))
p4a <- predict(m4,newdata=subset(software,partition=="test"))
sum((p1a-subset(software,partition=="test")$spending)^2)
sum((p2a-subset(software,partition=="test")$spending)^2)
sum((p4a-subset(software,partition=="test")$spending)^2)
# model 1(4) - 6786268, model 2(6) - 6580861, model 4(8) - 6762468
# model 2(6)is the most preferred
# results are consistent

#Q2)
#1)
breast <- read.csv("breastcancer.csv")
summary(breast)
# density has the most number of missing entries - 76

#2)
table(breast$severity)
# balanced dataset

#3)
breast <- na.omit(breast)
library(caTools)
set.seed(1000)
spl <- sample.split(breast$severity,SplitRatio=0.6)
train <- subset(breast,spl==TRUE)
test <- subset(breast,spl==FALSE)
m1 <- glm(severity~age+shape+margin+density,data=train,family="binomial")
summary(m1)
# P(severity=1) = exp(-5.68+0.059*age+0.68*shape+0.42*margin-0.30*density)/(1+exp(-5.68+0.059*age+0.68*shape+0.42*margin-0.30*density))

#4)
# OR = Odds(sev,x+1)/odds(sev,x) = (P(sev=1|x+1)/P(sev=0|x+1))/(P(sev=1|x)/P(sev=0|x))
# = exp(beta_x(x+1)+..)/exp(beta_x(x)+...) = exp(beta_x) 
# exp(0.05989) = 1.06172

#5)
p1 <- predict(m1,newdata=test,type="response")
sum(log(p1[test$severity==1])) + sum(log(1-p1[test$severity==0]))
table(p1 >=0.5,test$severity)
# -156.94
# accuracy = (127+139)/(127+139+22+44) = 0.801

#6)
m2 <- glm(severity~age+shape+margin,data=train,family="binomial")
p2 <- predict(m2,newdata=test,type="response")
sum(log(p2[test$severity==1])) + sum(log(1-p2[test$severity==0]))
table(p2 >=0.5,test$severity)
# -156.06
# (126+141)/(126+141+20+45) = 0.804
# the second model(6) is preferred 

#7)
library(ROCR)
predictROC2 <- prediction(p2,test$severity)
performROC2 <- performance(predictROC2,measure="auc")
performROC2
performROC2a <- performance(predictROC2,measure="auc",fpr.stop=0.75)
performROC2a
performROC2a <- performance(predictROC2,measure="auc",fpr.stop=0.7)
performROC2b
# Partial AUC for FPR >= 0.75 is = 0.8621554-0.6135901 = 0.248
# Partial AUC for FPR >= 0.7 is = 0.8621554-0.5642113 = 0.297

#8)
m3 <- glm(severity~age+shape+margin,data=train,family=binomial(link="probit"))
p3 <- predict(m3,newdata=test,type="response")
sum(log(p3[test$severity==1])) + sum(log(1-p3[test$severity==0]))
table(p3 >=0.5,test$severity)
# -156.23
# (126+140)/(126+140+45+21) = 0.801

#9)
# Logistic is slightly better in accuracy

#10)
table(test$physician>=4,test$severity)
# 157 - on the test set
# 398 - on the entire set

#Q3)
#1)
sum(cumprod(seq(4,100,by=2))/cumprod(seq(5,101,by=2)))

#2)
# max_{\beta_0,\beta} \sum_i y_i(\beta_0+\beta'x_i) - \sum_i \log(1+exp(\beta_0+\beta'x_i))
# derivative with respect to \beta_0
# \sum_i y_i = \sum_i exp(\beta_0+\beta'x_i)/(1+exp(\beta_0+\beta'x_i))
# Divide by n gives
# fraction of 1s in dataset = Average probability of 1 in dataset

#3)
Model 1 - high bias, low variance 
Model 2 - low bias, high variance 

#4)
# 0.2 0.3 0.5
# x_1/x_2 = 0.2/0.3 and x_1+x_2 = 1
# x_1 = 0.4, x_2 = 0.6

# 0.4 0.2 0.4
# x_1/x_2 = 0.4/0.2 and x_1+x_2 = 1
# x_1 = 2/3, x_2 = 1/3

#5)
# false
