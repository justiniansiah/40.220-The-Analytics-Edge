setwd("D:/School Stuff/40 .220 The Analytics Edge/Finals")
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(randomForest)
library(tm)
library(SnowballC)
library(e1071)
library(jpeg)
library(survival)
library(caret)  #normalise data
library(flexclust) #see week 10 stuff
###########################################################################################
#Q1
pollution <- read.csv("pollution.csv")
str(pollution)
summary(pollution)
which.max(pollution$NOXReal)
pollution[,29]
pollution[29,]
colMeans(pollution$MORTReal)
meaneans(pollution$MORTReal)
means(pollution$MORTReal)
mean(pollution$MORTReal)
pollution$PRECReal <- scale(pollution$PRECReal)
pollution$JANTReal <- scale(pollution$JANTReal)
pollution$JULTReal <- scale(pollution$JULTReal)
pollution$OVR65Real <- scale(pollution$OVR65Real)
pollution$POPNReal <- scale(pollution$POPNReal)
pollution$EDUCReal <- scale(pollution$EDUCReal)
pollution$HOUSReal <- scale(pollution$HOUSReal)
pollution$DENSReal <- scale(pollution$DENSReal)
pollution$NONWReal <- scale(pollution$NONWReal)
pollution$WWDRKReal <- scale(pollution$WWDRKReal)
pollution$POORReal <- scale(pollution$POORReal)
pollution$HCReal <- scale(pollution$HCReal)
pollution$NOXReal <- scale(pollution$NOXReal)
pollution$SOReal <- scale(pollution$SOReal)
pollution$HUMIDReal <- scale(pollution$HUMIDReal)
pollution$MORTReal <- scale(pollution$MORTReal)
summary(pollution)
min(pollution$NOXReal)
max((pollution$NOXReal))
?cumsum
cumsum(10)
cumsum(c(1,2,3))
cumsum(c(1,2,3,4))
cumsum(c(0,1,2,6))
#1.3
distances = dist(pollution[,1:60], method = 'euclidean')
#1.3
distances = dist(pollution[,1:16], method = 'euclidean')
distances
8569*8568/2
6000*5999/2
cluster_dend = hclust(distances, method = "ward.D2")
plot(cluster_dend)
clusterGroups = cutree(cluster_dend,k= 3)
plot(clusterGroups)
clusterGroups
pollution$MORTReal[clusterGroups==1]
mean(pollution$MORTReal[clusterGroups==1])
mean(pollution$MORTReal[clusterGroups==2])
mean(pollution$MORTReal[clusterGroups==3])
#1.5
c1 <- mean(pollution$MORTReal[clusterGroups==1])
pollution$MORTReal[clusterGroups==1]
sqrt( ((pollution$MORTReal[clusterGroups==1])^2)+(c1^2) )
e1 <- sqrt( ((pollution$MORTReal[clusterGroups==1])^2)+(c1^2) )
max(e1)
e2 <- sqrt( ((pollution$MORTReal[clusterGroups==2])^2)+(c2^2) )
max(e2)
e3 <- sqrt( ((pollution$MORTReal[clusterGroups==3])^2)+(c3^2) )
max(e3)
c1 <- mean(pollution$MORTReal[clusterGroups==1])
c2 <-mean(pollution$MORTReal[clusterGroups==2])
c3 <-mean(pollution$MORTReal[clusterGroups==3])
#1.6
e1 <- sqrt( ((pollution$MORTReal[clusterGroups==1])^2)+(c1^2) )
max(e1)
e2 <- sqrt( ((pollution$MORTReal[clusterGroups==2])^2)+(c2^2) )
max(e2)
e3 <- sqrt( ((pollution$MORTReal[clusterGroups==3])^2)+(c3^2) )
max(e3)
#1.7
clusterGroups_2 = cutree(cluster_dend,k= 5)
plot(clusterGroups_2)
pollution$MORTReal[clusterGroups==4]
pollution$MORTReal[clusterGroups_2==4]
pollution[clusterGroups_2==4]
pollution[clusterGroups_2==4,]
pollution[29]
pollution[29,]
pollution[clusterGroups_2==5,]
pollution <- read.csv("pollution.csv") #they want not normalised
mean(pollution$MORTReal[clusterGroups_2==1])
mean(pollution$MORTReal[clusterGroups_2==2])
mean(pollution$MORTReal[clusterGroups_2==3])
mean(pollution$MORTReal[clusterGroups_2==4])
mean(pollution$MORTReal[clusterGroups_2==5])
mean(pollution$NOXReal[clusterGroups_2==1])
mean(pollution$NOXReal[clusterGroups_2==2])
mean(pollution$NOXReal[clusterGroups_2==3])
mean(pollution$NOXReal[clusterGroups_2==4])
mean(pollution$NOXReal[clusterGroups_2==5])
mean(pollution$HCReal[clusterGroups_2==1])
mean(pollution$HCReal[clusterGroups_2==2])
mean(pollution$HCReal[clusterGroups_2==3])
mean(pollution$HCReal[clusterGroups_2==4])
mean(pollution$HCReal[
clusterGroups_2==5])
max(pollution$MORTReal[clusterGroups_2==1])
max(pollution$MORTReal[clusterGroups_2==2])
max(pollution$MORTReal[clusterGroups_2==3])
max(pollution$MORTReal[clusterGroups_2==4])
max(pollution$MORTReal[clusterGroups_2==5])
ionosphere <- read.csv("ionosphere.csv")
str(ionosphere)
set.seed(1)
spl <- sample.split(ionosphere$V35,SplitRatio=0.7)
train <- subset(ionosphere,spl == TRUE)
test <- subset(ionosphere, spl == FALSE)
#2.1
table(train$V35)
table(test$V35)
#2.2
model <- naiveBayes(V35~. , data = train)
summary(model)
model$apriori
model$tables  # List tables for each predictor. For each numeric variable, it gives target class, mean and standard deviation.
predict_train <- predict(model, newdata=train, type="class")
table(predict_train,train$V35)
predict_test <- predict(model, newdata=test, type="class")
table(predict_test,test$V35)
predict_train <- predict(model, newdata=train, type="class")
t1 <- table(predict_train,train$V35)
sum(diag(t1)/sum(t1))
predict_test <- predict(model, newdata=test, type="class")
t2 <- table(predict_test,test$V35)
sum(diag(t2)/sum(t2))
model$apriori
#2.1
table(train$V35)
table(test$V35)
ionosphere[,1]
ionosphere[1,]
X <- ionosphere[,1:34]
str(X)
s <- svd(X)
var(X)
s <- svd(X)
for (k in 1:34){
x_hat <- s$u[,1:k] %*% diag(s$d[1:k]) %*% t(s$v[,1:k])
if(x-x_hat > 0.1*x){
return (k)
}
k=1
s$u[,1:k] %*% diag(s$d[1:k]) %*% t(s$v[,1:k])
k=2
s$u[,1:k] %*% diag(s$d[1:k]) %*% t(s$v[,1:k])
for (k in 2:34){
x_hat <- s$u[,1:k] %*% diag(s$d[1:k]) %*% t(s$v[,1:k])
if(x-x_hat > 0.1*x){
return (k)
}
for (k in 2:34){
x_hat <- s$u[,1:k] %*% diag(s$d[1:k]) %*% t(s$v[,1:k])
if(X-x_hat > 0.1*X){
return (k)
}
if((X-x_hat) > 0.1*X){
return (k)
}
for (k in 2:34){
x_hat <- s$u[,1:k] %*% diag(s$d[1:k]) %*% t(s$v[,1:k])
if((X-x_hat) > 0.1*X){
return (k)
}
X
x_hat
for (k in 2:34){
x_hat <- s$u[,1:k] %*% diag(s$d[1:k]) %*% t(s$v[,1:k])
flag = TRUE
for (i in 1:322){
if((X[i]-x_hat[i]) < 0.1*X[i]){
flag = FALSE
}
if (flag == TRUE){
return(k)
}
#Q3
SalesConversion <- read.csv("SalesConversion.csv")
SalesConversion$Approved_Conversion
str(SalesConversion)
for (i in 1:nrow(SalesConversion)){
k= SalesConversion$Approved_Conversion[i]
if (k > 5){
SalesConversion$Approved_Conversion[i] = 5
}
SalesConversion$Approved_Conversion
SalesConversion <- read.csv("SalesConversion.csv")
str(SalesConversion)
table(SalesConversion$Approved_Conversion)
for (i in 1:nrow(SalesConversion)){
k= SalesConversion$Approved_Conversion[i]
if (k > 5){
SalesConversion$Approved_Conversion[i] = 5
}
table(SalesConversion$Approved_Conversion)
7+7+4+5+3+4+1+1+2+1+1
set.seed(1)
spl <- sample.split(SalesConversion$Approved_Conversion,SplitRatio=0.7)
train <- subset(SalesConversion,spl == TRUE)
test <- subset(SalesConversion, spl == FALSE)
table(train$Approved_Conversion)
table(test$Approved_Conversion)
str(train)
cart1 <- rpart(data=train, Approved_Conversion~age+gender+interest+Impressions+Clicks+Spent+Total_Conversion, method = "class")
prp(cart1)
prp(cart1,type=2)
prp(cart1,extra=2)
print(cart1)
pred_train <- predict(cart1, newdata = train, type = "class")
pred_test <- predict(cart1, newdata = test, type = "class")
pred_train
t <- table(pred_train, train$Approved_Conversion)
t
sum(diag(t)/sum(t))
t <- table(pred_test, test$Approved_Conversion)
t
sum(diag(t)/sum(t))
#3.2
set.seed(1)
spl <- sample.split(SalesConversion$Approved_Conversion,SplitRatio=0.7)
train <- subset(SalesConversion,spl == TRUE)
test <- subset(SalesConversion, spl == FALSE)
cart1 <- rpart(data=train, Approved_Conversion~age+gender+interest+Impressions+Clicks+Spent+Total_Conversion, method = "class")
#3.6
pred_train <- predict(cart1, newdata = train, type = "class")
t <- table(pred_train, train$Approved_Conversion)
t
sum(diag(t)/sum(t))
printcp(cart1)
forest <- randomForest(data=train, Approved_Conversion~age+gender+interest+Impressions+Clicks+Spent+Total_Conversion)
predF <- predict(forest, newdata = test)
predF
table(predF,test$Approved_Conversion)
?randomForest
table(predF>0.5,test$Approved_Conversion)
pred_test
forest <- randomForest(data=train, Approved_Conversion~age+gender+interest+Impressions+Clicks+Spent+Total_Conversion, method="class")
predF <- predict(forest, newdata = test)
table(predF>,test$Approved_Conversion)
table(predF,test$Approved_Conversion)
set.seed(10)
forest <- randomForest(data=train, Approved_Conversion~age+gender+interest+Impressions+Clicks+Spent+Total_Conversion)
predF <- predict(forest, newdata = test)
table(round(predF),test$Approved_Conversion)
t <-table(round(predF),test$Approved_Conversion)
t
sum(diag(t)/sum(t))
varImpPlot(forest)
###########################################################################################
#q2
ionosphere <- read.csv("ionosphere.csv")
set.seed(1)
spl <- sample.split(ionosphere$V35,SplitRatio=0.7)
train <- subset(ionosphere,spl == TRUE)
test <- subset(ionosphere, spl == FALSE)
X <- ionosphere[,1:34]
X
s
s <- svd(X)
s
var <- cumsum(s$d^2)
str(var)
plot(1:34,var/max(var))
abline(h=0.9)
str(var)
var
0.9*var/max(var)
var/max(var)
s$u
#2.6
df <- s$u[,1:21]
df$output <- ionosphere$V35
df
#2.6
df <- s$u[,1:21]
str(df)
#2.6
df <- s$u[1:21]
str(df)
#2.6
df <- s$u[1:21]
str(df)
s$u
#2.6
df <- as.dataframe(s$u[1:21])
#2.6
df <- as.data.frame(s$u[1:21])
str(df)
#2.6
df <- as.data.frame(s$u[,1:21])
str(df)
df$output <- ionosphere$V35
str(df)
train2 <- subset(df,spl == TRUE)
test2 <- subset(df, spl == FALSE)
#2.7
model2 <- naiveBayes(V35~. , data = train2)
summary(model2)
model2$apriori
model2$tables
#2.7
model2 <- naiveBayes(output~. , data = train2)
summary(model2)
model2$apriori
model2$tables
predict_train <- predict(model2, newdata=train2, type="class")
t1 <- table(predict_train,train2$output)
sum(diag(t1)/sum(t1))
predict_test <- predict(model2, newdata=test2, type="class")
t2 <- table(predict_test,test2$output)
sum(diag(t2)/sum(t2))
savehistory("D:/School Stuff/40 .220 The Analytics Edge/Finals/JustinianSiah.txt")
savehistory("D:/School Stuff/40 .220 The Analytics Edge/Finals/JustinianSiah.R")
