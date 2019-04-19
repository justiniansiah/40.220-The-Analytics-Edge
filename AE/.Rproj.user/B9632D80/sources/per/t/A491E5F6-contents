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


df <- read.csv("")

set.seed()
spl <- sample.split(df$y,SplitRatio=0.7)
train <- subset(df,spl == TRUE)
test <- subset(df, spl == FALSE)

glmodel <- glm(data = df, family = "binomial", Y ~. )
pred <- predict(df, newdata = train/test ,type = "response")
table1 <- table(pred>=0.5, train/test$y )
table1
sum(diag(table1)/sum(table1))

###########################################################################################
#email to stefano_galelli@sutd.edu.sg "Yourname.R" nd "Yourname.txt"


###########################################################################################
#Q1
#1.1
pollution <- read.csv("pollution.csv")
str(pollution)
summary(pollution)

which.max(pollution$NOXReal)
pollution[29,]

mean(pollution$MORTReal)

#1.2
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

min(pollution$NOXReal)
max((pollution$NOXReal))


#1.3
6000*5999/2
#1.4
distances = dist(pollution[,1:16], method = 'euclidean')
cluster_dend = hclust(distances, method = "ward.D2")
plot(cluster_dend)
#1.5
clusterGroups = cutree(cluster_dend,k= 3)
plot(clusterGroups)

#1.5
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


pollution[clusterGroups_2==4,]
pollution[clusterGroups_2==5,]
#1.8

pollution <- read.csv("pollution.csv") #they want not normalised

mean(pollution$MORTReal[clusterGroups_2==1])
mean(pollution$MORTReal[clusterGroups_2==2])
mean(pollution$MORTReal[clusterGroups_2==3])
mean(pollution$MORTReal[clusterGroups_2==4])
mean(pollution$MORTReal[clusterGroups_2==5])

#1.9

mean(pollution$NOXReal[clusterGroups_2==1])
mean(pollution$NOXReal[clusterGroups_2==2])
mean(pollution$NOXReal[clusterGroups_2==3])
mean(pollution$NOXReal[clusterGroups_2==4])
mean(pollution$NOXReal[clusterGroups_2==5])

mean(pollution$HCReal[clusterGroups_2==1])
mean(pollution$HCReal[clusterGroups_2==2])
mean(pollution$HCReal[clusterGroups_2==3])
mean(pollution$HCReal[clusterGroups_2==4])
mean(pollution$HCReal[clusterGroups_2==5])

max(pollution$MORTReal[clusterGroups_2==1])
max(pollution$MORTReal[clusterGroups_2==2])
max(pollution$MORTReal[clusterGroups_2==3])
max(pollution$MORTReal[clusterGroups_2==4])
max(pollution$MORTReal[clusterGroups_2==5])



###########################################################################################
#q2
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

#2.3
predict_train <- predict(model, newdata=train, type="class")
t1 <- table(predict_train,train$V35)
sum(diag(t1)/sum(t1))

predict_test <- predict(model, newdata=test, type="class")
t2 <- table(predict_test,test$V35)
sum(diag(t2)/sum(t2))

#2.4

X <- ionosphere[,1:34]

s <- svd(X)

var <- cumsum(s$d^2)
str(var)
var/max(var)

#2.5
plot(1:34,var/max(var))
abline(h=0.9)

#2.6
df <- as.data.frame(s$u[,1:21])
df$output <- ionosphere$V35
train2 <- subset(df,spl == TRUE)
test2 <- subset(df, spl == FALSE)

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







#Q3
#3.1
SalesConversion <- read.csv("SalesConversion.csv")
str(SalesConversion)
table(SalesConversion$Approved_Conversion)
for (i in 1:nrow(SalesConversion)){
  k= SalesConversion$Approved_Conversion[i]
  if (k > 5){
    SalesConversion$Approved_Conversion[i] = 5
  }
}
table(SalesConversion$Approved_Conversion)

#3.2
set.seed(1)
spl <- sample.split(SalesConversion$Approved_Conversion,SplitRatio=0.7)
train <- subset(SalesConversion,spl == TRUE)
test <- subset(SalesConversion, spl == FALSE)

table(train$Approved_Conversion)
table(test$Approved_Conversion)

#.3.3,3.4

cart1 <- rpart(data=train, Approved_Conversion~age+gender+interest+Impressions+Clicks+Spent+Total_Conversion, method = "class")
prp(cart1)

#3.5
print(cart1)

#3.6 3.7 3.8
pred_train <- predict(cart1, newdata = train, type = "class")
pred_test <- predict(cart1, newdata = test, type = "class")

t <- table(pred_train, train$Approved_Conversion) 
t
sum(diag(t)/sum(t))
t <- table(pred_test, test$Approved_Conversion) 
t
sum(diag(t)/sum(t))

#3.9.
set.seed(10)
forest <- randomForest(data=train, Approved_Conversion~age+gender+interest+Impressions+Clicks+Spent+Total_Conversion)
predF <- predict(forest, newdata = test)
t <-table(round(predF),test$Approved_Conversion)
t
sum(diag(t)/sum(t))

#3.10
varImpPlot(forest)


















































