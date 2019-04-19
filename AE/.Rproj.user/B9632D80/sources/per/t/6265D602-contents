
# Script for ...


######## Set the working environemnt

# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# Set the working folder
# getwd()
setwd("D:/School Stuff/40 .220 The Analytics Edge/W8/1) Supreme Court")


######## Data Preparation

# Load the data on the supreme court
supreme <- read.csv("supreme.csv") # We have 623 observations and 20 variables
head(supreme) # First part of the dataframe
# str(supreme) # Internal structure of the dataframe 
# summary(supreme) # Summary of the data

# A few comments on the data:
# docket: case number
# term: year the case was discussed
# party_1 and party_2: parties involved in the case
# rehndir stevdir ocondir scaldir kendir soutdir thomdir gindir brydir: direction of the judgement of the 9 judges
# (i.e., Rehquist, Stevens, O'Connor, Scalia, Kennedy, Souter, Thomor, Ginsburg, Breyen). 0 means liberal, 1 conservative. 
# 9 indicates that the vote is not available for the case.
# petit: petitioner type
# respon: respondant type
# circuit: circuit of origin
# uncost: binary number indicating if the petitioner argued constitutionality of a law or practice
# lctdir: lower court direction of results (liberal or conservative)
# issue: area of issue
# result: 0 means liberal, 1 conservative.

# Let's now focus on a specific judge, say Stevens (we remove data with no entry from Stevens--Stevens was present)
stevens <- subset(supreme[,c("docket","term","stevdir","petit","respon","circuit","unconst","lctdir","issue","result")],supreme$stevdir!=9)

# Processing the output result to affirm or reverse for judge Stevens
stevens$rev <- as.integer((stevens$lctdir=="conser" & stevens$stevdir==0) | (stevens$lctdir=="liberal" & stevens$stevdir==1))
# This creates a new variable ($rev) in the dataframe that takes a value of 1 if Stevens decision reverses the lower court decision
# and 0 if the judge affirms the lower court decision. Note that a similar analysis can be done for the other judges or for the overall case result.
table(stevens$rev)


######## Modelling exercise: logistic regression

# Let's prepare the data for our modelling exercise
# Load the caTools package (basic utility functions)
if(!require(caTools)){
  install.packages("caTools")
  library(caTools)
}
# Set the seed
set.seed(1)
# Create train and test sets (with balanced response from the judge Stevens)
spl <- sample.split(stevens$rev,SplitRatio=0.7) # We use 70% of the data for training
train <- subset(stevens,spl==TRUE); # table(train$rev)
test <- subset(stevens,spl==FALSE); # table(test$rev)
# There is only one realization of the IR value (Interstate Relations), which does not
# appear in the training dataset. Let's thus modify the test dataset as follows:
test <- subset(test,test$issue!="IR")

# Logistic regression
# We predict judge Stevens' decision 
m1 <- glm(rev~petit+respon+circuit+unconst+issue+lctdir,data=train,family="binomial")
summary(m1)
# let's now try to make a prediction on the test data
p1 <- predict(m1,newdata=test,type="response")

# How does the model perform?
table(p1>=0.5,test$rev)
# Confusion matrix
#        0  1
# FALSE 53 32
# TRUE  29 70
# The accuracy is about 0.66 

# Area Under the Curve
if(!require(ROCR)){
  install.packages("ROCR")
  library(ROCR)
}
pred <- prediction(p1,test$rev) # create the prediction object
perf <- performance(pred, x.measure="fpr",measure="tpr") # calculate False and True Positive Rate (FPR and TPR)
plot(perf) # FPR vs TPR
performance(pred,measure="auc") # 0.7404352


######## Classification And Regression Trees (CARTs)

# rpart is for CART
if(!require(rpart)){
  install.packages("rpart")
  library(rpart)
}
# rpart.plot extend some functionalities of rpart
if(!require(rpart.plot)){
  install.packages("rpart.plot")
  library(rpart.plot)
}
# rattle gives additional functionalities
if(!require(rattle)){
  install.packages("rattle")
  library(rattle)
}
# colors
if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

# Learning the tree (when building the tree for classification, rpart uses the Gini index)
#
# This will not work, because rpart will build a regression tree by default
# cart1 <- rpart(rev~petit+respon+circuit+lctdir+issue+unconst,data=train)
#
# Correct command
cart1 <- rpart(rev~petit+respon+circuit+lctdir+issue+unconst,data=train,method="class")
# Alternative way of building a classification tree
cart1 <- rpart(as.factor(rev)~petit+respon+circuit+lctdir+issue+unconst,data=train)

# Tree visualization
# 
# Let's first use the default function implemented in the rpart package
plot(cart1); text(cart1)
#
# Let's use a more advanced function from the rpart.plot package
prp(cart1) 
prp(cart1,type=1) # labels all nodes (not just the leaves)
prp(cart1,type=4) # draws separate labels for leaf and right directions for all nodes and label nodes
prp(cart1,type=4,extra=4) # in addition, this also plots the probability per class of observation
# prp(cart1,type=4,extra=9) # probablities times fraction of observations at the node (the sum across all leaves is 1)
#
# There many other visualization options: see an example with the fancyRpartPlot() function, which requires rpart.plot, rattle, and RColorBrewer.
fancyRpartPlot(cart1)

# More details on the tree structure
#
# With this command, we get information on: split criteria, no. rows (observations) in this node, no. of misclassified, predicted class,
# and % of rows in predicted class for this node.
# node), split, n, loss, yval, (yprob)
#      * denotes terminal node
print(cart1)
# Note that the children of node x are numbered 2x and 2x+1. Also, note that a variable might appear many times in the tree.
# Example: 38) issue=CP,FA 16   4 0 (0.75000000 0.25000000) * (we also see that this is a terminal node)
#
# An equivalent command is just
cart1

# Predictions
#
# The function predict uses cart1 and the data test to carry out the predictions.
# The function chooses the class with the highest probability.
predictcart1 <- predict(cart1,newdata=test,type="class")
predictcart1
#
# Confusion matrix
table(test$rev,predictcart1)
# 0  1
# 0 54 28
# 1 33 69
# The accuracy is 0.6684783
# We can see that this model has performance similar to that achieved by the logistic regression
#
# What would be the accuracy of a baseline model?
table(train$rev)
# 0   1 
# 195 239 --> 0.55
table(test$rev)
# 0   1 
# 82 102 
#
# We can also use the Area Under the Curve
predictcart1_prob <- predict(cart1,newdata=test) # predicted probabilities of rev=0 and rev=1 for each test observation
pred_cart1 <- prediction(predictcart1_prob[,2],test$rev)
perf_cart1 <- performance(pred_cart1, x.measure="fpr",measure="tpr")
plot(perf_cart1)
performance(pred_cart1,measure="auc") # 0.7119799
# In synthesis, we can see that the performance of the logistic regression and CART model are similar.


######## Pruning

# Display the cp table for model cart1
printcp(cart1)
# Variables actually used in tree construction:
#   [1] circuit issue   lctdir  petit   respon 
# 
# Root node error: 195/434 = 0.44931
# 
# n = 434 
# 
#       CP    nsplit rel error  xerror  xstd
# 1 0.210256      0   1.00000 1.00000 0.053142
# 2 0.035897      1   0.78974 0.78974 0.051116
# 3 0.023077      3   0.71795 0.89744 0.052407
# 4 0.018803      5   0.67179 0.88718 0.052307
# 5 0.012821      9   0.57949 0.89744 0.052407
# 6 0.010256     11   0.55385 0.88718 0.052307
# 7 0.010000     17   0.49231 0.90769 0.052502
#
# Here we see from the smallest tree (no splits) to the largest one (17 splits). The errors are scaled so that
# the first node has error = 1.The CP parameter is also scaled: the default stopping value used in rpart is 0.01.
# Roughly, the relative error can be converted to absolute error by multiplying by the root node error. 
# xerror and xstd refer to the output from the cross-validation. You would like
# to pick the tree with lowest xerror+xstd (1 standard deviation).
#
#
# We can also plot the relation between cp and error
plotcp(cart1)
#
# Equivalently, we can use the function summary(), which contains all info about the tree
summary(cart1)
# cart1 contains all the information about the tree, with nodes, branches and the corresponding probability associated to each class.
#
# We are now ready for pruning
#
# Let's look for the value of cp yielding the smallest error
opt <- which.min(cart1$cptable[,"xerror"])
#get its value
cp <- cart1$cptable[opt, "CP"] # 0.03589744
#
# Pruning
cart2 <- prune(cart1,cp=0.035897)
fancyRpartPlot(cart2)
predictcart2 <- predict(cart2,newdata=test,type="class")
table(test$rev,predictcart2)
# 0  1
# 0 63 19
# 1 34 68
# The accuracy is 0.7119565

# Other options to control the tree size (prepruning)
cart3 <- rpart(rev~petit+respon+circuit+lctdir+issue+unconst,data=train,method="class",
                       maxdepth= 30, 
                       minbucket = 20)
fancyRpartPlot(cart3)
# minbucket provides the smallest number of observations that are allowed in a terminal node. 
# the maxdepth parameter prevents the tree from growing past a certain depth / height. The default is 30.


######## Random Forests

# Load the randomForest package (it can be used for both classification and regression)
if(!require(randomForest)){
  install.packages("randomForest")
  library(randomForest)
}
# set seed
set.seed(1)
#
# Build a forest of 200 trees, with leaves 5 observations in the terminal nodes
forest <- randomForest(as.factor(rev)~petit+respon+circuit+unconst+lctdir+issue,data=train,nodesize=5,ntree=250)
forest
# The prediction is carried out through majority vote (across all trees)
predictforest <- predict(forest,newdata=test,type="class")
table(test$rev,predictforest)
# 0  1
# 0 48 34
# 1 23 79
# The accuracy is 0.6902174

# Variable importance
varImpPlot(forest)
# forest$importance

