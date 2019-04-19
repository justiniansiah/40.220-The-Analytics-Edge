####################
# GENERAL FUNCTIONS
####################
wine<-read.csv("Dataset/wine.csv")

str(wine)

summary(wine)

m <- matrix(c(3,4,5,6,7,8), nrow=3, ncol=2)

n <- array(c(3,4,5,6,7,8), c(3,2,1)) 

t <- data.frame(names=c("karthik","sam","jim"),
                ages = c(36,34,40),
                sex = c("M","F","M"),
                children=c(2,0,1))

t$names

# rounded down to create a few categories
table(floor(wine$DEGREES))

DegreesDF<-as.data.frame(table(wine$DEGREES_ROUNDED))

dim(wine) # applies for dataframe/matrix
length(wine$VINT) # applies for a particular column/array

wine$LPRICEx2 <- wine$LPRICE * 2

is.na(wine$LPRICE)

wine_narm<-wine[!is.na(wine$LPRICE),]

wine_narm2<-na.omit(wine)

mean(wine$LPRICE) 
mean(wine$LPRICE, na.rm=TRUE) 

# using row index
wine[2,] # to choose row 2
wine[-2,] # to choose every row except row 2
# using a condition
wine[wine$DEGREES>16,]
subset(wine, wine$DEGREES>16)
wine[,c("WRAIN", "DEGREES")]

subset(wine, select=c(WRAIN, DEGREES))

subset(wine, select=-c(WRAIN,DEGREES))

x<-c(1,2,3,6,3,6)
which(x==max(x))

which(is.na(wine$LPRICE))
wine[which(is.na(wine$LPRICE)),]

max(wine$HRAIN)
which(wine$HRAIN==max(wine$HRAIN)) # alternatively, look at which.max()
wine[17,]

which.max(wine$HRAIN)

as.logical(1)
as.numeric(TRUE)

wrain_factor<-as.factor(wine$WRAIN)
#wrain_factor

# Wrong
#as.numeric(wrain_factor)

# Correct
as.numeric(as.character(wrain_factor))

# remove na data for tapply
wine_tapply<-na.omit(wine)
# floor it to create a few categories
tapply(wine_tapply$LPRICE, floor(wine_tapply$DEGREES), mean)

table(tapply(wine_tapply$LPRICE, floor(wine_tapply$DEGREES), mean))
# not meaningful due to the data..

addOne<-function(x){
    return (x+1)
}

myarray<-c(1,2,3)
sapply(myarray, addOne)

#?apply
#apply(wine[,c('WRAIN')],2,mean) # x must be an array
apply(wine[,c('WRAIN','DEGREES')],2,mean)

library(caTools)

# remove na data
wine_narm<-na.omit(wine)
# create categories for LPRICE
wine_narm$LPRICE_CAT <- wine_narm$LPRICE>-1.0
table(wine_narm$LPRICE_CAT)

spl <- sample.split(wine_narm, 0.75)
train <- subset(wine_narm, spl==TRUE)
test <- subset(wine_narm, spl==FALSE)
table(train$LPRICE_CAT)

table(test$LPRICE_CAT)

trainID<-sample(1:nrow(wine_narm),nrow(wine_narm)/2) # sample(start_index:end_index, number_of_rows_to_split)
train<-wine_narm[trainID,]
test<-wine_narm[-trainID,]
str(train)

str(test)

t.test(wine$DEGREES)

t.test(subset(wine,wine$VINT<1973)$DEGREES, subset(wine,wine$VINT>=1973)$DEGREES)

min(wine$DEGREES)
max(wine$DEGREES)

wine$WRAIN
sort(wine$WRAIN)

cor(wine$WRAIN, wine$LPRICE, use="pairwise.complete.obs")

df <- data.frame(days=c("monday","tuesday","wednesday","friday","monday"),
                customers = c(36,34,40,100,2))
str(df)
df

tapply(df$customers, df$days, sum)

df$days<-factor(df$days, ordered=TRUE, levels=c("monday", "tuesday", "wednesday", "thursday", "friday"))
df

tapply(df$customers, df$days, sum)

names(wine)

####################
# PLOTTING
####################
data(faithful)
str(faithful)

plot(faithful)

plot(faithful, type="l")

hist(faithful$eruptions, breaks=seq(1.6,5.2,0.2))

plot.ecdf(faithful$eruptions)

qqnorm(faithful$eruptions)

####################
# LINEAR REGRESSION
####################

library(caTools)
set.seed(1)
spl<-sample.split(wine, SplitRatio = 0.75)
train<-subset(wine, spl==TRUE)
test<-subset(wine, spl==FALSE)

lm_model <- lm(LPRICE~., data=train)
summary(lm_model)

lm_model2 <- lm(LPRICE~VINT+WRAIN, data=train) # train with VINT and WRAIN as predictor
lm_model3 <- lm(LPRICE~.-VINT, data=train) # remove one predictor using '-' before the predictor
lm_model4 <- lm(LPRICE~.-1, data=train) # remove the intercept using '-1' 

predict(lm_model, newdata=test)
# ignore the warning message. it is due to empty beta coefficient for TIME_SV

lm_model5 <- lm(LPRICE~DEGREES, data=train)
plot(train$DEGREES, train$LPRICE)
abline(lm_model5)

# Method 1 (only on train data since it can only be obtained from the model)
sse<-sum(lm_model$residuals^2)

# Method 2 (for both train and test data)
sse<-sum((train$LPRICE-predict(lm_model, newdata=train))^2, na.rm = TRUE)
sse
# again, ignore the warning message

sst<-sum((train$LPRICE-mean(train$LPRICE, na.rm=TRUE))^2, na.rm = TRUE)
sst

1-sse/sst

####################
# LOGISTIC REGRESSION
####################

orings<-read.csv("Dataset/Orings.csv")

str(orings)

summary(orings)

glm_model<-glm(Field~Temp+Pres,data=orings,family=binomial)
summary(glm_model)

predict(glm_model, newdata=orings)

glm_predict<-predict(glm_model, newdata=orings,type="response") # predict probabilities and store in variable
glm_predict

table(glm_predict[1:138]>0.1, orings$Field[1:138])

table(orings$Field)

library(ROCR)
ROCRpred <- prediction(glm_predict[1:138], orings$Field[1:138]) # prediction(predicted_values, actual_values)

ROCRperf <- performance(ROCRpred, measure='tpr', x.measure='fpr')
plot(ROCRperf)

performance(ROCRpred, measure='auc')

####################
# SUBSET SELECTION
####################

hitters<-read.csv("Dataset/Hitters.csv")
hitters<-na.omit(hitters) # to remove na values

str(hitters)

summary(hitters)

library(leaps)

hitters<-hitters[,2:21] # to remove first column of names
regsubset_model1<-regsubsets(Salary~., data=hitters)
regsubset_model2<-regsubsets(Salary~., data=hitters, nvmax=19)
regsubset_model3<-regsubsets(Salary~., data=hitters, nvmax=19, method='forward')

summary(regsubset_model1)

summary(regsubset_model2)

summary(regsubset_model2)$rsq
summary(regsubset_model2)$rss
summary(regsubset_model2)$adjr2

plot(summary(regsubset_model2)$adjr2)

which.max(summary(regsubset_model2)$adjr2)
coef(regsubset_model2, 11)

####################
# LASSO
####################

library(glmnet)

x<-model.matrix(Salary~., data=hitters)
y<-hitters$Salary

set.seed(1) # always set seed the line before
train<-sample(1:nrow(x),nrow(x)/2) # if unclear about sample(), refer to above
test<--train

# generate a range of lambda values for LASSO model
grid<-10^seq(10,-2,length=10)
lasso_model<-glmnet(x[train,],y[train],lambda=grid)

lasso_model # check names(lasso_model) if unsure
lasso_model$df
lasso_model$beta #or coef(lasso_model) to see the intercept beta0
coef(lasso_model, s = 1.000e+02)

plot(lasso_model, xvar = 'lambda')

grid

predict(lasso_model, newx=x[test,])

predict(lasso_model, newx=x[test,], s=100)

predict(lasso_model, newx=x[test,], s=1.25e+02)

predict(lasso_model, newx=x[test,], s=1.25e+02, exact=T, x=x[train,], y=y[train])

predicted <- predict(lasso_model, newx=x[test,], s=100)
mean((predicted-y[test])^2)

set.seed(1)
cvlasso_model<-cv.glmnet(x[train,],y[train])

cvlasso_model$lambda.min
cvlasso_model$lambda
cvlasso_model$cvm
cvlasso_model$nzero

predict(lasso_model, newx=x[test,], s=16.7801585216616, exact=T, x=x[train,], y=y[train])

coef(lasso_model, s = 16.7801585216616)


