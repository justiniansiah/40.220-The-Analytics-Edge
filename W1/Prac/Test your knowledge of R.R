rm(list=ls())

#1
x <- c(4,2,6)
y <- c(1,0,-1)

length(x)
sum(x)
sum(x^2)
x+y
x*y
x-2
x^2
x*y[1:2]

#2
7:11
seq(2,9)
seq(4,10,by=2)
seq(3,30,length=10)
seq(6,-4,by=-2)

#3 Repetitions
rep(2,4)
rep(c(1,5),4)
rep(c(1,2),c(4,4))

#4 
x <- c(5,9,2,3,4,6,7,0,8,12,2,9)
x[2]
x[2:4]
x[c(2,3,6)]
x[c(1:5,10:12)]
x[-(10:12)]

#4.5
x <- as.numeric(-4:4)
x <- c(x,4)
income <- c(100,200,200,300,400,0,50,70,90,100)
tapply(income,x,mean)
#tapply applies a function (mean) to the vector (income) based on the index (x)

#5 Matrices
x = matrix(c(3,-1,2,-1),2,2)
y = matrix(c(1,0,4,1,0,-1),2.3)

2*x
x*x
x%*%x
x%*%y
t(y)
solve(x)

x[1,]
x[2,]
x[,2]
y[1,2]
y[,2:3]

z <- 1:30
#???

diag(10)

cbind(c(1,2,1),c(3,4,5)) #column bind
rbind(c(1,2,1),c(3,4,5)) #row bind

r <- matrix(c(3,4,5,6,7,8),3,2)
m <- matrix(c(2,3,4,5,6,7),nrow=3,ncol=2)

r*m

r%*%m #matrix multiplication
r%*%t(m) #transpose m first

a <- matrix(c(2,1,-1,2),c(2,2)) #2nd vector denotes r,c
b <- c(4,4)
solve(a,b) #solves Ax=b

e <- eigen(a) #returns eigenvalues and eigenvector of matrix a
e$values #returns only the values

#5.5 Lists
aaa <- list(first=1,alpha="aaa")
aaa
aaa$first

A <- data.frame(names=c("fish","cake"),price=c(10,40))
A

#5.6 Dataframes
data(faithful) #loads old faithful dataset from default sample data 
df <- faithful
str(df) #compactly display the structure 
plot(df)
hist(df$eruptions)
min(df$eruptions)
hist(df$eruptions,seq(1.5,5.2,0.2))
plot(df$eruptions)
plot(df$eruptions,type="l")

plot.ecdf(df$eruptions) #empirical cdf
qqnorm(df$eruptions) #qq plot

qqnorm(df$eruptions[df$eruptions>3])

boxplot(df$eruptions)

#statistics
faithful1 <- subset(df,df$eruptions<=3)
faithful2 <- subset(df,df$eruptions>3)

str(faithful1)
str(faithful2)

mean(faithful1$eruptions)
mean(faithful2$eruptions)
mean(faithful1$waiting)
mean(faithful2$waiting)

#H0: u0 = 0
#H1: u0 =/= 0
#calc t-stat: (xbar - u0) / (s/root(n))

t.test(faithful1$waiting) #default u = 0

#p-value is low so null is rejected

#reading csv
df <- read.csv("../W1/WHO.csv")
head(df)
str(df)
which(df$Country=="Singapore")
df[155,]
summary(df$Under15)
df[which(df$Over60==max(df$Over60)),]

# #6 Internet privacy has gained widespread attention in recent years. To measure the degree
# #to which people are concerned about hot-button issues like Internet privacy, social scientists
# conduct polls in which they interview a large number of people about the topic. In this
# question, we will analyze data from a July 2013 Pew Internet and American Life Project poll
# on Internet anonymity and privacy, which involved interviews across the United States. 

#(a) How many people participated in the poll?
poll <- read.csv("../W1/Prac/AnonymityPoll.csv")
summary(poll)
str(poll)
#1002 obs so 1002 participants

#(b)
#bi) How many interviewees responded that they use a smartphone?
table(poll$Smartphone)
#487 uses smartphone

#bii)How many interviewees responded that they don't use a smartphone?
#472 do not use smartphone

#biii)How many interviewees did not respond to the question, resulting in a missing value, or NA, in the summary() output?
#43 NAs

#(c) Look at the breakdown of the number of people with smartphones and Internet use using the table() command.
table(poll$Internet.Use)

#ci) How many interviewees reported not having used the Internet and not having used a smartphone?
table(poll$Smartphone[poll$Internet.Use==0])
#186 no internet, no smartphone

#cii)How many interviewees reported having used the Internet and having used a smartphone?
table(poll$Smartphone[poll$Internet.Use==1])
#470 have internet, use smartphone

#ciii)How many interviewees reported having used the Internet but not having used a smartphone?
#285 have internet, do not use smartphone

#civ)How many interviewees reported having used a smartphone but not having used the Internet?
#17 no internet, use smartphone


#di) How many interviewees have a missing value for their Internet use?
summary(subset(poll,poll$Internet.Use==0))
#209 

#dii)How many interviewees have a missing value for their smartphone use?
summary(subset(poll,poll$Smartphone==0))
#187
 
#e) Use the subset function to obtain a data frame called limited", which is limited to interviewees who reported Internet use or who reported smartphone use.
#How many interviewees are in the new data frame?
limited <- subset(poll,poll$Internet.Use==1)
summary(limited)
#limited to internet use, 775

#f) Which variables have missing values in the limited data frame?
#Smartphone, Age, Conservativeness, WorryAboutInfro,PrivactImportance, AnonymityPossible, TriedMaskingIdentity, PrivacyLawsEffective

#g) What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
#3.857

#hi)How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet)
#95

#hii)How many interviewees reported the maximum value of 11 for Info.On.Internet?
#8

#i) What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet?
table(limited$Worry.About.Info)
(table(limited$Worry.About.Info)[2])/(table(limited$Worry.About.Info)[1]+table(limited$Worry.About.Info)[2])
383/(390+383)
#49.5%

#j) What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?
table(limited$Anonymity.Possible)
(table(limited$Anonymity.Possible)[2])/(table(limited$Anonymity.Possible)[1]+table(limited$Anonymity.Possible)[2])
#37.1%

#k)Build a histogram of the age of interviewees..
library(ggplot2)
table(limited$Age)
qplot(limited$Age, geom="histogram", 
      main="Histogram of Ages of the Interviewees",
      xlab="Age",
      col=I("black"),
      alpha=I(.5)
      )

#misc 1 WHO dataset
w <- read.csv("../W1/WHO.csv")
library(ggplot2)
ggplot(w,aes(x=GNI,y=FertilityRate)) + geom_point() + geom_smooth()

