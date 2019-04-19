#1
rep(c(100:1),c(100:1))
  
#2
A <- c(1,2,0,4)
B <- c(3,6)
A*B
  
#3

#4
x <- factor(c(4, 5, 6, 6, 4))
as.numeric(as.character(x))
#factor vector does not store magnitudes, only levels
#one way to go about is to convert ot character 1st
