ch3=read.csv("D:/r-SEM/data/ch3.csv") 
fit<-lm(formula=y~x, data=ch3)
summary(fit) 