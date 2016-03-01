ex31=read.csv("D:/r-SEM/data/ex31.csv") 
fit<-lm(formula=y~x1+x2,data=ex31)
summary(fit) 