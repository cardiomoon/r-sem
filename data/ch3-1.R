ch31=read.csv("D:/r-SEM/data/ch31.csv") 
fit<-lm(formula=y~x1+x2, data=ch31)
summary(fit) 