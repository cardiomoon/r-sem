data=read.csv("data/data.csv")
model <- 'price =~ x1 + x2 + x3 + x4
service =~ x5 + x6 + x7 + x8
Atm <~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4
cl =~ y5 + y6 + y7 + y8
cs ~ price + service + Atm
cl ~ price + cs'
fit <- matrixpls::matrixpls(cov(data), model) 
summary(fit) 
str(result)
