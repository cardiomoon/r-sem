require(lavaan)

ch4=read.csv("ch4.csv")
model <- 'y1 ~ x1 + x2 + x3  
y2 ~ x1 + x2 + x3'
fit <- sem(model, data = ch4)
summary(fit)
