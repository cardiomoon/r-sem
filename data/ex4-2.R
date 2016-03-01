ex42=read.csv("D:/r-SEM/data/ex42.csv")
model <- 'y1 ~ x1 + x2 + x3 + x4  
y2 ~ y1'
fit <- sem(model, data = ex42)
summary(fit)
