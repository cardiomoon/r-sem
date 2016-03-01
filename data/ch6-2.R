data=read.csv("data/data.csv")
model <- 'price =~ x1 + x2 + x3 + x4
service =~ x5 + x6 + x7 + x8
atm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4
cl =~ y5 + y6 + y7 + y8'
fit <- cfa(model, data =data)

str(fit)

library(lavaan)

loadMatrix <- inspect(fit, "std")$lambda
loadMatrix[loadMatrix==0] <- NA
AVE=apply(loadMatrix^2,2,mean, na.rm = TRUE)
result=data.frame(AVE=AVE,SQRTAVE=sqrt(AVE))
result

summary(fit, standardized = TRUE, fit.measure=TRUE)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
