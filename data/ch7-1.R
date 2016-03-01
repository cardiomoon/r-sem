require(lavaan)

data=read.csv("data.csv")
model <- 'price =~ x1 + x2 + x3 + x4
service =~ x5 + x6 + x7 + x8
Atm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4
cl =~ y5 + y6 + y7 + y8
cs ~ price + service + Atm
cl ~ cs'
fit <- sem(model, data =data)
summary(fit, fit.measures=TRUE)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)

