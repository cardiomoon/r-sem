require(lavaan)
data=read.csv("data/data.csv")
str(data)
model <- 'price =~ x1 + x2 + x3 + x4
service =~ x5 + x6 + x7 + x8
atm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4
cl =~ y5 + y6 + y7 + y8'
fit <- cfa(model, data =data)
fit
getMethod("print","lavaan")
summary(fit)
summary(fit,header=FALSE,estimates=TRUE)

getMethod("summary","lavaan")
getMethod("short","lavaan")
short.summary(fit)


fitMeasures(fit,fit.measures = "all")
resid(fit)
fitMeasures(fit, fit.measures = "default")
print.fit.measures

PE<-parameterEstimates(fit,standardized=TRUE,add.attributes=TRUE)

PE

getMethod("print","lavaan.parameterEstimates")
getMethod("print","lavaan.data.frame")

print.lavaan.data.frame

str(PE)

PE$std.lv[46:55]


print(PE,nd=3L)
summary(fit)

summary(fit,header=FALSE,fit.measures=TRUE)

summary(fit, standardized = TRUE)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
