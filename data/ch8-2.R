semTools::measurementInvariance(model,data=data,group="sex")

require(lavaan)
require(semPlot)
data=read.csv("data/data.csv")
model <- '가격 =~ x1 + x2 + x3 + x4
service =~ x5 + x6 + x7 + x8
Atm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4
cl =~ y5 + y6 + y7 + y8
cs ~ 가격 + service + Atm
cl ~ 가격 + cs'
#fit <- sem(model, data =data, group="sex",group.equal = c("loadings"))
fit <- sem(model, data =data, group="sex")
#fit <- sem(model, data =data)
summary(fit, fit.measures=TRUE)
#par(mfrow=c(1,1))
diagram<-semPlot::semPaths(fit,ask=FALSE,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,title=FALSE,
                           curveAdjacent = TRUE,layout="tree",curvePivot=TRUE,
                           DoNotPlot=TRUE)

length(diagram)
require(qgraph)


qgraph::qgraph(diagram[[2]])

diagram[[1]]$Arguments$DoNotPlot=FALSE
qgraph::qgraph(diagram[[1]])
length(diagram)
diagram[[2]]
diagram
