exdata=read.csv("D:/r-SEM/data/exdata.csv")
model <- 'cuo =~ x1 + x2 + x3 + x4
es =~ x5 + x6 + x7 + x8
cs =~ x9 + x10 + x11 + x13
ci =~ x14 + x15 + x16 + x17
cl =~ x18 + x19 + x20 + x21
es ~ a*cuo + f*ci
cs ~ b*cuo + d*es
ci ~ c*cuo + e*cs
cl ~ g*cs
indirect effect:= a*d + c*f + b*e + d*f
  total effect:= g+(a*d)+(b*e)+(c*f)+(d*f)'
fit <- sem(model, data =exdata,se="bootstrap")
summary(fit, standardized=TRUE)
diagram<-semPlot::semPaths(fit,
whatLabels="std", intercepts=FALSE, style="lisrel",
nCharNodes=0, 
nCharEdges=0,
curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)