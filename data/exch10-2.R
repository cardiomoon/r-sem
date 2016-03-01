ex10=read.csv("D:/r-SEM/data/ex10.csv")
model <- '
i =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
s =~ 0*x1 + 1*x2 + 2*x3 + 3*x4
i~e
s~e'
fit <- growth(model, data =ex10)
summary(fit)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
