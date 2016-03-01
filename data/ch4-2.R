ch4=read.csv("data/ch4.csv")
model <- 'y1 ~ x1 + x2 + x3  
y2 ~ x1 + x2 + x3'
fit <- sem(model, data = ch4)
str(fit)
summary(fit)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
summary(fit)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)


