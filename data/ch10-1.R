ch10=read.csv("data/ch10.csv")
ch10

require(ggplot2)
require(reshape2)

data=melt(ch10,id.vars="gender")
data
p<-ggplot(data=data,aes(x=variable,y=value,group=gender))

model <- '초기치 =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
i ~ gender
s ~ gender'
fit <- growth(model, data =ch10)
summary(fit)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
