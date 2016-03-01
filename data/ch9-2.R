require(lavaan)
data=read.csv("data/data.csv")
model <- 'price =~ x1 + x2 + x3 + x4
service =~ x5 + x6 + x7 + x8
Atm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4
cl =~ y5 + y6 + y7 + y8
cs ~ a*price + b*service + c*Atm
cl ~ d*cs + f*price
indirect effect:= a*d+b*d+c*d
total effect:=f+(a*d)+(b*d)+(c*d)'
fit <- sem(model, data =data)
summary(fit, standardized=TRUE)

diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
str(fit)

addplot=function(mydoc,plotfunction,title=""){
  mydoc=addSlide(mydoc,"Title and Content")
  mydoc=addTitle(mydoc,title)
  mydoc=addPlot(mydoc,plotfunction,vector.graphic=FALSE)
  mydoc
}

require(ReporteRs)

mydoc=pptx()

mydoc=addSlide(mydoc,"Title Slide")
mydoc=addTitle(mydoc,"Results of SEM")
mydoc=addSubtitle(mydoc,"prepared by r-sem.com")

mydoc=addplot(mydoc,semPlot::semPaths(fit,
                                      whatLabels="std", intercepts=FALSE, style="lisrel",
                                      nCharNodes=0, 
                                      nCharEdges=0,
                                      curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE))

writeDoc(mydoc,file="r-sem.pptx")