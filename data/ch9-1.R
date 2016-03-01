require(lavaan)
require(semPlot)
ch9=read.csv("data/ch9.csv")
model <- 'Y ~ b*M + c*X  
          M ~ a*X
          indirect effect:=a*b
          total effect:=c+(a*b)'

#fit <- sem(model, data=ch9, se="bootstrap")
fit <- sem(model, data=ch9)
summary(fit,standardized=TRUE)


PE <- parameterEstimates(object, ci = ci, standardized = standardized, 
                         rsquare = rsquare, fmi = fmi, remove.eq = FALSE, 
                         remove.system.eq = TRUE, remove.ineq = FALSE, 
                         remove.def = FALSE, add.attributes = TRUE)

getMethod("summary","lavaan")

diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE, layout="tree2",curvePivot=TRUE,
                           DoNotPlot=TRUE)
require(qgraph)
str(diagram)
qgraph(diagram)
diagram$Arguments$DoNotPlot=FALSE
qgraph(diagram)
ch9
cor(ch9)

multilevel::sobel(ch9$X,ch9$M,ch9$Y)
bda::mediation.test(ch9$M,ch9$X,ch9$Y)


a=0.589
b=0.337
sa=0.099
sb=0.771

a*b/sqrt(b^2*sa^2+a^2*sb^2)
a*b/sqrt(b^2*sa^2+a^2*sb^2+sa^2*sb^2)
a*b/sqrt(b^2*sa^2+a^2*sb^2-sa^2*sb^2)


bda.mediation.test