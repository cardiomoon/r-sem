lower <- '
11.834,
6.947,    9.364,
6.819,    5.091,   12.532,
4.783,    5.028,    7.495,    9.986,
-3.839,   -3.889,   -3.841,   -3.625,   9.610,
-21.899,  -18.831,  -21.748,  -18.775,  35.522,  450.288'
wheaton.cov <- 
  getCov(lower, names = c("anomia67", "powerless67", "anomia71", "powerless71",
                          "education", "sei"))
wheaton.model <- '
ses     =~ education + sei
alien67 =~ anomia67 + powerless67
alien71 =~ anomia71 + powerless71
alien71 ~ alien67 + ses
alien67 ~ ses'
anomia67  ~~ anomia71
powerless67 ~~ powerless71
fit <- sem(wheaton.model, sample.cov=wheaton.cov, sample.nobs=932)
result=summary(fit, fit.measures=TRUE)
str(result)
diagram<-semPlot::semPaths(fit,
                           whatLabels="std", intercepts=FALSE, style="lisrel",
                           nCharNodes=0, 
                           nCharEdges=0,
                           curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
