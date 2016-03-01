set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)
model <- 'Y ~ c*X
M ~ a*X
Y ~ b*M
ab := a*b
total := c + (a*b)'
fit <- sem(model, data = Data)
summary(fit)
diagram<-semPlot::semPaths(fit,
             whatLabels="std", intercepts=FALSE, style="lisrel",
             nCharNodes=0, 
             nCharEdges=0,
             curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
