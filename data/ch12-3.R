data=read.csv("data.csv")
model <- 'price =~ x1 + x2 + x3 + x4
service =~ x5 + x6 + x7 + x8
Atm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4
cl =~ y5 + y6 + y7 + y8
cs ~ price + service + Atm
cl ~ price + cs'
require(lavaan)
fit <- sem(model, data =data)
summary(fit, fit.measures=TRUE)
diagram<-semPlot::semPaths(fit, "std", "hide", sizeLat = 15, shapeLat = "star", 
                           shapeMan = "heart",
                           col = list(man = "pink", lat = "yellow"), 
                           residuals = FALSE, borders = FALSE,
                           edge.color = "purple", XKCD = TRUE, edge.width = 2,
                           rotation = 2, layout = "tree2", fixedStyle = 1, mar = c(1, 3, 1, 3))
