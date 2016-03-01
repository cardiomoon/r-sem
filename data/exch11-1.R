exdata=read.csv("D:/r-SEM/data/exdata.csv")
model <- 'cuo <~ x1 + x2 + x3 + x4
es =~ x5 + x6 + x7 + x8
cs =~ x9 + x10 + x11 + x13
ci =~ x14 + x15 + x16 + x17
cl =~ x18 + x19 + x20 + x21
es ~ cuo + ci
cs ~ cuo + es
ci ~ cuo + cs
cl ~ cs
matrixpls.out <- matrixpls(cov(exdata), model) 
summary(matrixpls.out)
