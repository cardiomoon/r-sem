require(lavaan)
require(semPlot)

HS.model <- '  visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school")
summary(fit)

semPaths(fit,ask=FALSE)


fit2 <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school",group.equal = c("loadings"))
summary(fit2)

semPaths(fit2,ask=FALSE)


# load the lavaan package (only needed once per session)
library(lavaan)

# specify the model
HS.model <- ' visual  =~ x1 + x2 + x3      
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '

# fit the model
fit <- cfa(HS.model, data=HolzingerSwineford1939)

# display summary output
summary(fit, fit.measures=TRUE)
semPaths(fit)

myfun=function(){
   semPaths(fit)
}
require(ReporteRs)
mydoc=pptx()

mydoc=addSlide(mydoc,"Title and Content")

mydoc=addPlot(mydoc,myfun)
mydoc=addPlot(mydoc,myfun,vector.graphic = FALSE)

writeDoc(mydoc,file="test.pptx")


help_console <- function(topic, format=c("text", "html", "latex", "Rd"),
                         lines=NULL, before=NULL, after=NULL) {  
  format=match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  helpfile = utils:::.getHelpFile(help(topic))
  
  hs <- capture.output(switch(format, 
                              text=tools:::Rd2txt(helpfile),
                              html=tools:::Rd2HTML(helpfile),
                              latex=tools:::Rd2latex(helpfile),
                              Rd=tools:::prepare_Rd(helpfile)
  )
  )
  if(!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  cat(hs, sep="\n")
  invisible(hs)
}

help_console(iris,"text")
