require(lavaan)
require(semPlot)
require(qgraph)
require(ReporteRs)
require(devEMF)

mtcars

equation='Power=~ wt+hp
mpg~Power
' 

result=sem(equation,data=mtcars)
semPaths(result,"paths",ask=FALSE,title=FALSE)
diagram=semPaths(result,"paths",ask=FALSE,DoNotPlot=TRUE,title=FALSE)
emf(width=7,height=5)
diagram$Arguments$DoNotPlot=FALSE
qgraph::qgraph(diagram)
dev.off()

result=sem(equation,data=mtcars,group="am")
diagram=semPaths(result,"paths",ask=FALSE,DoNotPlot=TRUE,title=FALSE)

diagram[[1]]$Arguments$DoNotPlot=FALSE
diagram$Arguments$DoNotPlot=FALSE
qgraph::qgraph(diagram[[1]])

lapply(diagram,function(x) {
  
  x$Arguments$DoNotPlot=FALSE
  qgraph::qgraph(x)
})

count=length(diagram)
lapply(1:count,function(k) {
  png(paste0("semplot",k,".png"))
  diagram[[k]]$Arguments$DoNotPlot=FALSE
  qgraph::qgraph(diagram[[k]])
  title(paste0("am=",k))
  dev.off()
})

str(diagram[[1]])


gdiagram=c()

myqgraph=function(){
  qgraph::qgraph(gdiagram)
}

addqgraphPlot=function(mydoc,diagram,title="",...){
  mydoc=addSlide(mydoc,"Content")
  gdiagram<<-diagram
  mydoc=addPlot(mydoc,myqgraph,...)
  mydoc
}

addImageSlide=function(mydoc,image){
  mydoc=addSlide(mydoc,"Content")
  mydoc=addImage(mydoc,image)
  mydoc
}


lapply(1:count,function(i) {
  diagram[[i]]$Arguments$DoNotPlot=FALSE
  #qgraph::qgraph(diagram[[i]])
  title=paste0("am=",i)
  mydoc=addqgraphPlot(mydoc,diagram[[i]],title=title)
  mydoc
})  


mydoc=pptx(template="myppt.pptx")

mydoc=addSlide(mydoc,"Title Slide")
mydoc=addTitle(mydoc,"Results of SEM")
mydoc=addSubtitle(mydoc,"prepared by r-sem.com")

mydoc=addImageSlide(mydoc,"Rplot.emf")
mydoc=addqgraphPlot(mydoc,diagram)



fun_res = try( myqgraph, silent = T )
fun_res
inherits(fun_res, "try-error")

writeDoc(mydoc,file="r-semtest.pptx")


my.vector.pptx.graphic = function(doc, fun, pointsize = 11
                               , fontname = getOption("ReporteRs-default-font")
                               , editable = TRUE, offx, offy, width, height
                               , ... ) {
  slide = doc$current_slide
  plot_first_id = doc$plot_first_id
  
  check.dims = sum( c( !missing( offx ), !missing( offy ), !missing( width ), !missing( height ) ) )
  if( check.dims < 4 ){
    data.dims = get.graph.dims( doc )
    width = data.dims$widths
    height = data.dims$heights
    offx = data.dims$offxs
    offy = data.dims$offys
  }
  
  plotargs = list(...)
  
  dirname = tempfile( )
  dir.create( dirname )
  filename = file.path( dirname, "/plot_"  )
  filename = normalizePath( filename, winslash = "/", mustWork  = FALSE)
  
  env = dml.pptx( file = filename, width = width * 72.2, height = height * 72.2
                  , offx = offx * 72.2, offy = offy * 72.2, ps = pointsize, fontname = fontname
                  , firstid = doc$plot_first_id, editable = editable
  )
  
  fun_res = try( fun(...), silent = T )
  if( inherits(fun_res, "try-error")){
    dev.off()
    message(attr(fun_res,"condition"))
    stop("an error occured when executing plot function.")
  }
  last_id = .C("get_current_element_id", (dev.cur()-1L), 0L)[[2]]
  dev.off()
  
  doc$plot_first_id = last_id + 1
  
  
  
  plotfiles = list.files( dirname , full.names = T )
  
  for( i in seq_along( plotfiles ) ){
    dml.object = .jnew( class.DrawingML, plotfiles[i] )
    if( check.dims < 4 ){
      out = .jcall( slide, "I", "add", dml.object )
    } else {
      out = .jcall( slide, "I", "add", dml.object, width, height, offx, offy )
    }
    if( isSlideError( out ) ){
      stop( getSlideErrorString( out , "dml") )
    }
    
  }
  
  doc
}

set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)
model <- ' # direct effect
Y ~ c*X
# mediator
M ~ a*X
Y ~ b*M
# indirect effect (a*b)
ab := a*b
# total effect
total := c + (a*b)
'
fit <- sem(model, data = Data)
summary(fit)

