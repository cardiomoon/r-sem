require(rhandsontable)

values=reactiveValues()
current<-""
currentdir=getwd()
tempdir=tempdir()
owd <- setwd(tempdir)
on.exit(setwd(owd)) 
setwd(currentdir)
setHot=function(x) values[["Hot"]]=x

origin=reactive({
  
  if(input$Example=="uploaded_file") {
    if(is.null(input$file)) {
      if(input$language=="en") putmsg("Please upload file first !")
      else putmsg("먼저 file을 업로드하세요 !")   
      updateRadioButtons(session,"Example",selected="data.csv")
      df<-read.csv("data/data.csv")
    }            
    else df<-my_readfile(input$file) 
  }           
  else df<-read.csv(paste("data/",input$Example,sep=""),stringsAsFactors = FALSE)
  
  values$choice=input$Example
  values$new<-TRUE
  
  df
})

data=reactive({
  temp<-input$Example
  if(is.null(input$hot)) DF=origin()
  else {
    if((current=="")|(current==input$Example)){
      current<<-temp
      DF=hot_to_r(input$hot)
      
    } else {
      DF=origin()
      current<<-temp
    }       
    
  }       
  values[["DF"]]=DF
  DF
})


output$hot<-renderRHandsontable({
  DF=data() 
  
  hot=rhandsontable(DF) %>%
    hot_context_menu(
      customOpts = list(
        csv = list(name = "Remove all rows except 1",
                   callback = htmlwidgets::JS(
                     "function (key, options) {
                     this.alter('remove_row',1,this.countRows()-1);
}"))))
      
  hot
  })

output$table1<-renderTable({
  values$DF
  
})

df=reactive({
  values$DF
  
})