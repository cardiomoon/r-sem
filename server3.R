library(shiny)
#require(moonBook)
#require(ztable)
#require(mycor)
#require(ggplot2)
require(plyr)
require(rmarkdown)
require(readxl)
#require(ggthemes)
#require(reshape)
require(rhandsontable)
#require(shinyjs)
require(ReporteRs)
require(lavaan)
require(semPlot)
require(extrafont)
#require(matrixpls)

source("cleaning.R")

#loadfonts()

myequation=c("y1~x1+x2+x3\ny2~x1+x2+x3",
"price =~ x1 + x2 + x3 + x4\nservice =~ x5 + x6 + x7 + x8\natm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4\ncl =~ y5 + y6 + y7 + y8",
"price =~ x1 + x2 + x3 + x4\nservice =~ x5 + x6 + x7 + x8\natm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4\ncl =~ y5 + y6 + y7 + y8\ncs~price+service+atm\ncl~cs",
"price =~ x1 + x2 + x3 + x4\nservice =~ x5 + x6 + x7 + x8\natm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4\ncl =~ y5 + y6 + y7 + y8\ncs~price+service+atm\ncl~price+cs",
"Y ~ b*M + c*X\nM ~ a*X\nindirect effect:=a*b\ntotal effect:=c+(a*b)",
"price =~ x1 + x2 + x3 + x4\nservice =~ x5 + x6 + x7 + x8\natm =~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4\ncl =~ y5 + y6 + y7 + y8\ncs ~ a*price + b*service + c*atm
cl ~ d*cs + f*price\nindirect effect:= a*d+b*d+c*d\ntotal effect:=f+(a*d)+(b*d)+(c*d)",
"초기치 =~ 1*t1 + 1*t2 + 1*t3 + 1*t4\n변화율 =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
초기치 ~ gender\n변화율 ~ gender",
"price =~ x1 + x2 + x3 + x4\nservice =~ x5 + x6 + x7 + x8\nAtm <~ x9 + x10 + x11 + x12
cs =~ y1 + y2 + y3 + y4\ncl =~ y5 + y6 + y7 + y8\ncs ~ price + service + Atm
cl ~ price + cs"
)

myfile=c("ch4.csv","data.csv","data.csv","data.csv","ch9.csv","data.csv","ch10.csv","data.csv")


load("translation.bin") # contains the dictionary, parsed as a double list


options(ztable.type="html")
options(shiny.maxRequestSize=30*1024^2)

dic=read.csv("dictionary.csv",fileEncoding="utf-8",stringsAsFactors = FALSE)

shinyServer(function(input, output,session) {
    

    shinyjs::onclick("toggleModelPlot", shinyjs::toggle(id = "modelPlot", anim = TRUE))  
    shinyjs::onclick("toggleFinalPlot", shinyjs::toggle(id = "finalPlot", anim = TRUE))  
    shinyjs::onclick("toggle2ndEquation", shinyjs::toggle(id = "2ndEquation", anim = TRUE))  

    tr <- function(text){ # translates text into current language
        sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
    }
    
    tr2out=function(string,size=3){
        output[[string]]=renderPrint(cath(tr(string),size))
    }
    
    md2html=function(string){
        HTML(markdownToHTML(fragment.only=TRUE,text=string))
    }    
    
    

    file2ext=function(filename){
        namelist=unlist(strsplit(filename,".",fixed=TRUE))
        result=namelist[length(namelist)]
        if((result=="csv")|(result=="xlsx")) return(result)
        else return(NULL)
    }
    
    file2newname=function(file){
        mypath=unlist(strsplit(file$datapath,"/"))
        length(mypath)
        temp=mypath[1]
        for(i in 2:(length(mypath)-1)) temp=paste(temp,mypath[i],sep="/")
        result=paste(temp,"/","test.",file2ext(file$name),sep="")
        result
    }

    my_theme=function(p){
        if(input$theme!="gray") {p<-eval(parse(text=paste("p+theme_",input$theme,"()",sep="")))}
        p
    }
        
    my_readfile=function(file){
        ext=file2ext(file$name)
        result=NULL
        if(is.null(ext)) {
            session$sendCustomMessage(type = 'testmessage',
                                      message = list( 'Only file with xlsx or csv format is supported.'))
        } else if(ext=="csv") {
            try(result<-read.csv(file$datapath,header=TRUE,stringsAsFactors = FALSE),silent=TRUE)
            if(is.null(result)) {
                try(result<-read.csv(file$datapath,header=TRUE,fileEncoding="euc-kr",stringsAsFactors = FALSE),silent=TRUE)
                if(is.null(result)) session$sendCustomMessage(type = 'testmessage',
                                                      message = list( 'File read error : please check file encoding')) 
            }
        }  else {
            newname=file2newname(file)
            file.copy(file$datapath,newname)
            result=read_excel(newname)
            if(is.null(result)) session$sendCustomMessage(type = 'testmessage',
                                                          message = list( 'File read error : please check file encoding')) 
        } 
        result
    }
    mypaste=function(tempA,...,sep=","){
        result=""
        if(tempA=="")  result=paste0(...)
        else result=paste0(tempA,sep,...)
        result
    }
    myplotPNG=function(listf,width=input$plotWidth,height=input$plotHeight,
                       units="in",res=300,start=0){
        filename=c()
        count=length(listf)
        if(count>0) for(i in 1:count){
            path <- paste("plot_", i+start, ".png", sep="")
            filename <- c(filename, path)
            plotPNG(listf[[i]],path,width=width,height=height,
                    units=units,res=res)
        }
        filename
    }
    
    myplotPDF=function(listf,isggplot=NULL,width=input$plotWidth,height=input$plotHeight,
                       units="in",res=300,start=0){
        filename=c()
        count=length(listf)
        if(is.null(isggplot)) isggplot=rep(FALSE,count)
        if(count>0) for(i in 1:count){
            path <- paste("plot_", i+start, ".pdf", sep="")
            filename <- c(filename, path)
            plotPDF(listf[[i]],path,width=width,height=height,
                    units=units,res=res,isggplot[i])
        }
        filename
    }
    
    plotPDF=function(fun,file,width=7,height=5,units="in",res=300,ggplot=FALSE){
        
        if(ggplot) ggsave(file,fun(),width=width,device=cairo_pdf,height=height,units=units,dpi=res)
        else {
            cairo_pdf(file,width=width,height=height)
            #pdf(file,paper="letter")
            fun()
            dev.off()
        }
        
    }   
    
    myplotSVG=function(listf,isggplot=NULL,width=input$plotWidth,height=input$plotHeight,
                      start=0){
        filename=c()
        count=length(listf)
        if(is.null(isggplot)) isggplot=rep(FALSE,count)
        if(count>0) for(i in 1:count){
            path <- paste("plot_", i+start, ".svg", sep="")
            filename <- c(filename, path)
            plotSVG(listf[[i]],path,width=width,height=height,
                   isggplot[i])
        }
        filename
    }
    
    plotSVG=function(fun,file,width=7,height=5,ggplot=FALSE){
        
        if(ggplot) ggsave(file,fun(),width=width,height=height)
        else {
            svg(file,width=width,height=height)
            #pdf(file,paper="letter")
            fun()
            dev.off()
        }
        
    }   

  
    
    cath=function(string="",size=3){
         cat(paste("<h",size,">",string,"</h",size,">",sep=""))
    }
    
  sectionReport2=function(reportname){
        downloadHandler(
            filename = function() {
                paste(reportname, sep = '.', switch(
                    input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
            },
            
            content = function(file) {
                #if(input$format=="PDF") tempname=paste(reportname,'1.Rmd',sep="")
                #else 
                tempname=paste(reportname,'.Rmd',sep="")
                src <- normalizePath(tempname)
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, tempname)
                
                out <- render(tempname, switch(
                    input$format,
                    PDF = pdf_document(), HTML = html_document(), Word = word_document()
                ))
                file.rename(out, file)
            }
        )
    }
    
   output$semReport=sectionReport2("semReport")
   

   putmsg=function(msg="test message"){
       session$sendCustomMessage(type = 'testmessage',message = list( msg)) 
   }
   
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
  
  observe({
    
    df=df()
    
    vars=c("Select..."="",colnames(df))
    # For Meta-analysis
    
    updateSelectInput(session,"leftvar",choices=c(vars,"indirect effect","total effect"))
    updateSelectInput(session,"rightvar",choices=c(vars,"1"))
    updateSelectInput(session,"indepvar",choices=vars)
    updateSelectInput(session,"resvar",choices=vars)
    updateSelectInput(session,"mediator",choices=vars)
    updateSelectInput(session,"group",choices=c("",vars))
  
    if(input$language=="kor") {
      shinyjs::text("Title1","<h1>웹에서 하는 R 구조 방정식 모형</h1>")
      shinyjs::text("introduction",md2html("R을 이용하여 `구조방정식 모형`을 이용한 분석을 할 수 있습니다."))
      updateSelectInput(session, "SelectEx",label="예제 선택",
                        choices=c("선택안함"=0,"경로분석"=1,"확인요인분석"=2,
                                     "구조방정식모형"=3,"교차타당성분석"=4,
                                     "매개효과분석"=5,"매개효과분석2"=6,
                                     "잠재성장모형"=7,
                                     "부분최소제곱(PLS)"=8))
      updateRadioButtons(session, "method",label = "분석 옵션 선택",
                         choices=c("구조방정식모형 사용"="sem",
                                      "확인분석모형 사용"="cfa",
                                      "성장곡선모형 사용"="growth",
                                      "부분최소회귀(PLS)모형 사용"="matrixpls"))
      updateRadioButtons(session, inputId = "Example", label = "데이타 선택",
                         choices = c("data.csv", 
                                        "ex31.csv",
                                        "ch9.csv",
                                        "ch4.csv",
                                        "ch10.csv",
                                        "ADHD.csv",
                                        "업로드한 파일"),selected=input$Example)
    }    
    else {
      shinyjs::text("Title1","<h1>Structural Equation Modeling with R</h1>")
      shinyjs::text("introduction",md2html("With this app, you can perform `structural equation modeling`."))
      updateSelectInput(session,inputId = "SelectEx",label="Select Example",
                        choices=c("None"=0,"Path Analysis"=1,"Confirmatory Factor Analysis"=2,
                                     "Structural Equation Model"=3,"Cross-Validation Analysis"=4,
                                     "Mediation Effect Analysis"=5,"Mediation Effect Analysis2"=6,
                                     "Latent Growth Modeling"=7,
                                     "Partial Least Square"=8))
      updateRadioButtons(session,"method",label="Analysis options",
                         choices=c("fit a Structural Equation Model"="sem",
                                      "fit a Confirmatory Factor Analysis Models"="cfa",
                                      "fit a Growth Curve Model"="growth",
                                      "fit a Partial Least Squares Model"="matrixpls"))
      updateRadioButtons(session,inputId = "Example", label = "Select Data",
                         choices = c("data.csv", 
                                        "ex31.csv",
                                        "ch9.csv",
                                        "ch4.csv",
                                        "ch10.csv",
                                        "ADHD.csv",
                                        "uploaded_file"),selected=input$Example)
     
    }    
    
    headings=dic$key[dic$class=="output"]
    for(i in 2:length(headings)) tr2out(headings[i])
    
    checks=dic$key[dic$class=="Checkbox"]
    for(i in 1:length(checks)) updateCheckboxInput(session,checks[i],label=tr(checks[i]))
    
    selects=dic$key[dic$class=="Select"]
    for(i in 1:length(selects)) updateSelectInput(session,selects[i],label=tr(selects[i]))
    
    texts=dic$key[dic$class=="Text"]
    for(i in 1:length(texts)) updateTextInput(session,texts[i],label=tr(texts[i]))
    
    radios=dic$key[dic$class=="Radio"]
    for(i in 1:length(radios)) updateRadioButtons(session,radios[i],label=tr(radios[i]))
    
    numerics=dic$key[dic$class=="Numeric"]
    for(i in 1:length(numerics)) updateNumericInput(session,numerics[i],label=tr(numerics[i]))
    
    
   updateRadioButtons(session,'language',label=tr('language'))
   
})
  
  observe({
    #shinyjs::toggleState(id='semReport',condition=TRUE==FALSE)
    #shinyjs::toggleState(id='downloadPlot',condition=TRUE==FALSE)
    #shinyjs::toggleState(id='downloadPPT',condition=TRUE==FALSE)
  })                
  
observe({  

     if(input$SelectEx!=0){
         
         vars=colnames(df())
         choice=as.integer(input$SelectEx)
         updateRadioButtons(session,"Example",
                            selected=myfile[choice])
         updateTextInput(session,"equation",value=myequation[choice])
         if(input$SelectEx==2) {
             updateRadioButtons(session,"method",selected="cfa")
             updateCheckboxInput(session,"fit.measures",value=TRUE)
         }   
         else if(input$SelectEx==7) updateRadioButtons(session,"method",selected="growth")
         else if(input$SelectEx==8) updateRadioButtons(session,"method",selected="matrixpls")
         else updateRadioButtons(session,"method",selected="sem")
         
         if(input$SelectEx==4) {
           updateSelectInput(session,"group",selected="sex")
         }
         if(input$SelectEx==5) {
           updateCheckboxInput(session,"moderating",value=TRUE)
           updateSelectInput(session,"indepvar",choices=vars,selected="X")
           updateSelectInput(session,"mediator",choices=vars,selected="M")
           updateSelectInput(session,"resvar",choices=vars,selected="Y")

         }
       
     }
    
  })
  
  plustext=function(x){
      if(is.null(x)) result=""
      if(length(x)>0) result=x[1]
      if(length(x)>1) for(i in 2:length(x)) result=paste(result,x[i],sep="+")
      result
  }
  
  annotatetext=function(x){
    if(is.null(x)) result=""
    else{
    if(length(x)>0) result=paste0("c('",x[1],"'")
    if(length(x)>1) {
      for(i in 2:length(x)) result=paste0(result,",'",x[i],"'")
    }  
    result=paste0(result,")")
    }
    result
  }
  
  EqText=reactive({
      temp=""
      if(input$lefttext!="") temp=input$lefttext
      else temp=plustext(input$leftvar)
      if((temp=="") & (length(input$rightvar)==0)) result<-""
      else{
          result=paste(temp,input$operator, plustext(input$rightvar))  
      }
      result
  })
  
  newvar=c()
  
  observeEvent(input$add, {
    
      if(input$equation=="") updateTextInput(session,"equation",value=EqText()) 
      else updateTextInput(session,"equation",value=paste(input$equation,EqText(),sep="\n"))
      if(input$lefttext!="") {
         newvar<<-c(newvar,input$lefttext)
      }
      newchoices=c("Select..."="",colnames(df()),newvar)
      updateSelectInput(session,"leftvar",choices=newchoices,selected="")
      updateSelectInput(session,"rightvar",choices=newchoices,selected="")
      updateSelectInput(session,"indepvar",choices=newchoices)
      updateSelectInput(session,"resvar",choices=newchoices)
      updateSelectInput(session,"mediator",choices=newchoices)
      updateSelectInput(session,"group",choices=newchoices)
      updateTextInput(session,"lefttext",value="")
  })
  
  
  makeMediatorEq=function(){
     temp=""
     if((input$indepvar=="")|(input$resvar=="")|(input$mediator=="")) temp=""
     else {
         temp=paste0(input$resvar," ~ b*",input$mediator," + c*",input$indepvar,"\n")
         temp=paste0(temp,input$mediator," ~ a*",input$indepvar,"\n")
         temp=paste0(temp,"indirect effect:=a*b\n")
         temp=paste0(temp,"total effect:=c+(a*b)")
     }     
     temp
  }
  
  observeEvent(input$MakeEquation, {
    
    temp<-makeMediatorEq()
    if(temp=="") putmsg("Please Select Variables First")
    else {
      if(input$equation=="") updateTextInput(session,"equation",value=temp) 
      else updateTextInput(session,"equation",value=paste(input$equation,temp,sep="\n"))
      
    }  
    
  })
  
  observeEvent(input$reset, {
    updateTextInput(session,"equation",value="")
    updateSelectInput(session,"leftvar",selected="")
    updateSelectInput(session,"rightvar",selected="")
  })
  
  observeEvent(input$ResetEx, {
    updateTextInput(session,"equation",value="")
    updateSelectInput(session,"SelectEx",selected=0)
    updateRadioButtons(session,"method",selected="sem")
    updateCheckboxInput(session,"fit.measures",value=FALSE)
    
  })
  
  
  
  output$EquationText=renderPrint({
      if(EqText()=="") {
        if(input$language=="en") cat("Select variables and operators to make a equation !")
        else cat("변수와 연산자를 선택하여 구조방정식을 만드세요!")
      }  
      else cat(EqText())
  })
  
  output$exportCSV = downloadHandler(
      filename="Mydata.csv",
      content=function(file){
          
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          
          write.csv(df(),file=file,row.names=FALSE)
      },
      contentType="text/csv"
  )

   output$downloadReport <- downloadHandler(
       filename = function() {
           paste('my-report', sep = '.', switch(
               input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
           ))
       },
       
       content = function(file) {
           src <- normalizePath('report.Rmd')
           
           # temporarily switch to the temp dir, in case you do not have write
           # permission to the current working directory
           owd <- setwd(tempdir())
           on.exit(setwd(owd))
           file.copy(src, 'report.Rmd')
           
           out <- render('report.Rmd', switch(
               input$format,
               PDF = pdf_document(), HTML = html_document(), Word = word_document()
           ))
           file.rename(out, file)
       }
   )
   
 #  }) 
   
  
   
   
   # change character variables to factor variables
   df1=reactive({
       df1=df()
       select=sapply(df1,function(x) length(unique(x))<=4)
       count=length(which(select==TRUE))
       if(count>1) df1[,select]=lapply(df1[,select],factor)
       else if(count==1) df1[select]=lapply(df1[select],factor)
       df1
   })
  
  
output$citation1=renderPrint({
    if(input$language=="kor"){
    cat(paste("먼저 방법에서 ",version$version.string,"을 사용하여 통계처리를 하였다.`라고 기술해야 한다. 패키지를 사용하는 경우 패키지를 인용해야 하는데 대부분의 경우 통계분석을 하는데 사용한 모든 R패키지를 다 인용하지는 않는다. 일반적으로 사용되는 통계방법이 아닌 새로운 통계방법이나 통계방법을 특별히 언급해야 할 필요가 있는 경우에만 인용하는 것이 일반적이다. R version 은 달라질 수 있다. R version 및 사용한  패키지 정보는 다음 version() 및 sessionInfo()에서 확인할 수 있다.\n",sep=""))
    cat("\n\nR의 version\n\n")
    print(version)
    cat("\n\n웹 R의 sessionInfo()\n\n")
    print(sessionInfo())
    cat("\nReferences\n\n")
    cat("1. R을 인용할 때:\n")
    print(citation())
    cat("2. lavaan 패키지의 인용정보\n")
    print(citation("lavaan"))
    cat("3. semPlot 패키지의 인용정보\n")
    print(citation("semPlot"))
    } else{
        cat(paste("In your 'Method' section, please citate R as 'Analyses were performed using ",version$version.string," statistical software.' You can find informations about R version and sessionInfo as followings.\n",sep=""))
        cat("\n\nR version\n\n")
        print(version)
        cat("\n\nsessionInfo() of r-meta.com\n\n")
        print(sessionInfo())
        cat("\nReferences\n\n")
        cat("1. To citate R:\n")
        print(citation())
        cat("2. To citate package 'lavaan'\n")
        print(citation("lavaan"))
        cat("3. To citate package 'semPlot'\n")
        print(citation("semPlot"))
    }
    
    
})


output$sem.ui1=renderUI({
  
  
  input$doSEM
  
  isolate({
    if(input$equation!=""){    
      #fit<-myfit()
      tagList(
#         htmlOutput('result0'),
#         verbatimTextOutput('semPlotText0'),
#         plotOutput('SEMPlot0',height=700),
#         
#         if(input$equation2!="") verbatimTextOutput('semPlotText02'),
#         if(input$equation2!="") plotOutput('SEMPlot02',height=700),
        htmlOutput('result2'),
        verbatimTextOutput('semText'),
        htmlOutput('result1'),
        verbatimTextOutput('semPlotText'),
        uiOutput('semplot.ui'),
        #plotOutput('SEMPlot',height=700),
        if(input$equation2!="") verbatimTextOutput('semPlotText2'),
        if(input$equation2!="")  uiOutput('semplot2.ui')
          #plotOutput('SEMPlot2',height=700)
        
       
      )
    }
  })
  
})


output$semplot.ui=renderUI({
  input$doSEM
  
  isolate({
    if(input$equation!=""){    
      
      if(input$group=="") plotOutput('SEMPlot',height=700)
      else {
        count=length(unique(df()[[input$group]]))
        if(count==1) plotOutput('SEMPlot',height=700)
        else {
            fit<-myfit()    
            diagram<-mysemPlot(DoNotPlot=TRUE)
            
            semplot_output_list<-lapply(1:count,function(i) {
        
             semplotname<-paste0("semplot",i)

             diagram[[i]]$Arguments$DoNotPlot=FALSE
             output[[semplotname]] <- renderPlot({
               
               qgraph::qgraph(diagram[[i]])
               title(paste0(input$group,"=",i))
               })
              
                plotOutput(semplotname,height=700)
              }) 
           
            do.call(tagList,semplot_output_list)
           
        }
      }
    }
  })
})

output$semplot2.ui=renderUI({
  input$doSEM
  
  isolate({
    if(input$equation2!=""){    
      
      if(input$group=="") plotOutput('SEMPlot2',height=700)
      else {
        count2=length(unique(df()[[input$group]]))
        if(count2==1) plotOutput('SEMPlot2',height=700)
        else {
          fit<-myfit2()
          diagram2<-mysemPlot2(DoNotPlot=TRUE)
          
          sem2plot_output_list<-lapply(1:count2,function(k) {
            sem2plotname<-paste0("sem2plot",k)
            diagram2[[k]]$Arguments$DoNotPlot=FALSE
            output[[sem2plotname]] <- renderPlot({
              
              qgraph::qgraph(diagram2[[k]])
              title(paste0(input$group,"=",k))
            })
            plotOutput(sem2plotname,height=700)
            
          })
          do.call(tagList,sem2plot_output_list)
          
        }
      }
    }
  })
})

myfittext=function(group.equal=1){
  if(input$method!="matrixpls") temp=paste0(input$method,"(input$equation,data=df()")
  else temp <-"matrixpls::matrixpls(cov(df()),input$equation"
  if(input$group!="") temp=paste0(temp,",group='",input$group,"'")
  if(group.equal & !is.null(input$group.equal)) temp=paste0(temp,",group.equal=",annotatetext(input$group.equal))
  if(input$se!="default") temp=paste0(temp,",se='",input$se,"'")
  temp=paste0(temp,")")
  temp
}

myfit=function(group.equal=1){
  temp=myfittext(group.equal=group.equal)
  fit=eval(parse(text=temp))   
  fit
}

myfittext2=function(group.equal=1){
  if(input$method!="matrixpls") temp=paste0(input$method,"(input$equation2,data=df()")
  else temp <-"matrixpls::matrixpls(cov(df()),input$equation2"
  if(input$group!="") temp=paste0(temp,",group='",input$group,"'")
  if(group.equal & !is.null(input$group.equal)) temp=paste0(temp,",group.equal=",annotatetext(input$group.equal))
  if(input$se!="default") temp=paste0(temp,",se='",input$se,"'")
  temp=paste0(temp,")")
  temp
}

myfit2=function(group.equal=1){
  temp=myfittext2(group.equal=group.equal)
  fit=eval(parse(text=temp))   
  fit
}

output$semPlotText=renderPrint({
  input$doSEM
  
  isolate({
    if(input$equation!=""){ 
      cat("fit=",myfittext(),"\n")
      cat(diagramTemp())
    }
  })
})

output$semPlotText2=renderPrint({
  input$doSEM
  
  isolate({
    if(input$equation!=""){   
      cat("fit=",myfittext2(),"\n")
      cat(diagramTemp())
    }
  })
})

output$semPlotText0=renderPrint({
  input$doSEM
  
  isolate({
    if(input$equation!=""){   
      cat("fit=",myfittext(),"\n")
      cat(diagramTemp0())
    }
  })
})

output$semPlotText02=renderPrint({
  input$doSEM
  
  isolate({
    if(input$equation!=""){   
      cat("fit=",myfittext2(),"\n")
      cat(diagramTemp0())
    }
  })
})
 
output$semText=renderPrint({
  input$doSEM
  
  isolate({
      if(input$equation!=""){    
        
         
        #print(str(fit))

        if((input$moderating)&(input$showcor)){
#           cat("## Correlation analysis\n")
#           tempdf=df()[,c(input$indepvar,input$mediator,input$resvar)]
#           print(cor(tempdf))
#           cat("\n\n")
        }  
         cat("## Results of Analysis\n\nfit=",myfittext(),"\n")
         if(input$method=="matrixpls") {
           
           cat("summary(fit)\n")
           fit<-matrixpls::matrixpls(cov(df()),input$equation)
           summary(fit)
           print(summary(fit))
         } else {
           cat("summary(fit,standardized=",input$standardized,
               ",fit.measures=",input$fit.measures,
               ",rsquare=",input$rsquare,",modindices=",input$modindices,")\n\n")
           fit<-myfit()
           summary(fit,standardized=input$standardized,fit.measures=input$fit.measures,
                   rsquare=input$rsquare,modindices=input$modindices)
         
         } 
         
         if((input$moderating)&(input$sobel)){
             PE<-parameterEstimates(fit)
             a=PE$est[3]
             sa=PE$se[3]
             b=PE$est[1]
             sb=PE$se[1]
             
             result=Sobel(a,b,sa,sb)
             print(result)
           
         }   
         if(input$method=="cfa"){
             cat("\n\n## Average Variance Extracted \n\n")
             loadMatrix <- inspect(fit, "std")$lambda
             loadMatrix[loadMatrix==0] <- NA
             AVE=apply(loadMatrix^2,2,mean, na.rm = TRUE)
             result=data.frame(AVE=AVE,SQRTAVE=sqrt(AVE))
             print(result)
         }
      
      }
      if(input$equation2!=""){ 
          fit2=myfit2()
          cat("\n\n## Results of Analysis using equation2\n\nfit2=",myfittext2(),"\n")
          cat("summary(fit2,standardized=",input$standardized,
              ",fit.measures=",input$fit.measures,
              ",rsquare=",input$rsquare,",modindices=",input$modindices,")\n\n")
          summary(fit2,standardized=input$standardized,fit.measures=input$fit.measures,
                  rsquare=input$rsquare,modindices=input$modindices)
          if(input$compareModels){
              cat("\n\n## Compare Models\n")
              cat("\n\n## call: anova(fit,fit2)\n")
              anova(fit,fit2)
          }
          
      }  
  })
})


diagramTemp=function(DoNotPlot=FALSE){
  
  temp="semPaths(fit"
  if(input$what!="paths") temp=paste0(temp,",what='",input$what,"'")
  if(input$whatLabels!="") temp=paste0(temp,",whatLabels='",input$whatLabels,"'")
  if(input$style!="ram") temp=paste0(temp,",style='",input$style,"'")
  if(input$layout!="tree") temp=paste0(temp,",layout='",input$layout,"'")
  if(input$intercept==FALSE) temp=paste0(temp,",intercept=FALSE")
  if(input$residuals==FALSE) temp=paste0(temp,",residuals=FALSE")
  if(input$thresholds==FALSE) temp=paste0(temp,",thresholds=FALSE")
  if(input$nCharNodes!=3) temp=paste0(temp,",nCharNodes=",input$nCharNodes)
  if(input$nCharEdges!=3) temp=paste0(temp,",nCharEdges=",input$nCharEdges)
  if(input$rotation!=1) temp=paste0(temp,",rotation=",input$rotation)
  if(input$groups!="") temp=paste0(temp,",groups='",input$groups,"'")
  if(input$pastel==TRUE) temp=paste0(temp,",pastel=TRUE")
  if(input$curvePivot==TRUE) temp=paste0(temp,",curvePivot=TRUE")
  if(DoNotPlot) temp=paste0(temp,",DoNotPlot=TRUE,title=FALSE")
  if(input$Other!="") temp=paste0(temp,",",input$Other)
  temp=paste0(temp,",curveAdjacent=TRUE,ask=FALSE)")
  temp

}

diagramTemp0=function(DoNotPlot=FALSE){
  
  temp="semPaths(fit"
  if(input$what0!="paths") temp=paste0(temp,",what='",input$what0,"'")
  if(input$whatLabels0!="") temp=paste0(temp,",whatLabels='",input$whatLabels0,"'")
  if(input$style0!="ram") temp=paste0(temp,",style='",input$style0,"'")
  if(input$layout0!="tree") temp=paste0(temp,",layout='",input$layout0,"'")
  if(input$intercept0==FALSE) temp=paste0(temp,",intercept=FALSE")
  if(input$residuals0==FALSE) temp=paste0(temp,",residuals=FALSE")
  if(input$thresholds0==FALSE) temp=paste0(temp,",thresholds=FALSE")
  if(input$nCharNodes0!=3) temp=paste0(temp,",nCharNodes=",input$nCharNodes0)
  if(input$nCharEdges0!=3) temp=paste0(temp,",nCharEdges=",input$nCharEdges0)
  if(input$rotation0!=1) temp=paste0(temp,",rotation=",input$rotation0)
  if(input$groups0!="") temp=paste0(temp,",groups='",input$groups0,"'")
  if(input$pastel0==TRUE) temp=paste0(temp,",pastel=TRUE")
  if(input$curvePivot0==TRUE) temp=paste0(temp,",curvePivot=TRUE")
  if(DoNotPlot) temp=paste0(temp,",DoNotPlot=TRUE,title=FALSE")
  if(input$Other0!="") temp=paste0(temp,",",input$Other0)
  temp=paste0(temp,",curveAdjacent=TRUE,ask=FALSE)")
  temp
  
}

mysemPlot=function(DoNotPlot=FALSE,which=0){
  
  if(input$equation!=""){    
    #fit<-myfit(group.equal=0)
    fit<-myfit()
    if(input$method=="matrixpls") fit=sem(data=df(),input$equation)
    else fit=fit
    diagram<-eval(parse(text=diagramTemp(DoNotPlot)))
#     if(which==0) {
#       diagram$Arguments$DoNotPlot=TRUE
#       diagram
#     } else {
#       diagram[[which]]$Arguments$DoNotPlot=FALSE
#       qgraph::qgraph(diagram[[which]])
#       
#     } 
    
  }
}

mysemPlot2=function(DoNotPlot=FALSE){
  if(input$equation2!=""){    
    #fit=myfit2(group.equal=0)
    fit=myfit2()
    diagram<-eval(parse(text=diagramTemp(DoNotPlot)))
    diagram
    
  }
}

output$SEMPlot=renderPlot({
  input$doSEM
  
  isolate({
    if(input$equation!=""){    
       mysemPlot()
    }
  })
})

output$Model.ui=renderUI({
    if((input$preview) & (input$equation!=""))  plotOutput('ModelPlot',height=700)
})

output$ModelPlot=renderPlot({
  if((input$preview) & (input$equation!=""))  mysemPlot()
})


output$SEMPlot2=renderPlot({
  input$doSEM
  
  isolate({
    if(input$equation!=""){    
      mysemPlot2()
    }
  })
})

# addggplot=function(mydoc,plot,title=""){
#   mydoc=addSlide(mydoc,"Title and Content")
#   mydoc=addTitle(mydoc,title)
#   mydoc=addPlot(mydoc,fun=print,x=plot,vector.graphic=TRUE)
#   mydoc
# }
# 
# addplot=function(mydoc,plotfunction,title=""){
#   mydoc=addSlide(mydoc,"Title and Content")
#   mydoc=addTitle(mydoc,title)
#   mydoc=addPlot(mydoc,function() {plotfunction},vector.graphic=TRUE)
#   mydoc
# }

addplot=function(mydoc,plotfunction,title="",...){
  mydoc=addSlide(mydoc,"Title and Content")
  mydoc=addTitle(mydoc,title)
  mydoc=addPlot(mydoc,function() {plotfunction},vector.graphic=TRUE,...)
  mydoc
}

gdiagram=c()

myqgraph=function(){
  qgraph::qgraph(gdiagram)
}

addqgraphPlot=function(mydoc,diagram,title="",...){
  mydoc=addSlide(mydoc,"Content")
 # mydoc=addTitle(mydoc,title)
  gdiagram<<-diagram
  mydoc=addPlot(mydoc,myqgraph,vector.graphic=FALSE,...)
  if(title!="") mydoc=addFooter(mydoc,title)
  mydoc
}

mysemplotlist=function(){
    
    fit<-myfit()    
    diagram<-mysemPlot(DoNotPlot=TRUE)
    if(input$group=="") count=1
    else count=length(unique(df()[[input$group]]))
    
    result=list()
    if(count==1) {
       diagram$Arguments$DoNotPlot=FALSE
       result=list(result,qgraph::qgraph(diagram))
    }    
    else for(i in 1:count) {
      
      diagram[[i]]$Arguments$DoNotPlot=FALSE
      temp={
        qgraph::qgraph(diagram[[i]])
        title(paste0(input$group,"=",i))
      }
      result=list(result,temp)
    } 
   
    result
}


output$downloadPPT = downloadHandler(
  filename="R-sem.pptx",
  content=function(file){
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    
    mydoc=pptx(template="myppt.pptx")
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    mydoc=addSlide(mydoc,"Title Slide")
    mydoc=addTitle(mydoc,"Results of SEM")
    mydoc=addSubtitle(mydoc,"prepared by r-sem.com")
    
    #fit<-myfit()    
    diagram<-mysemPlot(DoNotPlot=TRUE)
    
    if(input$group=="") count=1
    else count=length(unique(df()[[input$group]]))
    if(count==1) {
        diagram$Arguments$DoNotPlot=FALSE
        mydoc=addqgraphPlot(mydoc,diagram)
    }  
    else for(i in 1:count) {
      diagram[[i]]$Arguments$DoNotPlot=FALSE
      #qgraph::qgraph(diagram[[i]])
      title=paste0(input$group,"=",i)
      mydoc=addqgraphPlot(mydoc,diagram[[i]],title=title)
      mydoc
    }  
    if(input$equation2!=""){
      diagram2<-mysemPlot2(DoNotPlot=TRUE)
      
      if(input$group=="") count=1
      else count=length(unique(df()[[input$group]]))
      if(count==1) {
        diagram2$Arguments$DoNotPlot=FALSE
        mydoc=addqgraphPlot(mydoc,diagram2)
      }  
      else for(i in 1:count) {
        diagram2[[i]]$Arguments$DoNotPlot=FALSE
        #qgraph::qgraph(diagram[[i]])
        title=paste0(input$group,"=",i)
        mydoc=addqgraphPlot(mydoc,diagram2[[i]],title=title)
        mydoc
      }    
    
    }
    writeDoc(mydoc,file=file)
    #zip(zipfile=file,files=c("R-Meta.pptx"))
  },
  contentType="application/vnd-ms-powerpoint"
)

myfigure=function(filename){
    if(input$plotformat=="png") png(filename,width=input$plotWidth,height=input$plotHeight)
    else if(input$plotformat=="svg") svg(filename,width=input$plotWidth,height=input$plotHeight)
    else pdf(filename,width=input$plotWidth,height=input$plotHeight)
}

myfigurename=function(i=1){
   result=paste0("plot_",i,".",input$plotformat)
   result
}

myeval=function(temp){
   eval(parse(text=temp))
}


output$downloadPlot = downloadHandler(
  filename="R-sem.zip",
  content=function(file){
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
    fs=c()
     
   fit<-myfit()    
   diagram<-mysemPlot(TRUE)
   if(input$group=="") count=1
    else count=length(unique(df()[[input$group]]))
   
    plotwidth=ifelse(input$plotformat=="png",input$plotWidth*300,input$plotWidth)
    plotheight=ifelse(input$plotformat=="png",input$plotHeight*300,input$plotHeight)
    
    if(count==1) {
       
       temp=paste0("semplot.",input$plotformat)
       temporder=paste0(input$plotformat,"('",temp,"',width=",plotwidth,
                        ",height=",plotheight,")")
       eval(parse(text=temporder))
       diagram$Arguments$DoNotPlot=FALSE
       qgraph::qgraph(diagram)
      dev.off()
      fs=c(fs,temp)
    }    
    else lapply(1:count,function(k) {
        temp=paste0("semplot",k,".",input$plotformat)
       temporder=paste0(input$plotformat,"('",temp,"',width=",plotwidth,
                               ",height=",plotheight,")")
        #putmsg(temporder)
           eval(parse(text=temporder))
  
        diagram[[k]]$Arguments$DoNotPlot=FALSE
        qgraph::qgraph(diagram[[k]])
        title(paste0(input$group,"=",k))
        dev.off()
        fs<<-c(fs,temp)
    })
   if(input$equation2!=""){
     diagram2<-mysemPlot2(DoNotPlot=TRUE)
     
     if(input$group=="") count=1
     else count=length(unique(df()[[input$group]]))
     if(count==1) {
       temp=paste0("semplot2.",input$plotformat)
       temporder=paste0(input$plotformat,"('",temp,"',width=",plotwidth,
                        ",height=",plotheight,")")
       eval(parse(text=temporder))
       diagram2$Arguments$DoNotPlot=FALSE
       qgraph::qgraph(diagram2)
       dev.off()
       fs=c(fs,temp)
     }  
     else lapply(1:count,function(k) {
       temp=paste0("semplot",k+count,".",input$plotformat)
       temporder=paste0(input$plotformat,"('",temp,"',width=",plotwidth,
                        ",height=",plotheight,")")
       #putmsg(temporder)
       eval(parse(text=temporder))
       diagram2[[k]]$Arguments$DoNotPlot=FALSE
       qgraph::qgraph(diagram2[[k]])
       title(paste0(input$group,"=",k))
       dev.off()
       fs<<-c(fs,temp)
     })
   }
    zip(zipfile=file, files=fs)
  },
  contentType="application/zip"
)

output$result0=renderPrint({
  cath("Path Diagram")
  
})

Sobel <- function (a, b, sa, sb) {
  
  cat("\n## Sobel test for the significance of a mediation effect\n\n")
  cat("\na=",a)
  cat("\nb=",b)
  cat("\nsa=",sa)
  cat("\nsb=",sb,"\n\n")
  
  tmp1 = b^2 * sa^2 + a^2 * sb^2
  tmp2 = sa^2 * sb^2
  zsob = a * b/sqrt(tmp1)
  psob = pnorm(-abs(zsob)) * 2
  zaro = a * b/sqrt(tmp1 + tmp2)
  paro = pnorm(-abs(zaro)) * 2
  if (tmp1 > tmp2) {
    zgm = a * b/sqrt(tmp1 - tmp2)
    pgm = pnorm(-abs(zgm)) * 2
  }
  else {
    zgm = NA
    pgm = NA
  }
  p.value = c(psob, paro, pgm)
  z.value = c(zsob, zaro, zgm)
  out = data.frame(rbind(z.value, p.value))
  names(out) = c("Sobel", "Aroian", "Goodman")
  result=t(out)
  data.frame(result)
}

})


