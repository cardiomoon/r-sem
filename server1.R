library(shiny)
require(moonBook)
require(ztable)
require(mycor)
require(ggplot2)
require(plyr)
require(rmarkdown)
require(readxl)
require(ggthemes)
require(reshape)
require(metafor)
require(meta)
require(rhandsontable)
require(shinyjs)
require(ReporteRs)

source("cleaning.R")



load("translation.bin") # contains the dictionary, parsed as a double list


options(ztable.type="html")
options(shiny.maxRequestSize=30*1024^2)

dic=read.csv("dictionary.csv",fileEncoding="utf-8",stringsAsFactors = FALSE)

shinyServer(function(input, output,session) {
    
    onclick("toggleAdvanced", toggle(id = "advanced", anim = TRUE))
    
    output$bgvalue<- renderText(input$bg)
    tr <- function(text){ # translates text into current language
        sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
    }
    
    tr2out=function(string,size=3){
        output[[string]]=renderPrint(cath(tr(string),size))
    }
    
    md2html=function(string){
        HTML(markdownToHTML(fragment.only=TRUE,text=string))
    }    
    
    mypptx=function(){
        file.copy(from="mywideppt.pptx",to="temp.pptx")
        mydoc=pptx(template="temp.pptx")
        file.remove("temp.pptx")
        mydoc
    }
    
    observe({
        
#         if(ready()==1) shinyjs::show("myOutput")
#         else shinyjs::hide("myOutput")
        
        if(input$language=="kor") {
            text("Title1","<h1>웹에서 하는 R 메타분석</h1>")
            text("introduction",md2html("메타 분석을 합니다. 먼저 자료의 유형에 따라 `효과 크기`를 계산하고 고정효과/랜덤효과 모형을 이용하여 `통합 추정치`를 추정하고 forest plot을 그립니다. `하위군분석`, `메타 ANOVA`, `메타회귀분석` 등이 가능하고 `출판편향분석`, `누적메타분석`, `민감도 분석` 등을 클릭만으로 할 수 있습니다. 보고서를 html 또는 PDF 형식으로 다운로드 할 수 있으며 출판 가능한 그림을 원하는 크기와 해상도로 다운로드할 수 있습니다.
                                        Plot은 forest plot 이외에도 radial plot, qqplot, L'Abbe plot, funnel plot, baujat plot 등을 그릴 수 있습니다."))
        }    
        else {
            text("Title1","<h1>Web-based Meta-Analysis with R</h1>")
            text("introduction",md2html("With this app, you can perform `meta-analysis`. You can calculate the `effect size`, estimate the weighted average using fixed-effects and/or random-effects models, test heterogeneity and draw the forest-plots. Additionally, you can perform `subgroup analysis`, `meta-ANOVA`, `meta-regression`, tests of `funnel plot asymmetry`, `cumulative meta-analysis` and `sensitivity test` with just one click. You can download the report with html or PDF format. You can also download the high-quality plots with desired size and resolution."))
        }    
        
        headings=dic$key[dic$class=="output"]
        for(i in 2:length(headings)) tr2out(headings[i])
        
        checks=dic$key[dic$class=="Checkbox"]
        for(i in 1:length(checks)) updateCheckboxInput(session,checks[i],label=tr(checks[i]))
        
        selects=dic$key[dic$class=="Select"]
        for(i in 1:length(selects)) updateSelectInput(session,selects[i],label=tr(selects[i]))
        
        texts=dic$key[dic$class=="Text"]
        for(i in 1:length(texts)) updateSelectInput(session,texts[i],label=tr(texts[i]))
        
        radios=dic$key[dic$class=="Radio"]
        for(i in 1:length(radios)) updateRadioButtons(session,radios[i],label=tr(radios[i]))
        
        numerics=dic$key[dic$class=="Numeric"]
        for(i in 1:length(numerics)) updateNumericInput(session,numerics[i],label=tr(numerics[i]))
        
        
        updateRadioButtons(session,'language',label=tr('language'))
        #updateSelectInput(session,'metadigits',label=tr('metadigits'))
        
        if(input$language=="en"){
            updateRadioButtons(session,"metaradio", label = "",
                               choices = list("Continuous Data - Two Groups"=1, 
                                              "Continuous Data - One Group(Before-After)"=2, 
                                              "Binary Data"=3," Binary Data2"=6,"Correlation Data"=4,
                                              "Effect size Data"=5,"None of above"=7),
                               selected=input$metaradio)
            updateRadioButtons(session,"metaExample", label = "Select Data",
                         choices = list("2 groups"="two_group", 
                                        "mentoring (2 groups)"="mentoring", 
                                        "cbt (2 groups)"="cbt",
                                        "one_group"="one_groupe",
                                        "binary"="binary",
                                        "scared (binary)"="scared",
                                        "bcg (binary2)"="bcg",
                                        "Correlation Data"="cor",
                                        "smoking (effect size)"="smoking",
                                        "Gilbody (effect size)"="Gilbody",
                                        "uploaded_file"="uploaded_file"),
                         selected = input$metaExample)
        }    
        else {
            
         
            updateRadioButtons(session,"metaradio", label = "",
                                choices = list("연속형자료-두집단"=1, "연속형자료-단일집단(사전-사후)"=2, 
                                               "이분형데이터"=3,"이분형데이터2"=6,"상관관계데이터"=4,
                                               "효과크기데이터"=5,"메타분석 자료 아님"=7),
                                selected=input$metaradio)
            updateRadioButtons(session,"metaExample", label = "데이터 선택",
                               choices = list("연속형자료-두집단"="two_group", 
                                              "연속형-두집단(mentoring)"="mentoring", 
                                              "연속형-두집단(cbt)"="cbt",
                                              "연속형자료-단일집단(사전-사후)"="one_group",
                                              "이분형자료"="binary",
                                              "이분형(scared)"="scared",
                                              "이분형2(bcg)"="bcg",
                                              "상관관계데이터"="cor",
                                              "효과크기데이터(smoking)"="smoking",
                                              "효과크기데이터(Gilbody)"="Gilbody",
                                              "업로드된 데이터"="uploaded_file"),
                               selected = input$metaExample)
        }  
    })
    
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
                       units=input$plotUnit,res=input$plotRes,start=0){
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
                       units=input$plotUnit,res=input$plotRes,start=0){
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
    
    plotSVG=function(fun,file,width=7,height=7,ggplot=FALSE){
        
        if(ggplot) ggsave(file,fun(),width=width,height=height)
        else {
            svg(file,width=width,height=height)
            #pdf(file,paper="letter")
            fun()
            dev.off()
        }
        
    }   

    sectionReport=function(reportname){
        downloadHandler(
        filename = function() {
            paste(reportname, sep = '.', switch(
                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
        },
        
        content = function(file) {
            src <- normalizePath('SectionReport.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'SectionReport.Rmd')
            
            out <- render('SectionReport.Rmd', switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
      )
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
    
   output$MetaReport=sectionReport2("MetaReport")
   

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
       
       if(input$metaExample=="uploaded_file") {
           if(is.null(input$file)) {
               if(input$language=="en") putmsg("Please upload file first !")
               else putmsg("먼저 file을 업로드하세요 !")   
               updateRadioButtons(session,"metaExample",selected="two_group")
               df<-read.csv("data/two_group.csv")
           }            
           else df<-my_readfile(input$file) 
       }           
       else df<-read.csv(paste("data/",input$metaExample,".csv",sep=""),stringsAsFactors = FALSE)

       values$choice=input$metaExample
       values$new<-TRUE

       df
   })
   
   data=reactive({
       temp<-input$metaExample
       if(is.null(input$hot)) DF=origin()
       else {
           if((current=="")|(current==input$metaExample)){
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
   
   output$text1=renderPrint({
       
       cat("current=",current,"\n")  
 
       cat("origin()\n")
       print(origin())
       cat("data()\n")
       print(data())
       cat("identical?=",identical(origin(),data()))
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
  
tableresult=reactive({
    result=0
    if(input$group1!="None") result=1
    if(input$allvar==TRUE) result=result+1
    if(length(input$add>0)) result=result+1
    result 
})



checkallassign=function(variables){
    sum=0
    for(i in 1:length(variables)) {
        if(input[[variables[i]]]=="") sum=sum+1
    }
    if(sum==0) result=1
    else result=0
    result
}



ready=reactive({
    
    result=0
    if(input$metaradio!=7) {
        if(input$metaradio==1) variables=c("ne","meane","sde","nc","meanc","sdc","sm","studlab1")
        else if(input$metaradio==2) variables=c("n","meanpre","sdpre","meanpost","sdpost","cor","sm2","studlab2")
        else if(input$metaradio==3) variables=c("evente","n3","eventc","n4","method3","sm3","studlab3")
        else if(input$metaradio==4) variables=c("cor1","n5","sm4","studlab4")
        else if(input$metaradio==5) variables=c("TE","varTE","sm5","studlab5")
        else variables=c("tpos","tneg","cpos","cneg","method6","sm6","studlab6")
        result=checkallassign(variables)
    }
    result    
})

observe({
    toggleState(id='doMeta',condition=ready()==1)
    toggleState(id='MetaReport',condition=ready()==1)
    toggleState(id='downloadMetaPlot',condition=ready()==1)
    toggleState(id='downloadPPT',condition=ready()==1)
})

observe({         
    
    df=df()
    
    contvar=ContinuousVar(df)
    # For Meta-analysis
    
    updateSelectInput(session,"ne",choices=contvar,selected="n1")
    updateSelectInput(session,"meane",choices=contvar,selected="m1")
    updateSelectInput(session,"sde",choices=contvar,selected="s1")
    updateSelectInput(session,"nc",choices=contvar,selected="n2")
    updateSelectInput(session,"n",choices=contvar,selected="n")
    updateSelectInput(session,"n3",choices=contvar,selected="n1")
    updateSelectInput(session,"n4",choices=contvar,selected="n2")
    updateSelectInput(session,"n5",choices=contvar,selected="n")
    updateSelectInput(session,"meanc",choices=contvar,selected="m2")
    updateSelectInput(session,"sdc",choices=contvar,selected="s2")
    updateSelectInput(session,"meanpre",choices=contvar,selected="mean_pre")
    updateSelectInput(session,"sdpre",choices=contvar,selected="sd_pre")
    updateSelectInput(session,"meanpost",choices=contvar,selected="mean_post")
    updateSelectInput(session,"sdpost",choices=contvar,,selected="sd_post")
    updateSelectInput(session,"cor",choices=contvar,selected="cor")
    updateSelectInput(session,"cor1",choices=contvar,selected="r")
    updateSelectInput(session,"evente",choices=contvar,selected="a")
    updateSelectInput(session,"eventc",choices=contvar,selected="c")
    updateSelectInput(session,"TE",choices=contvar,selected="yi")
    updateSelectInput(session,"varTE",choices=contvar,selected="vi")
    updateSelectInput(session,"studlab1",choices=colnames(df),selected=input$studlab1)
    updateSelectInput(session,"studlab2",choices=colnames(df),selected=input$studlab2)
    updateSelectInput(session,"studlab3",choices=colnames(df),selected=input$studlab3)
    updateSelectInput(session,"studlab4",choices=colnames(df),selected=input$studlab4)
    updateSelectInput(session,"studlab5",choices=colnames(df),selected=input$studlab5)
    updateSelectInput(session,"studlab6",choices=colnames(df),selected=input$studlab6)
    updateSelectInput(session,"tpos",choices=colnames(df),selected="tpos")
    updateSelectInput(session,"tneg",choices=colnames(df),selected="tneg")
    updateSelectInput(session,"cpos",choices=colnames(df),selected="cpos")
    updateSelectInput(session,"cneg",choices=colnames(df),selected="cneg")
    updateSelectInput(session,"metasubgrouptext",choices=colnames(df),selected=input$metasubgrouptext)
    updateSelectInput(session,"metaanovatext",choices=colnames(df),selected=input$metaanovatext)
    
    updateSelectInput(session,"metaregVars",choices=colnames(df),selected="")
    updateSelectInput(session,"metacumsortvar",choices=colnames(df),selected="")
    updateSelectInput(session,"metainfsortvar",choices=colnames(df),selected="")
    if(input$metaradio!=7){
        labelstudychoice=df()[[eval(parse(text=paste("input$studlab",input$metaradio,sep="")))]]
        updateSelectInput(session,"labelstudy",choices=labelstudychoice)
    } 
    if(input$language=="kor") {
        text(id="doMeta",text="  메 타 분 석  ")
        text(id="exportCSV",text="데이터를 CSV로 다운로드")
        text(id="MetaReport",text="보고서 다운로드")
        text(id="downloadMetaPlot",text="Plot 다운로드")
        text(id = "toggleAdvanced", text="기타 옵션 보이기/숨기기")
        text(id = "output", text="점 배경색:")

    } else {
        text(id="doMeta",text="  Meta-Analysis  ")
        text(id="exportCSV",text="Export Data as CSV")
        text(id="MetaReport",text="download Report")
        text(id="downloadMetaPlot",text="download Plots")
        text(id = "toggleAdvanced", text="Show/hide Plot Options")
        text(id = "output", text="background color of point:")

    }    
})

  result=0
       
  
   
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
   
  
output$intro2=renderPrint({
 
    if(input$language=="kor")
        md2html("통계결과 plot을 `png형식`으로 내려받을 때 크기를 정합니다. 그림은 하나의 `zip`화일(`MetaPlot.zip`)로 압축되어 다운로드됩니다.")
    else md2html("Select desired size of plots. Plots are made as `png format` and compressed to one `zip` file (`MetaPlot.zip`).")
})   

output$intro=renderPrint({
    
    if(input$language=="kor")
        md2html("메타 분석을 합니다. 먼저 자료의 유형에 따라 `효과 크기`를 계산하고 고정효과/랜덤효과 모형을 이용하여 `통합 추정치`를 추정하고 forest plot을 그립니다. `하위군분석`, `메타 ANOVA`, `메타회귀분석` 등이 가능하고 `출판편향분석`, `누적메타분석`, `민감도 분석` 등을 클릭만으로 할 수 있습니다. 보고서를 html 또는 PDF 형식으로 다운로드 할 수 있으며 출판 가능한 그림을 원하는 크기와 해상도로 다운로드할 수 있습니다. ")
    else md2html("With this app, you can perform `meta-analysis`. You can calculate the `effect size`, estimate the weighted average using fixed-effects and/or random-effects models, test heterogeneity and draw the forest-plots. Additionally, you can perform `subgroup analysis`, `meta-ANOVA`, `meta-regression`, tests of `funnel plot asymmetry`, `cumulative meta-analysis` and `sensitivity test` with just one click. You can download the report with html or PDF format. You can also download the high-quality plots with desired size and resolution.  ")
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
    cat("2. meta 패키지의 인용정보\n")
    print(citation("meta"))
    cat("3. metafor 패키지의 인용정보\n")
    print(citation("metafor"))
    } else{
        cat(paste("In your 'Method' section, please citate R as 'Analyses were performed using ",version$version.string," statistical software.' You can find informations about R version and sessionInfo as followings.\n",sep=""))
        cat("\n\nR version\n\n")
        print(version)
        cat("\n\nsessionInfo() of r-meta.com\n\n")
        print(sessionInfo())
        cat("\nReferences\n\n")
        cat("1. To citate R:\n")
        print(citation())
        cat("2. To citate package 'meta'\n")
        print(citation("meta"))
        cat("3. To citate package 'metafor'\n")
        print(citation("metafor"))
    }
    
    
})

metadata=reactive({
    
    if(input$metaradio==1) temp=paste("metacont(",input$ne,",",input$meane,",",input$sde,",",
                                      input$nc,",",input$meanc,",",input$sdc,", studlab=",input$studlab1,
                                      ",sm='",input$sm,"',comb.fixed=input$fixed,comb.random=input$random",
                                      sep="")
    else if(input$metaradio==2) {
        df=df()
        sd_pre=df[[input$sdpre]]
        sd_post=df[[input$sdpost]]
        cor=df[[input$cor]]
        mean_pre=df[[input$meanpre]]
        mean_post=df[[input$meanpost]]
        n=df[[input$n]]
        study=df[[input$studlab2]]
        
        sdiff=sqrt(sd_pre^2+sd_post^2-2*cor*sd_pre*sd_post)
        sp=sdiff/sqrt(2*(1-cor))
        d=(mean_post-mean_pre)/sp
        degf=n-1
        J=1-(3/(4*degf-1))
        g=J*d
        SEdiff=sdiff/sqrt(n)
        Vd=(1/n+(d^2)/(2*n))*2*(1-cor)
        SEd=sqrt(Vd)
        Vg=(J^2)*Vd
        
        one_group=data.frame(study,g,Vg)
        one_group=cbind(df,one_group)
        if(input$metatext2=="") meta=metagen(g,sqrt(Vg),studlab=study,
                     sm=paste("'",input$sm2,"'",sep=""),comb.fixed=input$fixed,comb.random=input$random,
                     data=one_group)
        else meta=eval(parse(text=paste("metagen(g,sqrt(Vg),studlab=study,sm='",
                                        input$sm2,"',comb.fixed=input$fixed,comb.random=input$random,
                                        data=one_group,",input$metatext2,")",sep="")))
    }
    else if(input$metaradio==3) temp=paste("metabin(",input$evente,",",input$n3,",",input$eventc,",",
                                           input$n4,",studlab=",input$studlab3,",sm='",input$sm3,"'",
                                           ",method='",input$method3,"',comb.fixed=input$fixed,comb.random=input$random",
                                           sep="")
    else if(input$metaradio==4) temp=paste("metacor(",input$cor1,",",input$n5,
                                           ",studlab=",input$studlab4,",sm='",input$sm4,"',comb.fixed=input$fixed,comb.random=input$random",
                                            sep="")    
    else if(input$metaradio==5) temp=paste("metagen(",input$TE,",sqrt(",input$varTE,")",
                                           ",studlab=",input$studlab5,",sm='",input$sm5,"',comb.fixed=input$fixed,comb.random=input$random,",
                                           sep="")
    else if(input$metaradio==6) {
        
        temp=paste("metabin(",input$tpos,",",input$tpos,"+",input$tneg,",",input$cpos,",",
                   input$cpos,"+",input$cneg,",studlab=",input$studlab6,",sm='",input$sm6,"'",
                   ",method='",input$method6,"',comb.fixed=input$fixed,comb.random=input$random",
                   sep="")    
    }
    
    if(input$metaradio==2) {
        meta
    } else{
        metatext=eval(parse(text=paste("input$metatext",input$metaradio,sep="")))
        if(metatext!="") temp=paste(temp,",",metatext,sep="")
        temp=paste(temp,",data=df())",sep="")
        meta=eval(parse(text=temp))
        meta
    }
})


fheight=function(){
    klength=metadata()$k
    if(klength<15) fheight=400
    else fheight=400+(klength-15)*10
    fheight
}

fheight2=function(){
    klength=metadata()$k
    if(klength<15) fheight=700
    else fheight=700+(klength-15)*10
    fheight
}


# for meta 

metaregtext=reactive({
    regTemp=""
    if(!is.null(input$metaregVars)) {
        regTemp=paste("~",input$metaregVars[1],sep="")
        if(length(input$metaregVars)>1) {
            for(i in 2:length(input$metaregVars))
                regTemp=paste(regTemp,input$metaregVars[i],sep="+")
        }
        
    }
    regTemp
})


bgcolor=function(){
    temp=input$bg
    temp
}
    
metaregvarcount=reactive({
    count=0
    if(!is.null(input$metaregVars)) count=length(input$metaregVars)
    count
})

output$metaText=renderPrint({
 
             
             
             if(input$metaradio!=7) {
                 meta=metadata()
                 cat("## Result of Meta-Analysis ##\n\n") 
                 meta
             }
   
})


output$metaText7=renderPrint({
  
        if(input$metaradio!=7 & input$metasubgroup) {
            meta=metadata()
            meta1=eval(parse(text=paste("update(meta,byvar=",input$metasubgrouptext,")",sep="")))
            cat("## Result of Subgroup Analysis ##\n\n") 
            meta1
        }
   
})

output$metaText8=renderPrint({
  
        
        if(input$metaradio!=7 & input$metaanova) {
            meta=metadata()
            meta1=eval(parse(text=paste("update(meta,byvar=",input$metaanovatext,
                                        ",tau.common=TRUE)",sep="")))
            cat("## Result of Meta ANOVA ##\n\n") 
            meta1
        }
 
})



output$meta.ui1=renderUI({
   
    
    input$doMeta
    
    isolate({
        if(ready()){
        klength=metadata()$k
        tagList(
             verbatimTextOutput('metaText'),
             if(input$forest){
                 plotOutput("metaPlot",height=fheight())
                 
             }
        )
        }
    })
 
})



output$meta.ui2=renderUI({
    input$doMeta
    
    isolate({
        if(ready() &input$metareg){
            tagList(
            verbatimTextOutput('metaText2'),
            plotOutput("bubblePlot",width=700,height=500)
            )
        }
    })
})

output$meta.ui3=renderUI({
    input$doMeta
    
    isolate({
        if(ready() & input$metapubbias){
            tagList(
            verbatimTextOutput('metaText3'),
            plotOutput("funnelPlot",height=500)
            )
        }
    })
})

output$meta.ui4=renderUI({
    input$doMeta
    
    isolate({
        if((input$metaradio!=7)& input$metapubbias) {
            tagList(
                verbatimTextOutput('metaText4'),
                plotOutput("funnelPlot2",height=500)
            )    
        }
    })
})

output$meta.ui5=renderUI({
    input$doMeta
    
    isolate({
        if((input$metaradio!=7)& input$metacum){
            
            tagList(
                verbatimTextOutput('metaText5'),
                #if(klength<15) plotOutput("forestPlot2")
                #else plotOutput("forestPlot2",height=700)
                plotOutput("forestPlot2",,height=fheight())
            )
        }
    })
})

output$meta.ui6=renderUI({
    input$doMeta
    
    isolate({
        if((input$metaradio!=7)& input$metainf){
            
            tagList(
                verbatimTextOutput('metaText6'),
                #if(klength<15) plotOutput("forestPlot3")
                #else plotOutput("forestPlot3",height=700)
                plotOutput("forestPlot3",height=fheight())
            )
        }
    })
})

output$meta.ui7=renderUI({
    input$doMeta
    
    isolate({
        if((input$metaradio!=7)& input$metainf & input$baujat){
            plotOutput("baujatPlot",height=500)
            
        }
    })
})

output$meta.ui8=renderUI({
    input$doMeta
    
    isolate({
        if(((input$metaradio==3)|(input$metaradio==6))& input$labbe){
            plotOutput("labbePlot",height=700,width=700)
            
        }
    })
})

output$meta.ui9=renderUI({
    input$doMeta
    
    isolate({
        if(input$metaradio!=7 & input$metasubgroup){
            tagList(
              verbatimTextOutput('metaText7'),
              plotOutput("metaSubgroupPlot",height=fheight2())
            )
        }
    })
})

output$meta.ui10=renderUI({
    input$doMeta
    
    isolate({
        if(input$metaradio!=7 & input$metaanova){
            tagList(
               verbatimTextOutput('metaText8'),
               plotOutput("metaAnovaPlot",height=fheight2())
            )
        }
    })
})


output$meta.ui11=renderUI({
    input$doMeta
    
    isolate({
        
            tagList(
                if(input$metaradio!=7 & input$metaradial) plotOutput("radialPlot"),
                if(input$metaradio!=7 & input$metaqqnorm) plotOutput("qqnormPlot")
            )
        
    })
})

output$meta.ui12=renderUI({
    input$doMeta
    
    isolate({
        
        if(ready()&input$influential) {
            tagList(
                htmlOutput("inftext"),
                verbatimTextOutput('infText'),
                htmlOutput("infplot"),
                plotOutput("infPlot",height=900),
                htmlOutput("infplot2"),
                plotOutput("infPlot2")
            
            )
        }
        
    })
})

output$meta.ui13=renderUI({
    input$doMeta
    
    isolate({
        
        if(ready() & input$random & input$blup) {
            tagList(
                htmlOutput("bluptext"),
                verbatimTextOutput('blupText'),
                htmlOutput("blupplot"),
                plotOutput("blupPlot",height=600)
                
            )
        }
        
    })
})

output$bluptext=renderPrint({
    cath("Best Linear Unbiased Predictions(BLUPs)")
})

output$blupText=renderPrint({
    meta=metadata()
    res=rma(TE,(seTE)^2,data=meta)
    #print(meta$TE)
    #print((meta$seTE)^2)
    print(blup(res))
    #cat("\nmean=",mean(blup(res)$pred))
    
})    

output$blupplot=renderPrint({
    cath("Plot showing BLUPs")
})

output$blupPlot=renderPlot({
     subblupPlot()  
})

subblupPlot=function(){
    meta=metadata()
    res=rma(TE,(seTE)^2,data=meta)
    blup(res)
    #res <- rma(yi, vi, data=dat)
    blups <- blup(res)$pred
    
    wi <- 1/sqrt(meta$seTE)
    size <- 0.5 + 3 * (wi - min(wi))/(max(wi) - min(wi))
    
    count=length(blups)
    
#     plotdata=data.frame(study=1:count,name=meta$studlab,size=size,y1=meta$TE,y2=blups)
#     plotdata1=melt(plotdata,id=1:3)
#     p=ggplot(data=plotdata1,aes(x=variable,y=value,size=size,group=study,colour=factor(study)))
#     p=p+geom_point()+geom_line(size=0.2)+ylab("Log Relative Risk")+xlab("")
#     p=p+geom_text(data=plotdata1[plotdata1$variable=="y1",],
#                   aes(x=as.numeric(factor(variable))-0.02,label=study,size=3),hjust=1)
#     p=p+geom_text(data=plotdata1[plotdata1$variable=="y2",],
#                   aes(x=as.numeric(factor(variable))+1.02,label=name,size=3),hjust=0)
#     p=p+scale_x_discrete(breaks=c("y1","y2"),labels=c("Observed\nValues","BLUPs"))
#     p=p+theme_bw()+guides(size=FALSE,colour=FALSE)
    
    plotdata=data.frame(study=1:count,name=meta$studlab,size=size,
                        x1=rep(1,count),y1=meta$TE,x2=rep(2,count),y2=blups)
    p=ggplot(data=plotdata,aes(x=x1,y=y1,fill=factor(study),size=size))
    p=p+geom_segment(aes(xend=x2,yend=y2,size=0.2,colour=factor(study)),data=plotdata)
    p=p+geom_point(pch=21)+ylab("Log Relative Risk")+xlab("")
    p=p+geom_point(aes(x=x2,y=y2),pch=21,data=plotdata)
    p=p+scale_x_continuous(breaks=c(1,2),labels=c("Observed\nValues","BLUPs"),limits=c(0.6,2.4))
    p=p+theme_bw()+guides(size=FALSE,colour=FALSE,fill=FALSE)
    
   
    #p=p+annotate("text",1.5, max(plotdata$y1),label= hat(mu) ==mean(plotdata$y2), size=5,colour="darkred")
    if(input$showlab) select=1:length(meta$studlab)
    else select=findainb(meta$studlab,input$labelstudy)
    if(length(select>0)){
        p=p+geom_text(data=plotdata[select,],
                      aes(x=0.98,y=y1,label=study,size=3),hjust=1)
        p=p+geom_text(data=plotdata[select,],
               aes(x=2.02,y=y2,label=name,size=3),hjust=0)
    }
    print(p)
    
    
    
    ##blup plots
    
#     plot(NA, NA, xlim=c(.8,2.4), ylim=c(-2,0.5), pch=19,
#          xaxt="n", bty="n", xlab="", ylab="Log Relative Risk")
#     segments(rep(1,13), meta$TE, rep(2,13), blups, col="darkgray")
#     points(rep(1,13), meta$TE, pch=19,cex=size)
#     points(rep(2,13), blups, pch=19)
#     axis(side=1, at=c(1,2), labels=c("Observed\nValues", "BLUPs"), lwd=0)
#     segments(.7, res$b, 2.15, res$b, lty="dotted")
#     graphics::text(2.3, res$b, expression(hat(mu)==-0.71), cex=1)
#     graphics::text(rep(0.95,13), meta$TE,1:13)
#     graphics::text(rep(2.05,13), blups, 1:13)
}

getrma=function(){
    meta=metadata()
    metaregvars=metaregtext()
    if(metaregvars!="") {
        temp=paste("metareg(meta,",metaregvars,")",sep="")
        #cat("result=",temp,"\n")
        result=eval(parse(text=temp))
        res=eval(parse(text=paste("rma(result$yi,result$vi,mods=",
                                  metaregvars,",data=meta$data)",sep="")))
    } else {
        res=rma(TE,(seTE)^2,data=meta)
    }
    res
}

output$inftext=renderPrint({
    cath("Influential Analysis")
    
})
output$infplot=renderPrint({
    cath("Influential Diagnositics")
    
})

output$infplot2=renderPrint({
    cath("Plot DFBETAS")
})

output$infText=renderPrint({
    res=getrma()
    inf=influence(res)
    inf
})

output$infPlot=renderPlot({
    subinfPlot()
})

subinfPlot=function(){
    res=getrma()
    inf=influence(res)
    plot(inf)
}

output$infPlot2=renderPlot({
    subinfPlot2()
})

subinfPlot2=function(){
    res=getrma()
    inf=influence(res)
    plot(inf,FALSE,TRUE)
}


output$radialPlot=renderPlot({
    
    plotradialPlot()
})


plotradialPlot=function(){
    dat=metadata()
    result=0
    if(input$fixed==TRUE) result=result+1
    if(input$random==TRUE) result=result+2
    if(result==3) par(mfrow=c(1,2))
    if(result%%2==1){
        res=rma(TE,(seTE)^2,data=dat,method="FE")
        radresult=radial(res,main="Fixed-Effects Model",pch=21,bg=input$bg)
        addtext(radresult$x,radresult$y,dat$studlab)
        
    }
    if(result>1){
        res=rma(TE,(seTE)^2,data=dat,method="REML")
        radresult2=radial(res,main="Random-Effects Model",pch=21,bg=input$bg)
        addtext(radresult2$x,radresult2$y,dat$studlab)
    }
    if(result==3) par(mfrow=c(1,1))
    
}

output$qqnormPlot=renderPlot({
    
    plotqqnormPlot()
    
})

plotqqnormPlot=function(){
    dat=metadata()
    result=0
    count=0
    if(input$fixed==TRUE) {
        result=result+1
        count=count+1
    }    
    if(input$random==TRUE) {
        result=result+2
        count=count+1
    }    
    if(metaregtext()!="") {
        result=result+4
        count=count+1
    }    
    if(count>1) par(mfrow=c(1,count))
    if(result%%2==1){
        res=rma(TE,(seTE)^2,data=dat,method="FE")
        qqres=qqnorm(res,main="Fixed-Effects Model",pch=21,bg=input$bg)
        addtext(qqres$x,qqres$y,dat$studlab)
    }
    if(result>1){
        res=rma(TE,(seTE)^2,data=dat,method="REML")
        qqres2=qqnorm(res,main="Random-Effects Model",pch=21,bg=input$bg)
        addtext(qqres2$x,qqres2$y,dat$studlab)
    }
    if(result>=4){
        res=getrma()
        qqres3=qqnorm(res,main="Mixed-Effects Model",pch=21,bg=input$bg)
        addtext(qqres3$x,qqres3$y,dat$studlab)
    }    
    if(count>1) par(mfrow=c(1,1))
    
}

output$metaPlot=renderPlot({
    
        if(input$metaradio!=7){
            meta=metadata()
            if(input$foresttext=="") 
                forest(meta,comb.fixed=input$fixed,comb.random=input$random,digits=input$metadigits)
            else 
                eval(parse(text=paste("forest(meta,comb.fixed=input$fixed,comb.random=input$random,
                       digits=input$metadigits,",input$foresttext,")")))
        }
 
})

plotmetaPlot1=function(){
    if(input$metaradio!=7){
        meta=metadata()
        if(input$foresttext=="") 
            forest(meta,comb.fixed=input$fixed,comb.random=input$random,digits=input$metadigits)
        else 
            eval(parse(text=paste("forest(meta,comb.fixed=input$fixed,comb.random=input$random,
                                  digits=input$metadigits,",input$foresttext,")")))
    }
    
}

output$metaSubgroupPlot=renderPlot({
    input$doMeta
    
    isolate({
        if(input$metaradio!=7 & input$metasubgroup){
            meta=metadata()
            meta1=eval(parse(text=paste("update(meta,byvar=",input$metasubgrouptext,")",sep="")))
            
            if(input$foresttext=="") 
                forest(meta1,comb.fixed=input$fixed,comb.random=input$random,digits=input$metadigits)
            else 
                eval(parse(text=paste("forest(meta1,comb.fixed=input$fixed,comb.random=input$random,
                                      digits=input$metadigits,",input$foresttext,")")))
        }
    })
})

plotmetaSubgroupPlot=function(){
    if(input$metaradio!=7 & input$metasubgroup){
        meta=metadata()
        meta1=eval(parse(text=paste("update(meta,byvar=",input$metasubgrouptext,")",sep="")))
        
        if(input$foresttext=="") 
            forest(meta1,comb.fixed=input$fixed,comb.random=input$random,digits=input$metadigits)
        else 
            eval(parse(text=paste("forest(meta1,comb.fixed=input$fixed,comb.random=input$random,
                                  digits=input$metadigits,",input$foresttext,")")))
    }
    
}

output$metaAnovaPlot=renderPlot({
    input$doMeta
    
    isolate({
        if(input$metaradio!=7 & input$metaanova){
            meta=metadata()
            meta1=eval(parse(text=paste("update(meta,byvar=",input$metaanovatext,
                                        ",tau.common=TRUE)",sep="")))
            if(input$foresttext=="") 
                forest(meta1,comb.fixed=input$fixed,comb.random=input$random,digits=input$metadigits)
            else 
                eval(parse(text=paste("forest(meta1,comb.fixed=input$fixed,comb.random=input$random,
                                      digits=input$metadigits,",input$foresttext,")")))
        }
        })
    })

plotmetaAnovaPlot=function(){
    if(input$metaradio!=7 & input$metaanova){
        meta=metadata()
        meta1=eval(parse(text=paste("update(meta,byvar=",input$metaanovatext,
                                    ",tau.common=TRUE)",sep="")))
        if(input$foresttext=="") 
            forest(meta1,comb.fixed=input$fixed,comb.random=input$random,digits=input$metadigits)
        else 
            eval(parse(text=paste("forest(meta1,comb.fixed=input$fixed,comb.random=input$random,
                                  digits=input$metadigits,",input$foresttext,")")))
    }
    
    }


output$metaText2=renderPrint({

        
        if((input$metaradio!=7)&input$metareg & (metaregtext()!="")) {
            meta=metadata()
            
            metaregvars=metaregtext()
            cat("## Result of Meta-Regression ##\n\n") 
            temp=paste("metareg(meta,",metaregvars,")",sep="")
            #cat("call: ",temp,"\n")
            result=eval(parse(text=temp))
            print(result)
            
        }
   
})

output$bubblePlot=renderPlot({
    input$doMeta
    
    isolate({
       plotmetaPlot2()
    })
})

findainb=function(a,b){
    select=c()
    for(i in 1:length(a)) if(a[i] %in% b) select=c(select,i)
    select
}


myregplot=function(){
   
    p=NULL
    meta=metadata()
    metaregvars=metaregtext()
    temp=paste("metareg(meta,",metaregvars,")",sep="")
    #cat("result=",temp,"\n")
    result=eval(parse(text=temp))
    plotdata=data.frame(x=meta$data[[input$metaregVars[1]]],y=exp(result$yi),
                        label=meta$studlab,number=1:length(meta$studlab))
    res=eval(parse(text=paste("rma(result$yi,result$vi,mods=",
                              metaregvars,",data=meta$data,measure='RR')",sep="")))
    
    moderator=meta$data[[input$metaregVars[1]]]
  
    margin=(max(moderator)-min(moderator))/20
    min=min(moderator)#-margin
    max=max(moderator)#+margin
    interval=(max-min)/100
    newmods=seq(min,max,interval)
    preds <- predict(res, newmods , transf = exp)
    #browser()
    wi <- 1/sqrt(result$vi)
    size <- 0.5 + 3 * (wi - min(wi))/(max(wi) - min(wi))
    if(input$metaregplot==1){
        plot(moderator, exp(result$yi), pch = 21, cex = size, bg=input$bg,
             xlab = "Absolute Latitude", ylab = "Relative Risk", 
             las=1,bty="l",log="y")
        lines(newmods, preds$pred)
        lines(newmods, preds$ci.lb, lty = "dashed")
        lines(newmods, preds$ci.ub, lty = "dashed")
        abline(h = 1, lty = "dotted")
        addtext(moderator,exp(result$yi),meta$studlab)
#         select=findainb(meta$studlab,input$labelstudy)
#         if(length(select)>0) graphics::text(moderator[select], 
#                                exp(result$yi)[select],meta$studlab[select],pos=input$labelpos)
    } else{
        
        #plotdata=data.frame(x=moderator,y=exp(result$yi),label=meta$studlab)
        preddata=data.frame(x=newmods,y=preds$pred,ymin=preds$ci.lb,ymax=preds$ci.ub)
        
        p<-ggplot(aes(x=x,y=y),data=plotdata)+geom_point(size=size*4,pch=21,fill=input$bg)+
                    scale_y_log10(breaks=seq(round(min(plotdata$y),1),
                                     round(max(plotdata$y),1),input$metastep))+
            geom_line(aes(x=x,y=y),data=preddata)+
            geom_ribbon(aes(x=x,ymin=ymin,ymax=ymax),data=preddata,alpha=0.2)+
            geom_hline(yintercept=1,lty=2)+
            theme_bw()+
            ylab("Relative Risk")+xlab(input$metaregVars[1])  
            
            if(input$showlab) select=1:length(meta$studlab)
            else select=findainb(meta$studlab,input$labelstudy)
            if(input$shownumber) {
                if(input$labelpos==1) p<-p+geom_text(aes(label=number,y=y*0.9),data=plotdata[select,])
                else if(input$labelpos==3) p<-p+geom_text(aes(label=number,y=y*1.10),data=plotdata[select,])
                else if(input$labelpos==2) p<-p+geom_text(aes(label=number,x=x-1),hjust=1,data=plotdata[select,])
                else p<-p+geom_text(aes(label=number,x=x+1),hjust=0,data=plotdata[select,])
        
            } else {
                if(input$labelpos==1) p<-p+geom_text(aes(label=label,y=y*0.9),data=plotdata[select,])
                else if(input$labelpos==3) p<-p+geom_text(aes(label=label,y=y*1.10),data=plotdata[select,])
                else if(input$labelpos==2) p<-p+geom_text(aes(label=label,x=x-1),hjust=1,data=plotdata[select,])
                else p<-p+geom_text(aes(label=label,x=x+1),hjust=0,data=plotdata[select,])
                
            }
        print(p)      
        p
    }
    

}

plotmetaPlot2=function(){
    if((input$metaradio!=7)&input$metareg&(metaregtext()!="")){
        p=NULL
        if(metaregvarcount()==1){
            p=myregplot()
            
        }
        else {
            meta=metadata()
            metaregvars=metaregtext()
            temp=paste("metareg(meta,",metaregvars,")",sep="")
            #cat("result=",temp,"\n")
            result=eval(parse(text=temp))
            plotdata=data.frame(x=meta$data[[input$metaregVars[1]]],y=exp(result$yi),
                                label=meta$studlab)
            bubble(result,studlab=input$showlab,bg=bgcolor())
        }
        p
        
    }
}    

output$baujatPlot=renderPlot({
    input$doMeta
    
    isolate({
        plotmetaPlot7()
        
    })
})

plotmetaPlot7=function(){
    if((input$metaradio!=7)& input$metainf & input$baujat){
        meta=metadata()
        if(input$baujattext=="") result=baujat(meta,studlab=FALSE,bg=bgcolor())
        else result=eval(parse(text=paste("baujat(meta,studlab=FALSE,bg=bgcolor()",input$baujattext,")")))
        result
        addtext(result$x,result$y,meta$studlab)

    }
}    

output$labbePlot=renderPlot({
    input$doMeta
    
    isolate({
        if(((input$metaradio==3)|(input$metaradio==6)) & input$labbe){
            meta=metadata()
            df=df()
           if(input$metaradio==3) temp=paste("rma(measure='RR', ai=",input$evente,",bi=",input$n3,"-",input$evente,
                      ",ci=",input$eventc,", di=",input$n4,"-",input$eventc,",data=df())",sep="")
           else temp=paste("rma(measure='RR', ai=",input$tpos,",bi=",input$tneg,
                           ",ci=",input$cpos,", di=",input$cneg,",data=df())",sep="")    
           res=eval(parse(text=temp))
            
            if(input$transformation==1) {
                labbe(res,transf = exp,pch=21,bg=bgcolor())
                if(input$metaradio==3) addtext((df[[input$eventc]]/df[[input$n4]]),
                                               (df[[input$evente]]/df[[input$n3]]),df[[input$studlab3]])
                else addtext((df[[input$cpos]]/(df[[input$cpos]]+df[[input$cneg]])),
                             (df[[input$tpos]]/(df[[input$tpos]]+df[[input$tneg]])),
                             df[[input$studlab6]])
            }    
            else {
                result=labbe(res,pch=21,bg=bgcolor())
                if(input$metaradio==3) addtext(log(df[[input$eventc]]/df[[input$n4]]),
                                               log(df[[input$evente]]/df[[input$n3]]),df[[input$studlab3]])
                else addtext(log(df[[input$cpos]]/(df[[input$cpos]]+df[[input$cneg]])),
                             log(df[[input$tpos]]/(df[[input$tpos]]+df[[input$tneg]])),
                             df[[input$studlab6]])
                
            }    
            
  #           if(input$labbetext=="") result=labbe(meta,studlab=input$showlab,bg=bgcolor())
  #           else result=eval(parse(text=paste("labbe(meta,studlab=input$showlab,bg=bgcolor(),",input$labbetext,")")))
          
        }
    })
})

plotmetaPlot8=function(){
    if(((input$metaradio==3)|(input$metaradio==6))&input$labbe){
        meta=metadata()
        df=df()
        if(input$metaradio==3) temp=paste("rma(measure='RR', ai=",input$evente,",bi=",input$n3,"-",input$evente,
                                          ",ci=",input$eventc,", di=",input$n4,"-",input$eventc,",data=df())",sep="")
        else temp=paste("rma(measure='RR', ai=",input$tpos,",bi=",input$tneg,
                        ",ci=",input$cpos,", di=",input$cneg,",data=df())",sep="")    
        res=eval(parse(text=temp))
        
        if(input$transformation==1) {
            labbe(res,transf = exp,pch=21,bg=bgcolor())
            if(input$metaradio==3) addtext((df[[input$eventc]]/df[[input$n4]]),
                                           (df[[input$evente]]/df[[input$n3]]),df[[input$studlab3]])
            else addtext((df[[input$cpos]]/(df[[input$cpos]]+df[[input$cneg]])),
                         (df[[input$tpos]]/(df[[input$tpos]]+df[[input$tneg]])),
                         df[[input$studlab6]])
        }    
        else {
            result=labbe(res,pch=21,bg=bgcolor())
            if(input$metaradio==3) addtext(log(df[[input$eventc]]/df[[input$n4]]),
                                           log(df[[input$evente]]/df[[input$n3]]),df[[input$studlab3]])
            else addtext(log(df[[input$cpos]]/(df[[input$cpos]]+df[[input$cneg]])),
                         log(df[[input$tpos]]/(df[[input$tpos]]+df[[input$tneg]])),
                         df[[input$studlab6]])
            
        }    
        
    }
}    

output$metaText3=renderPrint({

        
        if((input$metaradio!=7)& input$metapubbias) {
            meta=metadata()
            cat("## Analysis of Publication Bias ##\n\n") 
            if(input$metabiastext=="") result=metabias(meta)
            else result=eval(parse(text=paste("metabias(meta,",input$metabiastext,")")))
            result
        }
   
})


addtext=function(x,y,label){
    #browser()
    
if(input$shownumber) mylabel=1:length(label)    
else mylabel=label
if(input$showlab) graphics::text(x,y,mylabel,pos=input$labelpos)

else {
    select=findainb(label,input$labelstudy)
    if(length(select)>0) graphics::text(x[select],y[select],mylabel[select],pos=input$labelpos)
}    
}
    


output$funnelPlot=renderPlot({
    input$doMeta
    
    isolate({
        plotmetaPlot3()
    })
})

plotmetaPlot3=function(){
    if((input$metaradio!=7)& input$metapubbias & input$funnel) {
        meta=metadata()
        count=0
        result=0
        if(input$fixed==TRUE) {
            result=result+1
            count=count+1
        }    
        if(input$random==TRUE) {
            result=result+2
            count=count+1
        }    
        if(metaregtext()!="") {
            result=result+4
            count=count+1
        }    
        
        if(count>1) par(mfrow=c(1,count))
        if(result%%2==1) {
            res=rma(TE,(seTE)^2,data=meta,method="FE")
            if(input$enhanced)
                metafor::funnel(res,main="Fixed-Effects Model",atransf=exp,level=c(90,95,99),shade=c("white","gray","darkgray"),
                                at=log(c(.25,.5,1,2,4)))
            else
                metafor::funnel(res,main="Fixed-Effects Model",
                                atransf=exp)
            addtext(meta$TE,sqrt(meta$seTE),meta$studlab) 
        }
        if((result%%4)>=2){
            res=rma(TE,(seTE)^2,data=meta)
            if(input$enhanced)
                metafor::funnel(res,main="Random-Effects Model",atransf=exp,level=c(90,95,99),shade=c("white","gray","darkgray"),
                                at=log(c(.25,.5,1,2,4)))
            else
                metafor::funnel(res,main="Random-Effects Model",
                                atransf=exp)
            addtext(meta$TE,sqrt(meta$seTE),meta$studlab) 
        }
        if(result>=4){
            res=getrma()
            if(input$enhanced)
                metafor::funnel(res,main="Mixed-Effects Model",atransf=exp,level=c(90,95,99),shade=c("white","gray","darkgray"),
                                at=log(c(.25,.5,1,2,4)))
            else
                metafor::funnel(res,main="Mixed-Effects Model",
                                atransf=exp)
            addtext(meta$TE,sqrt(meta$seTE),meta$studlab) 
        }
        
        if(count>1) par(mfrow=c(1,1))
        
        
    }
}    

output$metaText4=renderPrint({

        if((input$metaradio!=7)& input$metapubbias & input$fsn) {
            meta=metadata()
            cat("## Fail-safe N calculation ##\n\n") 
            
            print(fsn(TE,I(seTE^2),data=meta))
            print(fsn(TE,I(seTE^2),data=meta,type="Orwin"))
            print(fsn(TE,I(seTE^2),data=meta,type="Rosenberg"))
        }
        if((input$metaradio!=7)& input$metapubbias & input$trim) {
            meta=metadata()
            cat("\n\n## Result of Trim-and-Fill ##\n\n")
            cat("1. Before Trim-and-Fill\n\n") 
            print(summary(meta))
            if(input$trimtext=="") result=trimfill(meta,comb.fixed=TRUE)
            else result=eval(parse(text=paste("trimfill(meta,",input$trimtext,",comb.fixed=TRUE)")))
            cat("\n\n2. After Trim-and-Fill \n\n") 
            print(summary(result))
        }
   
})

output$funnelPlot2=renderPlot({
    input$doMeta
    
    isolate({
        
            plotmetaPlot4()
        
    })
})

plotmetaPlot4=function(){
    if((input$metaradio!=7)& input$metapubbias & input$trim) {
        meta=metadata()

            result=rma(TE,(seTE)^2,data=meta,method="FE")
            if(input$trimtext=="") res=metafor::trimfill(result)
            else res=eval(parse(text=paste("metafor::trimfill(result,",input$trimtext,")")))
            if(input$enhanced)
                metafor::funnel(res,main="Adjusted Funnel Plot",atransf=exp,level=c(90,95,99),shade=c("white","gray","darkgray"),
                                at=log(c(.25,.5,1,2,4)))
            else
                metafor::funnel(res,main="Adjusted Funnel Plot",
                                atransf=exp,at=log(c(.25,.5,1,2,4)))
            
            count=length(meta$studlab)
            mylab=c(meta$studlab,res$slab[count:length(res$slab)])
            addtext(res$yi,sqrt(res$vi),mylab) 
            
        
#         if(input$showlab) {
#             count=length(meta$studlab)
#             mylab=c(meta$studlab,res$slab[count:length(res$slab)])
#             graphics::text(res$yi,sqrt(res$vi),mylab,pos=input$labelpos)
#         }
#         else {
#             select=findainb(meta$studlab,input$labelstudy)
#             if(length(select)>0) graphics::text(res$yi[select],sqrt(res$vi[select]),meta$studlab[select],pos=input$labelpos)
#         }    
    }
}

output$metaText5=renderPrint({

        
        if((input$metaradio!=7)& input$metacum) {
            meta=metadata()
            cat("## Cumulative Meta-Analysis ##\n\n") 
            temp=paste("metacum(meta,pooled='",input$metacumpooled,"'",sep="")
            if(input$metacumsortvar!="") temp=paste(temp,",sortvar=",input$metacumsortvar,sep="")
            temp=paste(temp,")",sep="")
            result=eval(parse(text=temp))
            cat("Call: ", temp,"\n")
            print(result)
            cat("\n\n## forest plot\n\nCall: forest(", temp,")\n")
        }
 
})

output$forestPlot2=renderPlot({
    input$doMeta
    
    isolate({
        
        if((input$metaradio!=7)& input$metacum) {
            meta=metadata()
            temp=paste("metacum(meta,pooled='",input$metacumpooled,"'",sep="")
            if(input$metacumsortvar!="") temp=paste(temp,",sortvar=",input$metacumsortvar,sep="")
            temp=paste(temp,")",sep="")
            result=eval(parse(text=temp))
            forest(result,digits=input$metadigits)
        }
    })
})

plotmetaPlot5=function(){
    if((input$metaradio!=7)& input$metacum) {
        meta=metadata()
        temp=paste("metacum(meta,pooled='",input$metacumpooled,"'",sep="")
        if(input$metacumsortvar!="") temp=paste(temp,",sortvar=",input$metacumsortvar,sep="")
        temp=paste(temp,")",sep="")
        result=eval(parse(text=temp))
        forest(result,digits=input$metadigits)
    }
}    
output$metaText6=renderPrint({

        if((input$metaradio!=7)& input$metainf) {
            meta=metadata()
            cat("## Sensitivity Analysis ##\n\n") 
            temp=paste("metainf(meta,pooled='",input$metainfpooled,"'",sep="")
            if(input$metainfsortvar!="") temp=paste(temp,",sortvar=",input$metainfsortvar,sep="")
            temp=paste(temp,")",sep="")
            result=eval(parse(text=temp))
            cat("Call: ", temp,"\n")
            print(result)
            cat("\n\n## forest plot\n\nCall: forest(", temp,")\n")
            
        }

})

output$forestPlot3=renderPlot({
    input$doMeta
    
    isolate({
        plotmetaPlot6()
        
    })
})

plotmetaPlot6=function(){
    if((input$metaradio!=7)& input$metainf) {
        meta=metadata()
        temp=paste("metainf(meta,pooled='",input$metainfpooled,"'",sep="")
        if(input$metainfsortvar!="") temp=paste(temp,",sortvar=",input$metainfsortvar,sep="")
        temp=paste(temp,")",sep="")
        result=eval(parse(text=temp))
        forest(result,digits=input$metadigits)
    }
}    

output$downloadMetaPlot = downloadHandler(
    filename="MetaPlot.zip",
    content=function(file){
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        plotlist=list()
        plotlist2=list()
        plotlist3=list()
        
        if(input$metaradio!=7) {
            if(input$forest) plotlist=c(plotlist,plotmetaPlot1)
            if(input$metasubgroup) plotlist=c(plotlist,plotmetaSubgroupPlot)
            if(input$metaanova) plotlist=c(plotlist,plotmetaAnovaPlot)
            
            
            
            if(input$metareg) plotlist2=c(plotlist2,plotmetaPlot2)
            if(input$random & input$blup) plotlist2=c(plotlist2,subblupPlot)
            
            if(((input$metaradio==3)|(input$metaradio==6))&input$labbe) plotlist2=c(plotlist2,plotmetaPlot8)
            if(input$metapubbias & input$funnel) plotlist2=c(plotlist2,plotmetaPlot3)
            if(input$metapubbias & input$trim) plotlist2=c(plotlist2,plotmetaPlot4)
            
            if(input$metacum) plotlist=c(plotlist,plotmetaPlot5)
            if(input$metainf) plotlist=c(plotlist,plotmetaPlot6)
            if(input$influential) {
                plotlist2=c(plotlist2,subinfPlot)
                plotlist2=c(plotlist2,subinfPlot2)
            }

            
            if(input$metainf & input$baujat) plotlist2=c(plotlist2,plotmetaPlot7)
            if(input$fixed & input$random){
                if(input$metaradial) plotlist=c(plotlist,plotradialPlot)
                if(input$metaqqnorm) plotlist=c(plotlist,plotqqnormPlot)
            } else {
                if(input$metaradial) plotlist=c(plotlist2,plotradialPlot)
                if(input$metaqqnorm) plotlist=c(plotlist2,plotqqnormPlot)
            }
            
        }
        
        if(input$plotformat=="png"){
          fs1=myplotPNG(plotlist,width=input$forestplotWidth,height=input$forestplotHeight,start=9)
          fs2=myplotPNG(plotlist2)
          
        } else if(input$plotformat=="svg") {
            fs1=myplotSVG(plotlist,width=input$forestplotWidth,height=input$forestplotHeight,start=9)
            fs2=myplotSVG(plotlist2)
            
        } else {
            fs1=myplotPDF(plotlist,width=input$forestplotWidth,height=input$forestplotHeight,start=9)
            fs2=myplotPDF(plotlist2)
            
        }
        fs=c(fs2,fs1)
        #if(input$metareg & (input$metaregplot==2) & (metaregvarcount()==1)) fs=c(fs,"bubbleplot.png")
        zip(zipfile=file, files=fs)
    },
    contentType="application/zip"
)

output$about=renderUI({
    
    if(input$language=="en") includeMarkdown("aboute.md")
    else includeMarkdown("about.md")
})

output$Howto1=renderUI({
    if(input$language=="en") includeMarkdown("Howto1.md")
    else includeMarkdown("Howto1k.md")
    
})

output$Howto2=renderUI({
    if(input$language=="en") includeMarkdown("Howto2.md")
    else includeMarkdown("Howto2k.md")
})
output$Howto3=renderUI({
    if(input$language=="en") includeMarkdown("Howto3.md")
    else includeMarkdown("Howto3k.md")
})
output$Howto4=renderUI({
    if(input$language=="en") includeMarkdown("Howto4.md")
    else includeMarkdown("Howto4k.md")
})


addplot=function(mydoc,plotfunction,title=""){
    mydoc=addSlide(mydoc,"Title and Content")
    mydoc=addTitle(mydoc,title)
    mydoc=addPlot(mydoc,plotfunction,vector.graphic=input$vector)
    mydoc
}

addTableSlide=function(mydoc,data,title="",vanilla=FALSE){
    mydoc = addSlide( mydoc, slide.layout = "Title and Content" )
    mydoc = addTitle( mydoc, title )
    mydoc = addFlexTable(mydoc,data2FTable(data,vanilla=vanilla,add.rownames=input$add.rownames))  
    mydoc
}

data2FTable=function(data,vanilla=FALSE,add.rownames=FALSE){
    
    if(vanilla==TRUE) {
        MyFTable=vanilla.table(data,add.rownames=add.rownames)
    } else {
        # Create a FlexTable with data.frame mtcars, display rownames
        # use different formatting properties for header and body
        MyFTable = FlexTable( data = data, add.rownames = add.rownames, 
                              header.cell.props = cellProperties( background.color = "#00557F" ), 
                              header.text.props = textProperties( color = "white", 
                                                                  font.size = 11, font.weight = "bold" ), 
                              body.text.props = textProperties( font.size = 10 )
        )
        # zebra stripes - alternate colored backgrounds on table rows
        MyFTable = setZebraStyle( MyFTable, odd = "#E1EEf4", even = "white" )
        
        # applies a border grid on table
        MyFTable = setFlexTableBorders(MyFTable,
                                       inner.vertical = borderProperties( color="#0070A8", style="solid" ),
                                       inner.horizontal = borderNone(),
                                       outer.vertical = borderProperties( color = "#006699", 
                                                                          style = "solid", width = 2 ),
                                       outer.horizontal = borderProperties( color = "#006699", 
                                                                            style = "solid", width = 2 )
        )
    }
    MyFTable
}

output$downloadPPT = downloadHandler(
    filename="R-Meta.pptx",
    content=function(file){
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        
        mydoc=pptx()
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        mydoc=addSlide(mydoc,"Title Slide")
        mydoc=addTitle(mydoc,"Results of Meta-Analysis")
        mydoc=addSubtitle(mydoc,"prepared by r-meta.com")
        mydoc=addTableSlide(mydoc,df(),title="Raw Data")
        #mydoc=addTableSlide(mydoc,df(),title="Raw Data",vanilla=TRUE)
        
        if(input$metaradio!=7) {
            if(input$forest) mydoc=addplot(mydoc,plotmetaPlot1,"Forest Plot")
            if(input$metaradial) mydoc=addplot(mydoc,plotradialPlot,"Radial Plot")
            if(input$metaqqnorm) mydoc=addplot(mydoc,plotqqnormPlot,"qqnormal Plot")
            if(input$metasubgroup) mydoc=addplot(mydoc,plotmetaSubgroupPlot,"Subgroup Analysis")
            if(input$metaanova) mydoc=addplot(mydoc,plotmetaAnovaPlot,"Results of Meta-ANOVA")
            
            if(((input$metaradio==3)|(input$metaradio==6))&input$labbe) mydoc=addplot(mydoc,plotmetaPlot8,"L'Abbe plot")
            
            if(input$metareg) {
#                 if((input$metaregplot==2) & (metaregvarcount()==1)) {
#     
#                     mydoc=addplot(mydoc,myregplot,"Meta-Regression Plot")
#                 }
#                 else
                    mydoc=addplot(mydoc,plotmetaPlot2,"Meta-Regression Plot")
            }
            if(input$influential) {
                mydoc=addplot(mydoc,subinfPlot,"Influential Diagnostics Plot")
                mydoc=addplot(mydoc,subinfPlot2,"Plot DFBETAS")
            }
            if(input$random & input$blup) mydoc=addplot(mydoc,subblupPlot,"Best Linear Unbiased Predictions(BLUPs)")
            if(input$metapubbias & input$funnel) mydoc=addplot(mydoc,plotmetaPlot3,"Funnel Plot")
            if(input$metapubbias & input$trim) mydoc=addplot(mydoc,plotmetaPlot4,"Adjusted Funnel Plot")
            
            if(input$metacum) mydoc=addplot(mydoc,plotmetaPlot5,"Cumulative Meta-Analysis")
            if(input$metainf) mydoc=addplot(mydoc,plotmetaPlot6,"Influential Analysis")
            
            if(input$metainf & input$baujat) mydoc=addplot(mydoc,plotmetaPlot7,"Baujat Plot")
            
    
            
        }
        writeDoc(mydoc,file=file)
        #zip(zipfile=file,files=c("R-Meta.pptx"))
    },
    contentType="application/vnd-ms-powerpoint"
)


})
