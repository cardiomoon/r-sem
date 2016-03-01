# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(markdown)
library(shinythemes)
require(shinyjs)
require(rhandsontable)

source("textInput2.R")

shinyUI(fluidPage(
    useShinyjs(),
    tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      }
    "))
    ),
    div(id="Title1",h1("Web-based Meta-Analysis with R")),
    hr(),
    div(id="introduction","With this app, you can perform `meta-analysis`. Please wait a minute ! This message is disappeared WHEN READY ! "),
    hr(),
    radioButtons(inputId = "language", label = "Select Language",
                 choices = list("English" = "en", "한국어(Korean)" = "kor"),
                 selected = "en",inline=TRUE),
    singleton(
        tags$head(tags$script(src = "message-handler.js"))
    ),
    
        navbarPage( "R-meta.com",
            tabPanel("Meta-Analysis",
                    
                     fluidPage(
                         
                         fluidRow(
                             column(4,htmlOutput("selectData"),
                                    
                                    wellPanel(
                                 fileInput("file", label = "upload data(*.xlsx or *.csv)"),
                                 radioButtons("metaExample", label = "Select Data",
                                              choices = list("Continuous Data - 2 Groups(1)"="two_group", 
                                                             "Continuous Data - 2 Groups(mentoring)"="mentoring", 
                                                             "Continuous Data - 2 Groups(cbt)"="cbt",
                                                             "Continuous Data - One Group(Before-After)"="one_group",
                                                             "Binary Data"="binary",
                                                             "Binary Data(scared)"="scared",
                                                             "Binary Data(bcg11)"="bcg11",
                                                             "Correlation Data"="cor",
                                                             "Effect Size Data(smoking)"="smoking",
                                                             "Effect Size Data(Gilbody)"="Gilbody",
                                                             "uploaded_file"),
                                              selected = "two_group")
                             )),
                             column(8, htmlOutput("dataTable"),
                                    
                                 #tableOutput('table2')
                                 rHandsontableOutput('hot',height=350),
                             
                                 
                                 downloadButton('exportCSV','Export to CSV')
                                 
                                 #,verbatimTextOutput('text1'),
                                 #tableOutput('table1')
                             )
                         ),
                         fluidRow(
                           
                             column(4,
                                    htmlOutput("dataFormat"),
                                    wellPanel(
                                        radioButtons("metaradio", label = "",
                                                           choices = list("Continuous Data - Two Groups"=1, 
                                                                          "Continuous Data - One Group(Before-After)"=2, 
                                                                          "Binary Data"=3,"Binary Data2"=6,"Correlation Data"=4,
                                                                          "Effect size Data"=5,"None of above"=7),
                                                           selected=7)
                             )),
                             column(8,htmlOutput("variable"),
                                    
                                    wellPanel(
                                
                                 conditionalPanel(condition='input.metaradio==1',#연속형 두그룹
                                                  selectInput3('ne','실험군 n 수',"n1",selectize=TRUE,selected="n1",width=120),
                                                  selectInput3('meane','실험군 평균',"m1",selectize=TRUE,selected="m1",width=120),
                                                  selectInput3('sde','실험군 표준편차',"s1",selectize=TRUE,selected="s1",width=120),
                                                  selectInput3('nc','대조군 n 수',"n2",selectize=TRUE,selected="n2",width=120),
                                                  selectInput3('meanc','대조군 평균',"m2",selectize=TRUE,selected="m2",width=120),
                                                  selectInput3('sdc','대조군 표준편차',"s2",selectize=TRUE,selected="s2",width=120),
                                                  selectInput3('sm','요약측정',choices=c("MD","SMD"),selected="SMD",width=120),
                                                  selectInput3('studlab1','연구이름',"study",selected="study",width=120),
                                                  textInput3("metatext1",'기타옵션',value="",width=365)
                                 ),
                                 conditionalPanel(condition='input.metaradio==2', #단일그룹 사전/사후
                                                  selectInput3('n','n 수',"n",selectize=TRUE,selected="n",width=120),
                                                  selectInput3('meanpre','사전 평균',"mean_pre",selectize=TRUE,selected="mean_pre",width=120),
                                                  selectInput3('sdpre','사전 표준편차',"sd_pre",selectize=TRUE,selected="sd_pre",width=120),
                                                  selectInput3('meanpost','사후 평균',"mean_post",selectize=TRUE,selected="mean_post",width=120),
                                                  selectInput3('sdpost','사후 표준편차',"sd_post",selectize=TRUE,selected="sd_post",width=120),
                                                  selectInput3('cor','사전/사후 상관계수',"cor",selectize=TRUE,selected="cor",width=120),
                                                  selectInput3('sm2','요약측정',choices=c("MD","SMD"),selected="SMD",width=120),
                                                  selectInput3('studlab2','연구이름',"study",selected="study",width=120),
                                                  textInput3("metatext2",'기타옵션',value="",width=365)
                                 ),
                                 conditionalPanel(condition='input.metaradio==3',  #이분형데이타
                                                  selectInput3('evente','실험군 event수',"a",selectize=TRUE,selected="a",width=120),
                                                  selectInput3('n3','실험군 n 수',"n1",selectize=TRUE,selected="n1",width=120),
                                                  selectInput3('eventc','대조군 event수',"c",selectize=TRUE,selected="c",width=120),
                                                  selectInput3('n4','대조군 n 수',"n2",selectize=TRUE,selected="n2",width=120),
                                                  selectInput3('sm3','요약측정',choices=c("RR","OR","RD","ASD"),selected="OR",width=120),
                                                  selectInput3('method3','방법',choices=c("Inverse","MH","Peto"),selected="Inverse",width=120),
                                                  selectInput3('studlab3','연구이름',"study",selected="study",width=120),
                                                  textInput3("metatext3",'기타옵션',value="",width=365)
                                 ),
                                 conditionalPanel(condition='input.metaradio==4',  #상관관계데이타
                                                  selectInput3('cor1','사전/사후 상관계수',"r",selectize=TRUE,selected="r",width=120),
                                                  selectInput3('n5','n 수',"n",selectize=TRUE,selected="n",width=120),
                                                  selectInput3('sm4','요약측정',choices=c("ZCOR","COR"),selected="ZCOR",width=120),
                                                  selectInput3('studlab4','연구이름',"study",selected="study",width=120),
                                                  textInput3("metatext4",'기타옵션',value="",width=365)
                                 ),
                                 conditionalPanel(condition='input.metaradio==5',  #효과크기 데이타
                                                  selectInput3('TE','효과 크기',"yi",selectize=TRUE,selected="yi",width=120),
                                                  selectInput3('varTE','분산',"vi",selectize=TRUE,selected="vi",width=120),
                                                  selectInput3('sm5','요약측정',choices=c("RD","RR","OR","ASD","MD","SMD"),selected="RR",width=120),
                                                  selectInput3('studlab5','연구이름',"study",selected="study",width=120),
                                                  textInput3("metatext5",'기타옵션',value="",width=365)
                                 ),
                                 conditionalPanel(condition='input.metaradio==6',  #이분형 데이타2
                                                  selectInput3('tpos','치료군양성',"tpos",selectize=TRUE,selected="tpos",width=120),
                                                  selectInput3('tneg','치료군음성',"tneg",selectize=TRUE,selected="tneg",width=120),
                                                  selectInput3('cpos','대조군양성',"cpos",selectize=TRUE,selected="cpos",width=120),
                                                  selectInput3('cneg','대조군음성',"cneg",selectize=TRUE,selected="cneg",width=120),
                                                  selectInput3('sm6','요약측정',choices=c("RR","OR","RD","ASD"),selected="RR",width=120),
                                                  selectInput3('method6','방법',choices=c("Inverse","MH","Peto"),selected="Inverse",width=120),
                                                  selectInput3('studlab6','연구이름',choices=c("study"),selected="study",width=120),
                                                  textInput3("metatext6",'기타옵션',value="",width=365)
                                 )
                                 ))),
                         fluidRow(column(4,
                                         htmlOutput('modelSelection'),
                                  
                                         wellPanel(checkboxInput("fixed",'고정효과모형',value=TRUE),
                                                   checkboxInput("random",'랜덤효과모형',value=TRUE)))
                                  ),
                         fluidRow(
                             column(4,
                                    htmlOutput("forestplotOption"),
                                 wellPanel(
                                 checkboxInput('forest',"Forest Plot",value=TRUE),
                                 numericInput3("metadigits",'소숫점아래 자리수',value=2,min=0,width=50),
                                 textInput3("foresttext",'기타옵션',value="",width=150),
                                 checkboxInput("metaradial",'Radial plot',value=TRUE),
                                 checkboxInput("metaqqnorm",'Q-Q normal plot',value=TRUE),
                                 a(id = "toggleAdvanced", "Show/hide Plot Options"),
                                 hidden(
                                     div(id="advanced",
                                 checkboxInput("showlab", "study label 모두 출력", value = FALSE),
                                 selectInput('labelstudy','Studies be labelled',choices="",
                                             multiple=TRUE,selectize=TRUE), 
                                 checkboxInput("shownumber", "study number로 출력", value = FALSE),
                                 radioButtons('labelpos','Label position',choices=c(1,2,3,4),selected=3,inline=TRUE),
                                 #selectInput("bg","점 배경색",choices=colors(),selected="magenta",width=150),
                                 div(id = "output", "점 배경색:",
                                     textOutput("bgvalue", inline = TRUE)),
                                 colourInput("bg",NULL,"magenta"),
                                 numericInput3("plotRes","Resolution",value=300,step=1,width=80),
                                 selectInput3("plotUnit","units",choices=c("in","cm","mm"),selected="in",width=70),
                                 div("Forest Plot",
                                     numericInput3("forestplotWidth","width",value=12,step=0.5,width=70),
                                     numericInput3("forestplotHeight","height",value=7,step=0.5,width=70)),
                                 div("Other Plots",numericInput3("plotWidth","width",value=7,step=0.5,width=70),
                                     numericInput3("plotHeight","height",value=5,step=0.5,width=70))
                                     ))
                                 
                                 )   
                                 ),
                             column(4,
                                    htmlOutput("additional1"),
                                 wellPanel(
                                 conditionalPanel(condition='input.metaradio==3 || input.metaradio==6',
                                         checkboxInput("labbe", "L'Abbe plot for binary outcome", value = TRUE),
                                         conditionalPanel(condition='input.labbe==true',
                                              radioButtons('transformation',"transformation",choice=list("none"=0,"exp"=1),selected=0,inline=TRUE),              
                                              textInput("labbetext","L'Abbe plot 옵션",value="")
                                         )
                                 ),
                                 checkboxInput("metasubgroup", "하위집단분석", value = FALSE),
                                 selectInput3("metasubgrouptext","그룹변수",choices="",width=150),
                                
                                 checkboxInput("metaanova", "메타ANOVA", value = FALSE),
                                 selectInput3("metaanovatext","그룹변수",choices="",width=150),
                              
                                 checkboxInput("metareg", "메타회귀분석", value = FALSE),
                                 selectInput3("metaregVars","조절변수(들)",choices="",
                                              selectize=TRUE,multiple=TRUE,width=150),
                                 conditionalPanel(condition='input.metareg==true',
                                       radioButtons("metaregplot", label = "plot종류",
                                                    choices = list("bubble"=1, "ggplot"=2),
                                                    selected = 2,inline=TRUE),
                                       
                                       numericInput3("metastep","RR표시간격",value=0.2)
                                 )
                                 )),
                             column(4,
                                    htmlOutput("additional2"),
                                    wellPanel(
                                 checkboxInput("influential", "영향치 진단", value = TRUE),  
                                 conditionalPanel(condition='input.random==true',
                                      checkboxInput("blup", "Best linear unbiased prediction", value = TRUE)
                                 ),  
                                 checkboxInput("metapubbias", "출간편향분석", value = FALSE),
                                 conditionalPanel(condition='input.metapubbias==true',
                                                  wellPanel(
                                                  textInput("metabiastext","metabias 옵션",value=""),
                                                  checkboxInput("funnel", "funnel plot", value = TRUE),
                                                  checkboxInput("enhanced", "contour-enhanced", value = TRUE),
                                                  #conditionalPanel(condition='input.funnel==true',
                                                  #    textInput("funneltext","funnel plot옵션",value="")
                                                  #),
                                                  checkboxInput("fsn", "fail safe N", value = TRUE),
                                                  checkboxInput("trim", "Trim-and-Fill", value = TRUE),
                                                  conditionalPanel(condition='input.trim==true',
                                                      textInput("trimtext","trim&fill 옵션",value="")
                                                  ),
                                                  checkboxInput("funnel2", "adjusted funnel plot", value = TRUE)
                                                  #,conditionalPanel(condition='input.funnel2==true',
                                                  #                 textInput("funneltext2","adjusted funnel plot옵션",value="")
                                                  #)
                                 )),
                                 checkboxInput("metacum", "누적메타분석", value = FALSE),
                                 conditionalPanel(condition='input.metacum==true',
                                     wellPanel(              
                                      radioButtons("metacumpooled", label = "pooled",
                                                               choices = list("fixed", "random"),
                                                               selected = "fixed",inline=TRUE),
                                      selectInput("metacumsortvar","정렬순서",choices="",selected="")
                                     ) 
                                      
                                 ),
                                 checkboxInput("metainf", "민감성분석", value = FALSE),
                                 conditionalPanel(condition='input.metainf==true',
                                                  wellPanel(              
                                                      radioButtons("metainfpooled", label = "pooled",
                                                                   choices = list("fixed", "random"),
                                                                   selected = "fixed",inline=TRUE),
                                                      selectInput("metainfsortvar","정렬순서",choices="",selected=""),
                                                      checkboxInput("baujat", "baujat plot : 이질성분석", value = TRUE),
                                                      conditionalPanel(condition='input.baujat==true',
                                                                       textInput("baujattext","baujat plot 옵션",value="")
                                                  ) 
                                                  
                                                  )
                                 )
                                 
                             ))
                         ),
                         hr(),
                  
                         fluidRow(
                             column(3,actionButton("doMeta","Meta-Analysis")),
                             column(3,
                                    downloadButton("MetaReport","download Report"),
                                    checkboxInput("rawdata","include raw data",value=TRUE),
                                    checkboxInput("includeResult","include analysis result",value=TRUE),
                                    radioButtons('format', 'Report Format As', c('PDF', 'HTML'),
                                                 inline = TRUE,selected='HTML')
                                    ),
                             column(3,downloadButton("downloadMetaPlot","download Plot(s)"),
                                    radioButtons('plotformat', 'Format As', c('png', 'svg','PDF'),
                                                 inline = TRUE,selected='png')
                                    ),
                             column(3,downloadButton("downloadPPT","download pptx"),
                                    checkboxInput("rawdata2","include raw data",value=TRUE),
                                    checkboxInput("add.rownames","include rownames",value=FALSE),
                                    #radioButtons('ppt', 'Select Powerpoint Format', c('normal(4:3)', 'wide screen'),
                                    #             inline = FALSE,selected='normal(4:3)'),
                                    radioButtons('vector', 'as editable vector graphic', c('yes'=TRUE, 'no'=FALSE),
                                                 inline = TRUE,selected=TRUE)
                             )
                             
                         ),
                         hr(),
                       
                         
                                 
                         #verbatimTextOutput('metaText'),
                         uiOutput("meta.ui1"),
                         uiOutput("meta.ui11"),
                         uiOutput("meta.ui8"),
                         #verbatimTextOutput('metaText7'),
                         uiOutput("meta.ui9"),
                         #plotOutput("metaSubgroupPlot",height="auto"),
                         #verbatimTextOutput('metaText8'),
                         uiOutput("meta.ui10"),
                         #plotOutput("metaAnovaPlot",height="auto"),
                         #verbatimTextOutput('metaText2'),
                         uiOutput("meta.ui2"),
                         #verbatimTextOutput('metaText3'),
                         uiOutput("meta.ui12"),    # influential analysis
                         uiOutput("meta.ui13"),    # blup
                         uiOutput("meta.ui3"),
                         #verbatimTextOutput('metaText4'),
                         uiOutput("meta.ui4"),
                         #verbatimTextOutput('metaText5'),
                         uiOutput("meta.ui5"),
                         #verbatimTextOutput('metaText6'),
                         uiOutput("meta.ui6"),
                         uiOutput("meta.ui7")
                             
                         )),
   
           
            navbarMenu("How to",
                         tabPanel("Getting Started",uiOutput("Howto1")),
                         tabPanel("Analyze Your Own Data",uiOutput("Howto2")),
                         tabPanel("A Complete Example",uiOutput("Howto3")),
                         tabPanel("Customize Your Plot",uiOutput("Howto4"))
                         ),
            tabPanel("Citation",
                     fluidPage(
                         htmlOutput('citation'),     
                         #HTML(markdownToHTML(fragment.only=TRUE,text=c("`논문 작성`시에는 다음과 같이 인용하세요."))),
                         hr(),
                         verbatimTextOutput('citation1')
                     )
            ),
            tabPanel("About",
                     uiOutput("about")
                     ),
            id='main',
            theme=shinytheme("united")
            
        )    
    )
  )              

