# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(markdown)
library(shinythemes)
#require(shinyjs)
require(rhandsontable)

source("textInput2.R")

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      }
    "))
    ),
    div(id="Title1",h1("Structural Equation Modeling with R")),
    hr(),
    div(id="introduction","With this app, you can perform `structural equation modeling`. Please wait a minute ! This message is disappeared WHEN READY ! "),
    hr(),
    radioButtons(inputId = "language", label = "Select Language",
                 choices = list("English" = "en", "한국어(Korean)" = "kor"),
                 selected = "en",inline=TRUE),
    singleton(
        tags$head(tags$script(src = "message-handler.js"))
    ),
    
        navbarPage( "R-sem.com",
            tabPanel("SEM",
                    
                     fluidPage(
                         
                         fluidRow(
                             column(4,htmlOutput("selectData"),
                                    
                                    wellPanel(
                                 fileInput("file", label = "upload data(*.xlsx or *.csv)"),
                                 radioButtons("Example", label = "Select Data",
                                              choices = c("data.csv", 
                                                             "ex31.csv",
                                                             "ch9.csv",
                                                             "ch4.csv",
                                                             "ch10.csv",
                                                             "ADHD",
                                                          "HolzingerSwineford1939",
                                                          "PoliticalDemocracy",
                                                          "example1",
                                                          "example2",
                                                          "Demo.growth",
                                                             "uploaded_file"),
                                              selected = "data.csv"),
                                 selectInput("SelectEx",label="Select Example",
                                             choices=c("None"=0,"Path Analysis"=1,"Confirmatory Factor Analysis"=2,
                                               "Structural Equation Model"=3,"Cross-Validation Analysis"=4,
                                               "Mediation Effect Analysis"=5,"Mediation Effect Analysis2"=6,
                                               "Latent Growth Modeling"=7,
                                               "Partial Least Square"=8),
                                             selected=0),
                           
                                 actionButton("ResetEx","Reset")
                             )),
                             column(8, htmlOutput("dataTable"),
                                    
                                 #tableOutput('table2')
                                 rHandsontableOutput('hot',height=350),
                             
                                 hr(),
                                 actionButton("showHelpData", "show help Data"),
                                 downloadButton('exportCSV','Export to CSV'),
                                 bsModal("modalHelpData", "help Data", "showHelpData", size = "large",
                                         htmlOutput("dataHelp"))
                                 
                                 
                                 
                                 #,verbatimTextOutput('text1'),
                                 #tableOutput('table1')
                             )
                         ),
                         hr(),
                         fluidRow(
                           htmlOutput("EditSE")),
                         fluidRow(
                           column(6,verbatimTextOutput("EquationText"))
                         ),        
                         fluidRow(
                             
                             column(2,
                                    selectInput("leftvar","leftvar","",multiple=TRUE,selectize=TRUE),
                                    textInput("lefttext","Latent Variable",value="")),
                             column(2, wellPanel(radioButtons("operator","operator",
                                                    choices=c("=~","~","~~",":=","==","<",">"),
                                                    selected="=~"))),
                             column(2,selectInput("rightvar","rightvar","",multiple=TRUE,selectize=TRUE)),
                             column(2,
                                    actionButton("add","add to equation"),
                                    hr(),
                                    actionButton("reset","reset the equation")),
                             column(4,
                                    textareaInput("equation","equation",rows=8,width=300),
                                    a(id = "toggle2ndEquation", "The 2nd Equation"),
                                    shinyjs::hidden(
                                       div(id="2ndEquation",
                                           textareaInput("equation2","2nd equation",rows=8,width=300),
                                           checkboxInput("compareModels","Compare Models",value=TRUE)
                                           ))
                                    )         
                         ),
                         fluidRow(
                             checkboxInput("moderating","Mediation Effect Analysis"),
                             conditionalPanel(condition="input.moderating== true",
                                              column(4,wellPanel(
                                                  
                                                  selectInput("indepvar","independent variable",c(""),selected="",selectize=TRUE),
                                                  selectInput("mediator","mediator",c(""),selected="",selectize=TRUE),
                                                  selectInput("resvar","response variable",c(""),selected="",selectize=TRUE),
                                                  checkboxInput("showcor","show correlation",value=TRUE),
                                                  checkboxInput("sobel","perform sobel test",value=TRUE),
                                                  actionButton("MakeEquation","Make Equation")
                                              )))
                         ),
                         fluidRow(
                           htmlOutput("analysisOption"),
                           column(4,wellPanel(radioButtons("method","Anlaysis options",
                                                           choices=c("fit a Structural Equation Model"="sem",
                                                                     "fit a Confirmatory Factor Analysis Models"="cfa",
                                                                     "fit a Growth Curve Model"="growth",
                                                                     "fit a Partial Least Squares Model"="matrixpls"),
                                                           selected="sem"),
                                              h5("Edit Analysis Order"),
                                              checkboxInput("editAnalysis","Edit Analysis Order",value=FALSE))),
                           column(4,wellPanel(
                                              selectInput("group","group",c(""),selected=""),
                                              selectInput("group.equal","group.equal",
                                                          c("loadings","intercepts","means","thresholds","regressions","residuals",
                                                            "residual.covariances","lv.variances","lv.covariances"),multiple=TRUE,selected=""),
                                              selectInput("se","se",c("default","standard","first.order","robust","robust.sem",
                                                                      "robust.huber.white","bootstrap")))),
                           column(4,wellPanel(htmlOutput("summaryOption"),
                                              checkboxInput("standardized","standardized",value = TRUE),
                                              checkboxInput("fit.measures","fit.measures",value = FALSE),
                                              checkboxInput("rsquare","rsquare",value = FALSE),
                                              checkboxInput("modindices","modindicis",value = FALSE),
                                              h5("Others"),
                                              checkboxInput("showcoef","show coefficient",value = FALSE),
                                              checkboxInput("showMeaInv","show Measurement Invariance",value = FALSE)
                                              ))
                           
                         ),
                         fluidRow(
                           conditionalPanel(condition="input.editAnalysis== true",
                                    wellPanel(
                                    h4("Edit the Analysis Order"),  
                                    textareaInput("AnalysisOrder","Analysis Order",rows=2,width=800),
                                    
                                    conditionalPanel(condition="input.equation2!= ''",
                                                     h4("Edit the Analysis Order - The 2nd Equation"),
                                                     textareaInput("AnalysisOrder2","Analysis Order2",rows=2,width=800)))
                                    )
                         ),
                         #h3("Plot Options - semPaths()"),
                         fluidRow(
                           htmlOutput("plotOption"),
                              
#                               column(12,a(id = "toggleModelPlot", h4("Model Plot Options")),
#                               shinyjs::hidden(
#                                   div(id="modelPlot",
#                                       column(3, wellPanel(selectInput("what0","what",c("paths","standardized","eq","col"),
#                                                                       selected="paths"),
#                                                           selectInput("whatLabels0","whatLabels",c("","label","est","std","eq","omit","hide"),
#                                                                       selected="hide"),
#                                                           selectInput("style0","style",c("ram","mx","OpenMx","lisrel"),
#                                                                       selected="ram"),
#                                                           selectInput("layout0","layout",c("tree","circle","spring","tree2","circle2"),
#                                                                       selected="tree"))),
#                                       column(3,wellPanel(
#                                         checkboxInput("intercept0","intercept",value=FALSE),
#                                         checkboxInput("residuals0","residuals",value=FALSE),
#                                         checkboxInput("thresholds0","thresholds",value=FALSE),
#                                         checkboxInput("pastel0","pastel",value=TRUE),
#                                         checkboxInput("curvePivot0","curvePivot",value=FALSE)
#                                       )),
#                                       column(6,wellPanel(
#                                         numericInput3("nCharNodes0","nCharNodes",value=0,width=120),
#                                         numericInput3("nCharEdges0","nCharEdges",value=0,width=120),
#                                         selectInput3("rotation0","rotation",1:4,selected=1,width=120),
#                                         selectInput3("groups0","groups",c("","manifests","latents","manlat"),selected="manlat",width=120),
#                                         textInput("Other0","Other Options",value="")
#                                       ))
#                                       )
#                               )),
                              column(12,a(id = "toggleFinalPlot", h4("Plot Options")),
                                     shinyjs::hidden(
                                       div(id="finalPlot",
                              column(3, wellPanel(selectInput("what","what",c("paths","standardized","eq","col"),
                                          selected="paths"),
                              selectInput("whatLabels","whatLabels",c("","label","est","std","eq","omit","hide"),
                                          selected="std"),
                              selectInput("style","style",c("ram","mx","OpenMx","lisrel"),
                                          selected="ram"),
                              selectInput("layout","layout",c("tree","circle","spring","tree2","circle2"),
                                          selected="tree"))),
                              column(3,wellPanel(
                                checkboxInput("intercept","intercept",value=FALSE),
                                checkboxInput("residuals","residuals",value=FALSE),
                                checkboxInput("thresholds","thresholds",value=FALSE),
                                checkboxInput("pastel","pastel",value=FALSE),
                                checkboxInput("curvePivot","curvePivot",value=FALSE)
                              )),
                              column(6,wellPanel(
                                numericInput3("nCharNodes","nCharNodes",value=0,width=120),
                                numericInput3("nCharEdges","nCharEdges",value=0,width=120),
                                selectInput3("rotation","rotation",1:4,selected=1,width=120),
                                selectInput3("groups","groups",c("","manifests","latents","manlat"),selected="",width=120),
                                textInput("Other","Other Options",value="")
                              ))
                            
                              )))),
                         checkboxInput("preview","Plot Preview",value = FALSE),
                         uiOutput('Model.ui'),
                         hr(),
                         fluidRow(
                               column(3,actionButton("doSEM","Do Analysis")
                               ),
                               column(3,
                                      downloadButton("semReport","download Report"),
                                      radioButtons('format', 'Report Format As', c('PDF', 'HTML'),
                                                   inline = TRUE,selected='HTML')
                               ),
                               column(3,downloadButton("downloadPlot","download Plot(s)"),
                                      radioButtons('plotformat', 'Format As', c('png', 'svg','pdf'),
                                                   inline = TRUE,selected='png'),
                                      numericInput3("plotWidth","width",value=7,step=0.1,width=50),
                                      numericInput3("plotHeight","height",value=5,step=0.1,width=50)
                                     
                               ),
                               column(3,downloadButton("downloadPPT","download pptx"),
                                radioButtons('pptformat', 'Format', c('wide', 'normal'),
                                                   inline = TRUE,selected='normal'),
                                radioButtons('pptfigformat', 'Figure As', c('emf', 'png'),
                                                   inline = TRUE,selected='emf')
                               )
                               
                             ),
                             hr(),   
                         
                         uiOutput("sem.ui1")
                         )),
   

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

