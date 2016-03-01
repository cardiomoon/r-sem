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
require(shinyBS)

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
    radioButtons('language','Select Language',
                 choices=c('English' = 'en', '한국어(Korean)' = 'kor'),
                 selected = 'en',inline=TRUE),
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
                                              choices = c("HolzingerSwineford1939",
                                                          "PoliticalDemocracy",
                                                          "example1",
                                                          "example2",
                                                          "Demo.growth",
                                                          "ADHD",
                                                             "uploaded_file"),
                                              selected = "HolzingerSwineford1939"),
                                 selectInput("SelectEx",label="Select Example",
                                             choices=c("None"=0,"Confirmatory Factor Analysis"=1,
                                               "Structural Equation Model"=2,"Cross-Validation Analysis"=3,
                                               "Mediation Effect Analysis"=4,
                                               "Latent Growth Modeling"=5,
                                               "ADHD data"=6),
                                             selected=0),
                           
                                 actionButton("ResetEx","Reset")
                             )),
                             column(8, htmlOutput("dataTable"),
                                    
                                 #tableOutput('table2')
                                 rHandsontableOutput('hot',height=350),
                             
                                 hr(),
                                 downloadButton('exportCSV','Export to CSV')
                                        
                             )
                         ),
                         a(id = "toggleHelpData", "Help for Data show/hide"),
                         shinyjs::hidden(
                           div(id="helpData",
                               uiOutput("dataHelp")      
                           )),
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
                                                    selected="=~"),
                                                 a(id = "toggleHelpOp", "?Operator")
                                                 )),
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
                         shinyjs::hidden(
                           div(id="helpOperator",
                               uiOutput("operatorHelp")
                           )),
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
                                              div(id="editOrder","Edit Analysis Order"),
                                              checkboxInput("editAnalysis","Edit Analysis Order",value=FALSE))),
                           column(4,wellPanel(
                                              selectInput("group","group",c(""),selected=""),
                                              selectInput("group.equal","group.equal",
                                                          c("loadings","intercepts","means","thresholds","regressions","residuals",
                                                            "residual.covariances","lv.variances","lv.covariances"),multiple=TRUE,selected=""),
                                              selectInput("se","se",c("default","standard","first.order","robust","robust.sem",
                                                                      "robust.huber.white","bootstrap")),
                                              selectInput("missing","missing",c("default","listwise","direct","ml","fiml")),
                                              selectInput("estimator","estimator",c("default","ML","GLS","WLS","ULS","DWLS")))),
                           column(4,wellPanel(div(id="summaryOptions","Summary Option"),
                                              checkboxInput("standardized","standardized",value = TRUE),
                                              checkboxInput("fit.measures","fit.measures",value = FALSE),
                                              checkboxInput("rsquare","rsquare",value = FALSE),
                                              checkboxInput("modindices","modindicis",value = FALSE),
                                              div(id="otherOptions","Others"),
                                              checkboxInput("showcoef","show coefficient",value = FALSE),
                                              checkboxInput("showMeaInv","show Measurement Invariance",value = FALSE)
                                              ))
                           
                         ),
                         fluidRow(
                           conditionalPanel(condition="input.editAnalysis== true",
                                    wellPanel(
                                    div(id="order1","Edit the Analysis Order"),  
                                    textareaInput("AnalysisOrder","Analysis Order",rows=2,width=800),
                                    
                                    conditionalPanel(condition="input.equation2!= ''",
                                                     div(id="order2","Edit the Analysis Order - The 2nd Equation"),
                                                     textareaInput("AnalysisOrder2","Analysis Order2",rows=2,width=800)))
                                    )
                         ),
                         #h3("Plot Options - semPaths()"),
                         fluidRow(
                           htmlOutput("plotOption"),
                              

                              column(12,a(id = "toggleFinalPlot", h4("Plot Options show/hide")),
                                     shinyjs::hidden(
                                       div(id="finalPlot",
                              column(3, wellPanel(selectInput("what","what",c("path","est","std","eq","col"),
                                          multiple=TRUE,selectize=TRUE,selected="path"),
                              selectInput("whatLabels","whatLabels",c("label","est","std","eq","hide"),
                                          multiple=TRUE,selectize=TRUE,selected="std"),
                              selectInput("style","style",c("ram","mx","OpenMx","lisrel"),
                                          selected="ram"),
                              selectInput("layout","layout",c("tree","circle","spring","tree2","circle2"),
                                          selected="tree"))),
                              column(3,wellPanel(
                                checkboxInput("intercept","intercept",value=FALSE),
                                checkboxInput("residuals","residuals",value=FALSE),
                                checkboxInput("thresholds","thresholds",value=FALSE),
                                checkboxInput("pastel","pastel",value=TRUE),
                                checkboxInput("curvePivot","curvePivot",value=FALSE)
                              )),
                              column(6,wellPanel(
                                numericInput3("nCharNodes","nCharNodes",value=0,width=120),
                                numericInput3("nCharEdges","nCharEdges",value=0,width=120),
                                selectInput3("rotation","rotation",1:4,selected=1,width=120),
                                selectInput3("groups","groups",c("","manifests","latents","manlat"),selected="manlat",width=120),
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
                                      radioButtons('format', 'Report Format As', c('PDF', 'Word','HTML'),
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
                                checkboxInput('pptvector','as vector graphic',value=FALSE)
                                #,radioButtons('pptfigformat', 'Figure As', c('emf', 'png'),
                                #                   inline = TRUE,selected='emf')
                               )
                               
                             ),
                             hr(),   
                         
                         uiOutput("sem.ui1")
                         )),
            navbarMenu("How to",
                 tabPanel("Getting Started",uiOutput("Howto1")),
                 tabPanel("Analyze Your Own Data",uiOutput("Howto2")),
                 tabPanel("Edit A Structural Equation",uiOutput("Howto3")),
                 tabPanel("Insert A Mediation Effect Equation",uiOutput("Howto4")),
                 tabPanel("A Complete Example",uiOutput("Howto5")),
                 tabPanel("Customize Your Plot",uiOutput("Howto6")),
                 tabPanel("Multiple Groups",uiOutput("Howto7")),
                 tabPanel("Compare Two Models using Different Equation",uiOutput("Howto8"))
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

