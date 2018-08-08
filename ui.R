# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(markdown)
library(shinythemes)
#require(shinyjs)
require(shinyBS)
require(shinyTree)
require(DT)
#require(shinyDND)

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
                             htmlOutput("selectData"),
                             column(4,
                                    fileInput("file", label = "upload data(*.xlsx or *.csv)")),
                             column(4,
                                 wellPanel(
                                    radioButtons("Example", label = "Select Data",
                                              choices = c("HolzingerSwineford1939",
                                                          "PoliticalDemocracy",
                                                          "example1",
                                                          "example2",
                                                          "ADHD",
                                                             "uploaded_file"),
                                              selected = "HolzingerSwineford1939"))),
                              column(4,   
                             textInput("mydata","Enter data name",value="HolzingerSwineford1939"),
                                 selectInput("SelectEx",label="Select Example",
                                             choices=c("None"=0,"Confirmatory Factor Analysis"=1,
                                               "Structural Equation Model"=2,"Cross-Validation Analysis"=3,
                                               "Mediation Effect Analysis"=4,
                                               "ADHD data"=6),
                                             selected=0),
                                 
                                 actionButton("ResetEx","Reset")
                             )),
                             htmlOutput("dataTable"),
                                    
                                 DT::DTOutput('x1'),
                                 #rHandsontableOutput('hot',height=350),
                                  
                                 hr(),
                                 downloadButton('exportCSV','Export to CSV'),
                                 checkboxInput("preprocessing","use preprocessing",value=TRUE),
                                 conditionalPanel(condition="input.preprocessing==true",
                                             uiOutput("Chooser")),
                                 
                                        
                             
                         
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
                         
                         # fluidRow(
                         #   h3("Edit mediation equation by drag and drop"),
                         #   p("Drag and drop the variable names and press the `make mediation equation` button"),
                         #   column(3,uiOutput("Drag")),
                         #   column(5, uiOutput("Drop")),
                         #   
                         #   column(4,          actionButton("MakeEquation2","make mediation equation"),
                         #          hr(),
                         #          actionButton("resetMediation2","reset mediation equation"),
                         #          hr(),
                         #          textareaInput("mediationEquation2","mediation Equation",rows=8,width=300),
                         #          actionButton("addMediationEquation2","add to the equation")
                         #   )       
                         # ),
                         # fluidRow(
                         #    h3("Edit mediation equation by drag and drop"),
                         #    shinyTree("tree",dragAndDrop=TRUE),
                         #    verbatimTextOutput("treestr")
                         # ),
                         fluidRow(
                           checkboxInput("moderating","Edit Mediation Effect by SelectInput"),
                           conditionalPanel(condition="input.moderating== true",
                                            column(4,wellPanel(
                                              
                                              selectInput("indepvar","independent variable",c(""),selected="",selectize=TRUE,multiple=TRUE),
                                              selectInput("mediator","mediator",c(""),selected="",selectize=TRUE,multiple=TRUE),
                                              selectInput("resvar","response variable",c(""),selected="",selectize=TRUE,multiple=TRUE),
                                              #checkboxInput("showcor","show correlation",value=TRUE),
                                              checkboxInput("sobel","perform sobel test",value=TRUE)
                                              
                                            )),
                                            column(3,actionButton("MakeEquation","make mediation Equation"),
                                                   hr(),
                                                   actionButton("resetMediation","reset mediation Equation")),
                                            column(5,wellPanel(textareaInput("mediationEquation","mediation Equation",rows=8,width=300),
                                                               actionButton("addMediationEquation","add to the equation")
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
                                              div(id="editOrder","Manual command line spec"),
                                              checkboxInput("editAnalysis","Manual command line specification",value=FALSE))),
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
                                              checkboxInput("modindices","modindices",value = FALSE),
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
                                           radioButtons("plotOption2","plot Option",choices=c("semPaths","mediationPlot"),selected="semPaths"),
                            conditionalPanel(condition="input.plotOption2=='semPaths'",  
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
                              ))),
                            conditionalPanel(condition="input.plotOption2=='mediationPlot'",  
                                 column(3, wellPanel( 
                                   selectInput("whatLabels2","whatLabels",c("std","est","name"),
                                                                             selectize=TRUE,selected="std"),
                                   numericInput("maxx","maxx",value=60),
                                   numericInput("maxy","maxy",value=30))),
                                 column(3, wellPanel( 
                                   numericInput("rectHeight","rect height",value=3),
                                   numericInput("rectWidth","rect width",value=8),
                                   numericInput("base_size","font size",value=5),
                                   selectInput("base_family","font family",
                                               c("Arial","Times","NanumGothic"),selected="NanumGothic")
                                   )),
                                 column(3, wellPanel( 
                                   checkboxInput("usecolor","usecolor",value=TRUE),
                                   checkboxInput("clean","clean theme",value=TRUE),
                                   checkboxInput("mediationOnly","mediationOnly",value=FALSE),
                                   checkboxInput("residuals2","residuals",value=FALSE),
                                   checkboxInput("regression","regression",value=TRUE),
                                   checkboxInput("indirect","indirect",value=FALSE),
                                   checkboxInput("secondIndirect","secondIndirect",value=FALSE)
                                 ))
                            )
                            
                              )))),
                         uiOutput("paraEst"),
                         checkboxInput("preview","Plot Preview",value = FALSE),
                         uiOutput('Model.ui'),
                         hr(),
                         
                         fluidRow(
                           htmlOutput("Tables"),
                           checkboxInput("showtable","show tables",value=FALSE),
                           conditionalPanel(condition="input.showtable== true",
                                          checkboxInput("vanilla","as vanilla table",value=FALSE),
                                          uiOutput("tableui")
                           )
                         ),
                         
                         fluidRow(
                           htmlOutput("inspect"),
                           p("You can inspect or extract information from a fitted lavaan object. Please select what needs to be inspect/extracted."),
                           column(4,wellPanel(
                             selectInput("matrices","Model matrices",choices=c("none","free","partable","se","start","est","dx.free","dx.all",
                                                                             "std","std.lv","std.nox")),
                           selectInput("data","data",c("none","data","group","ngroups","group.label","nobs",
                                                       "norig","ntotal","case.idx","empty.idx","patterns",
                                                       "coverage")),
                           selectInput("stats","observed sample statistics",c("none","sampstat","sampstat.h1","wls.obs","wls.v","gamma"))
                           )),
                           column(4,wellPanel(
                           selectInput("features","model features",c("none","meanstructure","categorical","fixed.x","parameterization")),
                           selectInput("samplestats","model-implied sample statistics",
                                       c("none","cov.lv","cor.lv","mean.lv","cov.ov","cor.ov","mean.ov",
                                         "cov.all","cor.all","th","wls.est","vy","rsquare")),
                           selectInput("optimizer","optimizer information",
                                       c("none","converged","iterations","optim"))
                           )),
                           column(4,wellPanel(
                           selectInput("infmatrices","Gradient, Hessian, observed, expected and first.order information matrices",
                                       c("none","gradient","hessian","information","information.expected","information.observed",
                                         "information.first.order","augumented.information","augmented.information.expected","augmented.information.observed",
                                         "augmented.information.first.order","inverted.information","inverted.information.expected","inverted.information.observed",
                                         "inverted.information.first.order")),
                           selectInput("varcovpar","Variance covariance matrix of the model parameters",
                                       c("none","vcov","vcov.std.all","vcov.std.lv",
                                         "vcov.std.nox")),
                           selectInput("misc","miscellaneous",
                                       c("none","UGamma","list","fit","mi","options","call",
                                         "timing","test","post.check"))
                           )),
                           column(3,actionButton("inspect","Inspect/extract inf")),
                           column(3,actionButton("resetinspect","Reset inspect"))
                           
                           
                         ),
                         hr(),
                         uiOutput("inspect.ui"),
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
                                      numericInput3("plotWidth","width",value=12,step=0.1,width=70),
                                      numericInput3("plotHeight","height",value=10,step=0.1,width=70)
                                     
                               ),
                               column(3,#actionButton("makePPT","make pptx"),
                                      downloadButton("downloadPPT","download pptx"),
                                      radioButtons('pptformat', 'Format', c('wide', 'normal'),
                                                   inline = TRUE,selected='normal')
                                      
                                
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

