rm(list = ls())
library(shiny)
library(shinyBS)

shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(numericInput("n", "n", 50),actionButton("go", "Go")),
        mainPanel(
          bsModal("modalExample", "Your plot", "go", size = "large",
                  plotOutput("plot"),
                  downloadButton('downloadPlot', 'Download'))
        )
      )
    ),
  server =
    function(input, output, session) {
      
      randomVals <- eventReactive(input$go, {
        runif(input$n)
      })
      
      plotInput <- function(){hist(randomVals())}
      
      output$plot <- renderPlot({
        hist(randomVals())
      })
      
      output$downloadPlot <- downloadHandler(
        filename = "Shinyplot.png",
        content = function(file) {
          png(file)
          plotInput()
          dev.off()
        }) 
      
    }
)
