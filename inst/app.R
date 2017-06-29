#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyDebuggingPanel)
library(DUE)

ui <- fluidPage(
   
   # Application title
   titlePanel("Attempt 2: DUE Shiny"),
   shinyDebuggingPanel::withDebuggingPanel() ,
      mainPanel(
         plotOutput("linePlot")
      )
   )


server <- function(input, output) {
  shinyDebuggingPanel::makeDebuggingPanelOutput(session) 
   output$linePlot <- renderPlot({
  
    plotProbsAndEU()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

