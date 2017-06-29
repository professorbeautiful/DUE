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
library(shinyBS)

ui <- fluidPage(
  
  # Application title
  titlePanel("Attempt 2: DUE Shiny"),
  shinyDebuggingPanel::withDebuggingPanel() ,
  
    fluidRow(
      column(6, "insert graph here"), 
      column(6,
             plotOutput("linePlot")
      )), 
    fluidRow(
        column(6, "insert graph here"), 
        column(6,
              bsButton(inputId="Additive", "Additive"),
              bsButton(inputId="Simple", "Simple"),
              bsButton(inputId="Cautious", "Cautious"),
              bsButton(inputId="Aggressive", "Aggressive")
        ))
  )


server <- function(input, output) {
  DUEenv=reactiveValues()
  shinyDebuggingPanel::makeDebuggingPanelOutput(session) 
  
  output$linePlot <- renderPlot({
    
    plotProbsAndEUsimplified()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

