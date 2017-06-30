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


server <- function(input, output, session) {
  source("plotProbsAndEUsimplified.R", local = TRUE)
  DUEstartShiny = 
    function () 
    {
      logdose<<-1
      require("mvtnorm")
      data(DUEenvironmentDefault)
      isolate({
        for(objname in names(DUEenvironmentDefault$DUEinits.default))
          eval(parse(text=print(paste0(
            "DUEenv$", objname, " <- get('",
            objname, "', DUEenvironmentDefault$DUEinits.default)"
          ))) )
        DUEenv$bgWindow <- "darkblue"
        print(DUEenv$bgWindow)
      })
      DUEenv$label.utilitychoice <- "X"
      # setupProbLines()
      DUEenv$label.utilityTitle <- "Utility functions"
      DUEenv$Unames = paste0("U.", c('rt','rT','Rt','RT'))
      #### End of DUEstartShiny ####    
    }
  
  
  DUEenv = reactiveValues()
  #### Overrides - DUEget, DUEput  ####
  DUEget = function(objname) DUEenv[[objname]]
  DUEput = function(objname, value) 
    DUEenv[[objname]] = value
  doseParameters(resetToDefaults = TRUE)
  ##### Create Utility Choice Buttons ####
  isolate({
    DUEenv$utilityChoices <- list(
      "Additive"   = data.frame(U.rt=0, U.rT=-1,  U.Rt=1, U.RT= 0),
      "Simple"     = data.frame(U.rt=0, U.rT= 0,  U.Rt=1, U.RT= 0),
      "Cautious"   = data.frame(U.rt=0, U.rT=-1,  U.Rt=1, U.RT=-1),
      "Aggressive" = data.frame(U.rt=0, U.rT= -1,  U.Rt=1, U.RT= 1)
    )
    DUEenv$utilityChoiceNames <- names(DUEenv$utilityChoices)
  })
  DUEstartShiny()
  shinyDebuggingPanel::makeDebuggingPanelOutput(session) 
  observe({
    input$Additive
    TheseUvalues = DUEenv$utilityChoices$Additive
    isolate ({
      DUEenv$U.rt <- TheseUvalues$U.rt
      DUEenv$U.Rt <- TheseUvalues$U.Rt
      DUEenv$U.rT <- TheseUvalues$U.rT
      DUEenv$U.RT <- TheseUvalues$U.RT
    })
    print("Finished Additive observer")
  })
  observe({
    input$Simple
    TheseUvalues = DUEenv$utilityChoices$Simple
    DUEenv$U.rt = TheseUvalues$U.rt
    DUEenv$U.Rt = TheseUvalues$U.Rt
    DUEenv$U.rT = TheseUvalues$U.rT
    DUEenv$U.RT = TheseUvalues$U.RT
  })
  output$linePlot <- renderPlot({
    plotProbsAndEUsimplified()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

