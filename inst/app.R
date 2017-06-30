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
library(shinyBS)
library(DUE)

browseUs = 'calculate.probabilities'

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
    column(3,
           fluidRow(
             bsButton(inputId="Additive", "Additive")),
           fluidRow(
             bsButton(inputId="Simple", "Simple"),
             bsButton(inputId="Cautious", "Cautious"),
             bsButton(inputId="Aggressive", "Aggressive"))
    ),
    column(3,
           "Responsive, but not yet activated",
           fluidRow(
                  numericInput(inputId="U.rt", "U.rt", value=0),
                  numericInput(inputId="U.rT", "U.rT", value=0)),
           fluidRow(
                  numericInput(inputId="U.Rt", "U.Rt", value=0),
                  numericInput(inputId="U.RT", "U.RT", value=0))
    )
  )
)


server <- function(input, output, session) {
  shinyDebuggingPanel::makeDebuggingPanelOutput(session) 
  
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
  ## isolate({doseParameters(resetToDefaults = TRUE)})
  ## Unknown why this call to doseParameters does not change DUEenv.
  ## But this source'ing approach works.
  source('doseParametersForApp.R', local=TRUE)
  
  ##### Create Utility Choice Buttons ####
  isolate({
    DUEenv$utilityChoices <- list(
      "Additive"   = data.frame(U.rt=0, U.rT=-1,  U.Rt=1, U.RT= 0),
      "Simple"     = data.frame(U.rt=0, U.rT= 0,  U.Rt=1, U.RT= 0),
      "Cautious"   = data.frame(U.rt=0, U.rT=-1,  U.Rt=1, U.RT=-1),
      "Aggressive" = data.frame(U.rt=0, U.rT= -1,  U.Rt=1, U.RT= 1)
    )
    DUEenv$utilityChoiceNames <- names(DUEenv$utilityChoices)
    DUEput('testing', 'testing')  ### OK
  })
  DUEstartShiny()
  observe({
    updateNumericInput(session=session, 'U.rt', value=DUEenv$U.rt)
    updateNumericInput(session=session, 'U.Rt', value=DUEenv$U.Rt)
    updateNumericInput(session=session, 'U.rT', value=DUEenv$U.rT)
    updateNumericInput(session=session, 'U.RT', value=DUEenv$U.RT)
  })
  resetButtonStyles = function(whichButton) {
    for(button in DUEenv$utilityChoiceNames) 
      updateButton(session, button,
                   style='default')
    updateButton(session, whichButton,
                 style='success')
    }
  updateUtilities = function(TheseUvalues) {
    DUEenv$U.rt = TheseUvalues$U.rt
    DUEenv$U.Rt = TheseUvalues$U.Rt
    DUEenv$U.rT = TheseUvalues$U.rT
    DUEenv$U.RT = TheseUvalues$U.RT
  }
  
  
  observe({
    input$Additive
    resetButtonStyles('Additive')
    isolate ({
      DUEenv$utility = TheseUvalues = 
        DUEenv$utilityChoices$Additive
      updateUtilities(TheseUvalues)
    })
  })
  observe({
    input$Simple
    resetButtonStyles('Simple')
    DUEenv$utility = TheseUvalues = 
      DUEenv$utilityChoices$Simple
    updateUtilities(TheseUvalues)
  })
  observe({
    input$Cautious
    resetButtonStyles('Cautious')
    DUEenv$utility = TheseUvalues = 
      DUEenv$utilityChoices$Cautious
    updateUtilities(TheseUvalues)
  }) 
  observe({
    input$Aggressive
    resetButtonStyles('Aggressive')
    DUEenv$utility = TheseUvalues = 
      DUEenv$utilityChoices$Aggressive
    updateUtilities(TheseUvalues)
  })
  output$linePlot <- renderPlot({
    plotProbsAndEUsimplified()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

