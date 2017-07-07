library(shiny)
library(shinyDebuggingPanel)
library(shinyBS)
library(DUE)

browseUs = 'calculate.probabilities'

desc <- packageDescription('DUE')

ui <- fluidPage(
  includeCSS('DUE.css'),
  titlePanel(paste("DUE Shiny app: date = ",
                   desc$Date, "  Version = ", desc$Version)),
  shinyDebuggingPanel::withDebuggingPanel() ,
  fluidRow(
    column(6, "insert contour graph here"), 
    column(6, plotOutput("linePlot"))
  ),
  fluidRow(
    column(6, "insert contour controllers here"), 
    column(6, 
           div(
             br(),
             h2("Controller for Utility Values", style="text-align:center; color:blue"),
             fluidRow(           
               column(6, h3("Enter Custom Values Below:", style="text-align:center; color:blue")),
               column(6, h3("Or choose a Preset Option", style="text-align:center; color:blue"),
                      bsButton(inputId="Additive", 
                               HTML("Additive<br>R=+1, T=-1")))
             )),
           fluidRow(
             fluidRow(
               column(4, h2("t", style="text-align:center;")),
               column(2, h2("T", style="text-align:center;"))
             ),
             column(1, h2("r")),
             column(2, 
                    tagAppendAttributes(
                      numericInput(inputId="U.rt", "U.rt", value=0),
                      class='rtobj')),
             column(2, offset=1,
                    tagAppendAttributes(
                      numericInput(inputId="U.rT", "U.rT", value=-1),
                      class='rTobj')),
               column(4,
                      br(), 
                      span(class='rTobj', '←') ,   
                      # LEFTWARDS ARROW
                      # Unicode: U+2190, UTF-8: E2 86 90,
                      tagAppendAttributes(
                        bsButton(inputId="Simple", HTML("Simple<br>U.rT=0")),
                        class='rTobj')
                      )
           ),
           br(),
           column(1, h2("R")),
           column(2, 
                  tagAppendAttributes(
                    numericInput(inputId="U.Rt", "U.Rt", value=1),
                    class='Rtobj')),
           column(2, offset=1,
                  tagAppendAttributes(
                    numericInput(inputId="U.RT", "U.RT", value=0),
                    class='RTobj'))
           ,
           column(4, 
                  span(class='RTobj', '⬋', style="font-size:200%;") ,   #SOUTH WEST BLACK ARROW Unicode: U+2B0B, UTF-8: E2 AC 8B)
                  tagAppendAttributes(
                    bsButton(inputId="Cautious", HTML("Cautious<br>U.RT=-1")),
                    class='RTobj'),
                  br(),
                  span(class='RTobj', '⬉') ,  #NORTH WEST BLACK ARROW  Unicode: U+2B09, UTF-8: E2 AC 89
                  tagAppendAttributes(
                    bsButton(inputId="Aggressive", HTML("Aggressive<br>U.RT=+1")),
                    class='RTobj')
           )
    )
  )
)


server <- function(input, output, session) {
  try(shinyDebuggingPanel::makeDebuggingPanelOutput(session) )
  
  source("plotProbsAndEUsimplified.R", local = TRUE)
  source("utilityControllers.R", local = TRUE)
  
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
    cat("resetButtonStyles: whichButton = ", whichButton, '\n')
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
  observe({
    TheseUvalues = data.frame(U.rt = input$U.rt , U.rT = input$U.rT, U.Rt = input$U.Rt, U.RT = input$U.RT)
    DUEenv$utility = TheseUvalues
    updateUtilities(TheseUvalues)
    
    for(button in DUEenv$utilityChoiceNames) {
      if (all(TheseUvalues == DUEenv$utilityChoices[[button]]))
        updateButton(session, button, style='success')
      else
        updateButton(session, button, style='default')
    }
    
  })
  
  
  output$linePlot <- renderPlot({
    plotProbsAndEUsimplified(DUEenv)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

