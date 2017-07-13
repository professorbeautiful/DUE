library(shiny)
library(shinyDebuggingPanel)
library(shinyBS)
library(DUE)

browseUs = 'calculate.probabilities'

desc <- packageDescription('DUE')

data(DUEenvironmentDefault)
probLineNames = DUEenvironmentDefault$probLineNames
rt.outcome.colors = DUEenvironmentDefault$rt.outcome.colors

make_linethicknessButton = function(labelNum)
  column(1,
         tagAppendAttributes(
           bsButton(paste0('linethickness_', label<-probLineNames[labelNum]), label=label),
           style=paste0('text-color:', rt.outcome.colors[labelNum]) ) )
linethicknessButtons = 
  lapply(1:length(probLineNames), make_linethicknessButton)   
print(linethicknessButtons)

ui <- fluidPage(
  includeCSS('DUE.css'),
  titlePanel(div( style='text-align:center; color:blue;', 
                  paste("DUE Shiny app: date = ",
                        desc$Date, "  Version = ", desc$Version))),
  shinyDebuggingPanel::withDebuggingPanel() ,
  
  fluidRow(
    column(5, 
           h2("Joint prob density of thresholds", br(), "for response (R) and toxicity (T)",
              style='color:blue'),
           h3(style='text-align:center', "r = non-response, t = non-toxicity"),
           fluidRow(
             plotOutput("ThresholdContour", click=clickOpts(id = 'click-threshold'))), 
           h3("Controller for thresholds", style="text-align:center; color:blue"),
           hr(),
           fluidRow(
             column(2, style='background-color:lightgray;',
                    numericInput(inputId = "populationNumber", HTML("<br># of groups"), value = 1),
                    numericInput(inputId = "thisPopulation", HTML("<br>Viewing group..."), value = 1)
             ),
             column(1, ""),
             column(9, 
                    fluidRow(
                      style='background-color:lightgray; vertical-align:center; min-height: 100%;',
                             column(5, offset=1, strong("This group's proportion")),
                             column(5, numericInput(inputId = "thisPopFraction", "", value = 1))),
                      fluidRow(style='background-color:lightgray;',
                               column(5, offset=1, strong("Which group's proportion is dependent")),
                                      column(5,  numericInput(inputId = "popFractionFollows", 
                                          HTML(""), value = 1)))
                    ,
                    hr(),
                    div(
                      column(4, style='background-color:lightgray',
                             numericInput(inputId = "thetaRmean", "Theta R Mean", value= 282),
                             numericInput(inputId = "thetaR.CV", "Theta R CV", value = .8)),
                      column(4, style='background-color:lightgray; min-height: 100%; display: flex;
    align-items: center; vertical-align:center;',
                            
                             numericInput(inputId = "correlation", HTML("<br>Correlation"), value = .8)),
                      column(4, style='background-color:lightgray',
                             numericInput(inputId = "thetaTmean", "Theta T Mean", value = 447),
                             numericInput(inputId = "thetaT.CV", "Theta T CV", value = .8))
                    )
             )
           ),
           hr(),
           br(),
           fluidRow(style='background-color:lightgray;',
             column(6,
                    numericInput(inputId = "probRefractory", "Pr(refractorytumor)", value = .85)),
             column(6,
                    numericInput(inputId = "responseLimitingTox", "K(response-limiting toxicity", value = .6))
           )
    )
  , 
  column(1, 
         div(style=paste0(
           "border-left:1px solid #000;height:1500px;",
           "border-right:1px solid #000;height:1500px;"),
           # See also https://stackoverflow.com/questions/571900/is-there-a-vr-vertical-rule-in-html
           # especially the display:flex solution.
           br(), br(), br(), br(),
           div(style='text-align:center; color:lightgreen;',
               numericInput('favoriteDose', 'selected dose', value=100, min=0))
           # numericInput('favoriteDose', HTML("div('selected dose',
           #   style='text-align:center; color:blue;'"), value=100, min=0)
         )
  ),
  column(5
         , h2("Probabilities and Expected Utility, E(U)", style="color:blue")
         , (fluidRow(column(2,  HTML("Line thickness controls")), 
                                         linethicknessButtons))
                    , plotOutput("linePlot")
                    ,
                    div(
                      br(),
                      h3("Controller for utility values", style="text-align:center; color:blue"),
                      div(
                          fluidRow(  style="text-align:center; color:blue; font-size:medium",
                            column(6, strong("Enter custom values below:", style="text-align:center; color:blue")),
                            column(6, strong("Or choose a preset option", style="text-align:center; color:blue"))
                          ),
                          fluidRow(style='background-color:lightgrey;',
                            fluidRow(
                              column(4, h2("t", style="text-align:center;")),
                              column(2, h2("T", style="text-align:center;")),
                              column(offset=6.5, width = 3, HTML("&nbsp;"),
                                     bsButton(inputId="Additive", 
                                              HTML("Additive<br>R=+1, T=-1"))
                              )),
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
                                   span(class='rTobj', '⬅︎') ,   
                                   # LEFTWARDS ARROW
                                   # Unicode: U+2190, UTF-8: E2 86 90,
                                   tagAppendAttributes(
                                     bsButton(inputId="Simple", HTML("Simple<br>U.rT=0")),
                                     class='rTobj')
                            )
                          ),
                          div(style='background-color:lightgrey;'),
                          fluidRow(style='background-color:lightgrey;',
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
                                          span(class='RTobj', '⬉', style="font-size:200%;") ,  #NORTH WEST BLACK ARROW  Unicode: U+2B09, UTF-8: E2 AC 89
                                          tagAppendAttributes(
                                            bsButton(inputId="Aggressive", HTML("Aggressive<br>U.RT=+1")),
                                            class='RTobj')
                                   )
                          )
                      )
                    )
             )
  )
)
    

server <- function(input, output, session) {
    try(shinyDebuggingPanel::makeDebuggingPanelOutput(session) )
    DUEenv = reactiveValues()
    
    #setupProbLines(DUEenv)
    probLineNames <<- 
      c("R", "T", "rt","rT","Rt","RT","EU")
    probLabels  <<- list()
    probLineWidthChoices <<- c(0, 1, 5)
    probLineWidths <- rep(probLineWidthChoices[2], 7) 
    names(probLineWidths) <- probLineNames
    #probLineWidths["EU"] <- probLineWidthChoices[3] #5
    DUEenv$probLineWidths <- probLineWidths 
    
    # source("plotProbsAndEUsimplified.R", local = TRUE) # we are not using the reactive version, yet it works!
    # source("utilityControllers.R", local = TRUE)
    
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
    
    
    #### Overrides - DUEget, DUEput -- unnecessary! ####
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
    
    
    linethicknessObserving= function(label)  { 
      inputId = paste0('linethickness_', label)
      input[[inputId]]  ## for reactivity
      cat("observed click on linethicknessButton ", label, "\n")
      isolate({
        whichWidth = which(
          DUEenv$probLineWidths[label]==probLineWidthChoices)
        whichWidth = whichWidth + 1
        if(whichWidth > length(probLineWidthChoices))
          whichWidth = 1
        DUEenv$probLineWidths[label] <- probLineWidthChoices[whichWidth]
        updateButton(session, inputId, 
                     label=switch(whichWidth, `1`=paste0('(',label,')'),
                                  `2`=label, `3`=HTML(paste0('<b>',label,'</b>'))),
                     size = switch(whichWidth, `1`='small',
                                   `2`='', `3`='large'))
      })
    }
    observe(linethicknessObserving('R'))
    observe(linethicknessObserving('T'))
    observe(linethicknessObserving('rt'))
    observe(linethicknessObserving('rT'))
    observe(linethicknessObserving('Rt'))
    observe(linethicknessObserving('RT'))
    observe(linethicknessObserving('EU'))
    
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
    
    observe({
      tryval = input$thisPopFraction
      source('shiny.entrybox.popFraction.f.R', local = TRUE)
    })
    observe({
      #intended to mirror the code for the utility inputs, not sure what to put for "value"
      updateNumericInput(session = session, 'populationNumber', value=input$populationNumber)
      updateNumericInput(session = session, 'thisPopulation', value=input$thisPopulation)
      updateNumericInput(session = session, 'thisPopFraction', value=input$thisPopFraction)
      updateNumericInput(session = session, 'popFractionFollows', value=input$popFractionFollows)
      updateNumericInput(session = session, 'thetaRmean', value=input$thetaRmean)
      updateNumericInput(session = session, 'thetaR.CV', value=input$thetaR.CV)
      updateNumericInput(session = session, 'correlation', value=input$correlation)
      updateNumericInput(session = session, 'thetaTmean', value=input$thetaTmean)
      updateNumericInput(session = session, 'thetaT.CV', value=input$thetaT.CV)
      updateNumericInput(session = session, 'probRefractory', value=input$probRefractory)
      updateNumericInput(session = session, 'responseLimitingTox', value=input$responseLimitingTox)
    })
    
    
    output$linePlot <- renderPlot({
      plotProbsAndEUsimplified(DUEenv)
    })
    
    output$ThresholdContour<- renderPlot({
      DUEenv
      print('called plotThresholdContour')
      cexQ = 4; OKfont = c("sans serif", "bold")
      isolate(recalculate.means.and.variances(DUEenv))
      the.grid = as.matrix(expand.grid(log10(DUEenv$doseValues),
                                       log10(DUEenv$doseValues)))
      the.dmvnorms = apply(as.array(1:DUEenv$nPops), 1, function(i) {
        #cat("plotThresholdContour: the.means.pop[[i]] = ", DUEenv$the.means.pop[[i]], "\n")
        return(DUEenv$proportions[i] * dmvnorm(the.grid, mean = DUEenv$the.means.pop[[i]]/log(10),
                                               sigma = DUEenv$the.variances.pop[[i]]))
      })
      the.dmvnorms = array(the.dmvnorms, dim = c(nrow(the.grid),
                                                 DUEenv$nPops))
      contour.values = matrix(apply(the.dmvnorms, 1, sum), nrow = DUEenv$nDoses)
      contour(DUEenv$doseValues, DUEenv$doseValues, contour.values,
              xlim = range(DUEenv$doseValues), ylim = range(DUEenv$doseValues),
              log = "xy", axes = F, xlab = "Threshold of Response",
              ylab = "Threshold of Toxicity")
      DUEenv$parPlotSize.contour <- par("plt")
      DUEenv$usrCoords.contour <- par("usr")
      ### vfont works for text but not for axis or title. (ERROR)
      ### font.main and family work for title, but not for axis.(not an error, just no effect).  HersheySans etc but not Sans or Serif. Or Arial.
      ### for axis, cex.axis  and font.axis affect the tick values
      ### for axis, cex.lab  and font.lab do NOT affect the labels 
      ###  This system stinks & is so poorly documented!
      axis(1, at = with(DUEenv, doseTicks),
           cex.axis=1.0) #, cex.lab=3)
      #font.axis=2, font.lab=2, family="Arial")
      axis(2, at = with(DUEenv, doseTicks),
           cex.axis=1.0) # cex.lab=3)
      #font.axis=4, font.lab=2, family="HersheySans")
      #title(main = plot.title, cex.main = 1.5, col.main = "blue")
      #font.main=4, family="HersheySerif")
      ###  Works for title() not for axis().
      abline(a = 0, b = 1, lty = 2, col = "black", lwd = 3)
      drawQuadrants()
      for (iPop in 1:DUEenv$nPops)
        text(DUEenv$the.Ethresholds.pop[[iPop]][1],
             DUEenv$the.Ethresholds.pop[[iPop]][2],
             as.character(iPop),
             vfont=OKfont,
             cex = 4, col = "red")
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
  