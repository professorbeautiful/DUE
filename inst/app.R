library(shiny)
library(shinyDebuggingPanel)
library(shinyBS)
library(DUE)

# Let's try installExprFunction(), to aid debugging with breakpoints.

browseUs = 'calculate.probabilities'

desc <- packageDescription('DUE')

try(rm(DUEenv))

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
  titlePanel(paste("DUE Shiny app: date = ",
                   desc$Date, "  Version = ", desc$Version)),
  shinyDebuggingPanel::withDebuggingPanel() ,
  fluidRow(
    
    column(6, plotOutput("ThresholdContour", click = 'plot_click')), 
    column(6, plotOutput("linePlot")
           , wellPanel(fluidRow(column(1,  HTML("Line thickness<br>controls")), column(1, ""),
                                linethicknessButtons))
    )
  ),
  fluidRow(
    column(6, 
           fluidRow(
             column(4, 
                    numericInput(inputId = "nPops", "# of Populations", value = DUEenvironmentDefault$nPops, min = 1)),
             column(4, 
                    numericInput(inputId = "thisPopulation", "This Population", value = 1)),
             column(4,
                    numericInput(inputId = "thisPopFraction", "This Population Fraction", value = 0.5, step = 0.1)
             ),
             
             fluidRow(
               column(2, 
                      numericInput(inputId = "popFractionFollows", "Which Population Fraction Follows", value = 2)),
               column(2,
                      numericInput(inputId = "thetaRmedian", "Theta R Median", value = DUEenvironmentDefault$the.medianThresholds.pop[[1]] [1])),
               column(2,
                      numericInput(inputId = "thetaR.CV", "Theta R CV", value = DUEenvironmentDefault$the.CVs.pop[[1]] [1])),
               column(2,
                      numericInput(inputId = "correlation", "Correlation", value = DUEenvironmentDefault$the.correlations.pop[1])),
               column(2,
                      numericInput(inputId = "thetaTmedian", "Theta T Median", value = DUEenvironmentDefault$the.medianThresholds.pop[[1]] [2])),
               column(2,
                      numericInput(inputId = "thetaT.CV", "Theta T CV", value = DUEenvironmentDefault$the.CVs.pop[[1]] [2]))
             ),
             
             fluidRow(
               column(6,
                      numericInput(inputId = "probRefractory", "Pr(refractorytumor)", value = DUEenvironmentDefault$refractory)),
               column(6,
                      numericInput(inputId = "responseLimitingTox", "K(response-limiting toxicity", value = DUEenvironmentDefault$Kdeath))
             )
             
           )
    ), 
    column(6, 
           div(
             br(),
             h2("Controller for Utility Values", style="text-align:center; color:blue"),
             fluidRow(           
               column(6, h3("Enter Custom Values Below:", style="text-align:center; color:blue")),
               column(6, h3("Or choose a Preset Option", style="text-align:center; color:blue"))
             )),
           fluidRow(
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
                  span(class='RTobj', '⬉', style="font-size:200%;") ,  #NORTH WEST BLACK ARROW  Unicode: U+2B09, UTF-8: E2 AC 89
                  tagAppendAttributes(
                    bsButton(inputId="Aggressive", HTML("Aggressive<br>U.RT=+1")),
                    class='RTobj')
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
  ####thisPopFraction input#####
  observe({
    tryval = input$thisPopFraction
    #source('shiny.entrybox.popFraction.f.R', local = TRUE)
    #	cat("entrybox.popFraction.f: Callback call: ", sys.call(), "\n")
    popFractionTemp = as.numeric(tryval) 
    badtryval<-(is.na(popFractionTemp ) | (popFractionTemp < 0) 
                | (popFractionTemp > 1 + DUEenv$proportions[DUEenv$whichFollows])) 
    if (!badtryval){
      isolate({
      proportionDifference = DUEenv$proportions[DUEenv$thisPop] - popFractionTemp
      DUEenv$proportions[DUEenv$thisPop] <- popFractionTemp 
      #	print(proportions[thisPop])
      #	print(proportions)
      #	print(proportions[1:nPops][-whichFollows])
      #	print(sum(proportions[1:nPops][-whichFollows]))
      proportionWhichFollows = 1 - sum(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$whichFollows])
      cat("proportionDifference = ", proportionDifference, "\n")
      cat("proportionWhichFollows = ", proportionWhichFollows, "\n")
      if(proportionWhichFollows >= 0 & proportionWhichFollows <= 1){
        DUEenv$proportions[DUEenv$whichFollows] <- proportionWhichFollows
      }else if (any(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop] > 0)){
        DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop] <-
          DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop] * (1 - DUEenv$proportions[DUEenv$thisPop])/sum(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop])
      } else {  #### all others are zero
        if(DUEenv$nPops == 1)
          DUEenv$proportions[1] <- 1.0
        else if (DUEenv$thisPop == 1)
          DUEenv$proportions[2] <- 1 - DUEenv$proportions[1]
        else
          DUEenv$proportions[1] <- 1 - DUEenv$proportions[DUEenv$thisPop]
      }
      
      }
      
      )
    }
    #### When changing proportions (fractions), one has to be dependent.
    ####  Callback needs to check ranges and modify the dependent fraction to add to 1.
  })
  ####nPops input#####
  observe({
    nPopsTemp=as.integer(input$nPops)
    if (!is.null(input$nPops))
      isolate({
        if(nPopsTemp < DUEenv$nPops) {
          DUEenv$proportions = DUEenv$proportions[1:nPopsTemp]/sum(DUEenv$proportions[1:nPopsTemp])
          if(DUEenv$thisPop > nPopsTemp) 
            DUEenv$thisPop <- nPopsTemp      
        }      
        
        if(nPopsTemp > DUEenv$nPops) {
          newPopIndices <- (DUEenv$nPops+1):nPopsTemp
          DUEenv$proportions[newPopIndices] <- 0 
          for(i in newPopIndices) {
            DUEenv$the.logmedians.pop[[i]] <- DUEenv$the.logmedians.pop[[DUEenv$nPops]]
            DUEenv$the.variances.pop[[i]] <- DUEenv$the.variances.pop[[DUEenv$nPops]]
            DUEenv$the.correlations.pop[[i]] <- DUEenv$the.correlations.pop[[DUEenv$nPops]]
          }
        }
        DUEenv$nPops <- nPopsTemp
        #source('shiny.entrybox.nPops.f.R', local = TRUE)
        updateNumericInput(session = session, 'nPops', value = DUEenv$nPops)
        updateNumericInput(session = session, 'thisPopulation', value = DUEenv$nPops)
      })
  })
  
  observe({
    DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [1] = input$thetaRmedian
  })
  
  observe({
    DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [2]= input$thetaTmedian
  })
  
  observe({
    DUEenv$the.CVs.pop[[DUEenv$thisPop]] [1] = input$thetaR.CV
  })
  
  observe({
    DUEenv$the.CVs.pop[[DUEenv$thisPop]] [2]= input$thetaT.CV
  })
  
  output$linePlot <- renderPlot({
    plotProbsAndEUsimplified(DUEenv)
  })
  
  output$ThresholdContour<- renderPlot({
    DUEenv
    print('called plotThresholdContour')
    plot.title = "Contour plot for thresholds"
    cexQ = 4; OKfont = c("sans serif", "bold")
    isolate(recalculate.means.and.variances(DUEenv))
    the.grid = as.matrix(expand.grid(log10(DUEenv$doseValues),
                                     log10(DUEenv$doseValues)))
    the.dmvnorms = apply(as.array(1:DUEenv$nPops), 1, function(i) {
      #cat("plotThresholdContour: the.medianThresholds.pop[[i]] = ", DUEenv$the.medianThresholds.pop[[i]], "\n")
      return(DUEenv$proportions[i] * dmvnorm(the.grid, mean = DUEenv$the.logmedians.pop[[i]]/log(10),
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
    title(main = plot.title, cex.main = 1.5, col.main = "blue")
    #font.main=4, family="HersheySerif")
    ###  Works for title() not for axis().
    abline(a = 0, b = 1, lty = 2, col = "black", lwd = 3)
    drawQuadrants()
    for (iPop in 1:DUEenv$nPops)
      text(DUEenv$the.medianThresholds.pop[[iPop]][1],
           DUEenv$the.medianThresholds.pop[[iPop]][2],
           as.character(iPop),
           vfont=OKfont,
           cex = 4, col = "red")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

