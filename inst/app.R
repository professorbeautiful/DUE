library(shiny)
library(shinyDebuggingPanel)
library(shinyBS)
library(DUE)

# Let's try installExprFunction(), to aid debugging with breakpoints.

browseUs = 'calculate.probabilities'

desc <- packageDescription('DUE')

try(rm(DUEenv))
try(rm(DUEenvironmentDefault))

data(DUEenvironmentDefault)
rt.outcome.colors <<- c(R='#00ff00', T='#ff0000', rt='#8E9233', rT='#F007E6', 
                      Rt='#009215', RT='#CB8C92', EU='#000000', RLE='#6C9291')
probLineNames <<- rt.outcome.strings <<- names(rt.outcome.colors)
#"darkgreen" "red" "darkblue" "magenta" "dark goldenrod" "sea green" "black"

make_linethicknessButton = function(labelNum)
  column(1,
         tagAppendAttributes(
         bsButton(paste0('linethickness_', label<-probLineNames[labelNum]), label=label),
                  style=paste0('color:', rt.outcome.colors[labelNum], ';',
                               'border-color:', rt.outcome.colors[labelNum], ';') ) )
linethicknessButtons = 
  lapply(1:length(probLineNames), make_linethicknessButton)   
print(linethicknessButtons)

ui <- fluidPage(
  includeCSS('DUE.css'),
  titlePanel(div( style='text-align:center; color:blue;', 
                  paste("DUE Shiny app: date = ",
                        desc$Date, "  Version = ", desc$Version))),
  shinyDebuggingPanel::withDebuggingPanel() ,
  fluidRow(style='text-align:center',
           column(5, 
                  h2("Joint prob density of thresholds", br(), 
                     style='color:blue'),
                  fluidRow(style='text-align:center; text-color:blue;color:blue; font-size:150%;', 
                           column(4, offset=2, "R = response", br(), "r = non-response"), 
                           column(4, "T = toxicity", br(), "t = non-toxicity")
                  ),
                  fluidRow(
                    plotOutput("ThresholdContour", click = 'click_threshold')), 
                  h3("Controller for thresholds", style="text-align:center; color:blue"),
                  fluidRow(
                    column(4, offset=4, div(style='background-color:lightgray; align-items:center; text-align:center',
                                            numericInput(inputId = "nPops",  "Number of groups", value = 2, min=1))
                    )
                  ),
                  fluidRow(style='background-color:lightgray; vertical-align:center; min-height: 100%;',
                           column(4, 
                                  numericInput(inputId = "thisPop", "This group #", value = 1, min = 1, max = DUEenvironmentDefault$nPops)),
                           column(4, 
                                  numericInput(inputId = "thisPopFraction", "This group's proportion", value = 0.6, min=0, max=1, step=0.1)),
                           column(4, 
                                  numericInput(inputId = "whichFollows", 
                                               HTML("Dependent group #"), value = 2))
                  ),
                  hr(style='margin-top:0em; margin-bottom:0em; border-color:white'),
                  fluidRow(style='background-color:lightgray; vertical-align:center; min-height: 100%;',
                           column(4, style='background-color:lightgray',
                                  numericInput(inputId = "thetaRmedian", "Theta R Mean", value= 282),
                                  numericInput(inputId = "thetaR.CV", "Theta R CV", value = .8)),
                           column(4, 
                                  #                             style='background-color:lightgray; min-height: 100%; display: flex;
                                  #    align-items: center; vertical-align:center;display:inline-block;vertical-align:middle;',  ### none of this works!
                                  br(style='background-color:white;'),
                                  div(style='background-color:lightgray;', 
                                      numericInput(inputId = "correlation", "Correlation", value = DUEenvironmentDefault$the.correlations.pop[1],
                                                   min = -(1-0.01), max = 1-0.01, step = 0.1))
                           ),
                           column(4, style='background-color:lightgray',
                                  numericInput(inputId = "thetaTmedian", "Theta T Mean", value = 447),
                                  numericInput(inputId = "thetaT.CV", "Theta T CV", value = .8))
                           
                  ),
                  hr(style='margin-top:0em; margin-bottom:0em; border-color:white'),
                  h3("Auxiliary parameters", style='color:blue;'),
                  fluidRow(style='background-color:lightgray;',
                           column(6,
                                  numericInput(inputId = "probRefractory", "Pr(refractorytumor)", value = .85, step = .1)),
                           column(6,
                                  numericInput(inputId = "responseLimitingTox", "RLE: log10 (response-limiting gap) (RT->rT)", value = .6))
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
                    div(style='text-align:center; color:white; border-color:darkgreen; background-color:green;',
                        numericInput('favoriteDose', 'selected dose', value=100, min=0))
                  ),
                  actionButton(inputId = "save", label = "Save")
           ),
           column(5
                  , h2("Probabilities and Expected Utility, E(U)", style="color:blue")
                  , fluidRow(style='background-color:lightgrey;', column(2,  HTML("Line thickness controls")), 
                             linethicknessButtons)
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
                                 column(width = 4, HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                        tagAppendAttributes(
                                          bsButton(inputId="Additive",
                                                   HTML("Additive<br>R=+1, T=-1")),
                                          style=paste0('background-color:black; color:white;'))
                               )),
                               fluidRow(
                                 column(1, h2("r")),
                                 column(2,
                                        tagAppendAttributes(
                                          numericInput(inputId="U.rt", "U.rt", value=0),
                                          style=paste0('color:', rt.outcome.colors['rt']))),
                                 column(2, offset=1,
                                        tagAppendAttributes(
                                          numericInput(inputId="U.rT", "U.rT", value=-1),
                                          style=paste0('color:', rt.outcome.colors['rT']))),
                                 # we could also try transform: rotate(7deg);
                                 column(4, style=paste0('color:', rt.outcome.colors['rT']),
                                        br(),
                                        span(style=paste0('color:', rt.outcome.colors['rT']),
                                             '⬅︎') ,
                                        # LEFTWARDS ARROW
                                        # Unicode: U+2190, UTF-8: E2 86 90,
                                        tagAppendAttributes(
                                          bsButton(inputId="Simple", HTML("Simple<br>U.rT=0")),
                                          style=paste0('background-color:', rt.outcome.colors['rT'],
                                                       '; color:white;')
                                        )
                                 )
                               )
                      )
                    ),
                    div(style='background-color:lightgrey;', ""),
                    fluidRow(style='background-color:lightgrey;',
                             column(1, h2("R")),
                             column(2,
                                    tagAppendAttributes(
                                      numericInput(inputId="U.Rt", "U.Rt", value=1),
                                      style=paste0('color:', rt.outcome.colors['Rt']))),
                             column(2, offset=1,
                                    tagAppendAttributes(
                                      numericInput(inputId="U.RT", "U.RT", value=0),
                                      style=paste0('color:', rt.outcome.colors['RT']))
                             )
                             ,
                             column(4, style=paste0('color:', rt.outcome.colors['RT']),
                                    span( '⬋', style="font-size:200%;") ,   #SOUTH WEST BLACK ARROW Unicode: U+2B0B, UTF-8: E2 AC 8B)
                                    tagAppendAttributes(
                                      bsButton(inputId="Cautious", HTML("Cautious<br>U.RT=-1")),
                                      style=paste0('background-color:', rt.outcome.colors['RT'],
                                                   '; color:white;')),
                                    br(),
                                    span('⬉', style="font-size:200%;") ,  #NORTH WEST BLACK ARROW  Unicode: U+2B09, UTF-8: E2 AC 89
                                    tagAppendAttributes(
                                      bsButton(inputId="Aggressive", HTML("Aggressive<br>U.RT=+1")),
                                      style=paste0('background-color:', rt.outcome.colors['RT'],
                                                   '; color:white;'))
                             )
                    )
                  )
           )
  )
)


####Server starts here####

server <- function(input, output, session) {
  try(shinyDebuggingPanel::makeDebuggingPanelOutput(session) )
  DUEenv = reactiveValues()
  
  #### In place of setupProbLines(DUEenv) ####
  probLineNames <<- 
    c("R", "T", "rt","rT","Rt","RT","EU", "RLE")
  probLabels  <<- list()
  probLineWidthChoices <<- c(0, 1, 5)
  probLineWidths <- rep(probLineWidthChoices[2], 8) 
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
  observe(linethicknessObserving('RLE'))
  
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
  #### When changing proportions (fractions), one has to be dependent.
  ####  Callback needs to check ranges and modify the dependent fraction to add to 1.
  
  observe({
    tryval = input$thisPopFraction
    sanityCheck = try(isolate(DUEenv$whichFollows == DUEenv$thisPop))
    if(class(sanityCheck)=='try-error' 
       | sanityCheck==TRUE){
      isolate(
        if(DUEenv$nPops >1) {
          newWhichFollows = ifelse(input$thisPop==1, DUEenv$nPops, input$thisPop - 1 )
          updateNumericInput(session, 'whichFollows', value=newWhichFollows)
        }
      )
    }
    #source('shiny.entrybox.popFraction.f.R', local = TRUE)
    #	cat("entrybox.popFraction.f: Callback call: ", sys.call(), "\n")
    popFractionTemp = as.numeric(tryval) 
    badtryval<-(
      is.na(popFractionTemp ) | (popFractionTemp < 0) 
      | ((DUEenv$nPops > 1) 
         & popFractionTemp > 1 + DUEenv$proportions[DUEenv$whichFollows])
    ) 
    if ( ! badtryval){
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
        } else if (any(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop] > 0)){
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
      })
    }
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
        updateNumericInput(session = session, 'thisPop', value = 1, min = 1, max=DUEenv$nPops)
      })
  })
  
  #### observing thisPop ####
  
  observe({
    # installExprFunction(name = 'thisFunc', expr = {
#    isolate(
#      cat("ENTERING: theta  medians is ", capture.output(DUEenv$the.medianThresholds.pop), '\n')
#    )
    thisPop<-input$thisPop
    sanityCheck = try(isolate(input$whichFollows == thisPop))
    if(class(sanityCheck)=='try-error' 
       | sanityCheck==TRUE){
      isolate(
        if(DUEenv$nPops >1) {
          newWhichFollows = ifelse(thisPop==1, DUEenv$nPops, thisPop - 1 )
          updateNumericInput(session, 'whichFollows', value=newWhichFollows)
        }
      )
    }
    isolate({
      cat("thisPop is now ", thisPop, '\n')
        DUEenv$thisPop = input$thisPop
#      cat("BEFORE: theta  medians is ", capture.output(DUEenv$the.medianThresholds.pop), '\n')
      updateNumericInput(session, "thetaRmedian", value = DUEenv$the.medianThresholds.pop[[thisPop]] [1])
      updateNumericInput(session, "thetaTmedian", value = DUEenv$the.medianThresholds.pop[[thisPop]] [2])
      updateNumericInput(session, "thetaR.CV", value = DUEenv$the.CVs.pop[[thisPop]] [1])
      updateNumericInput(session, "thetaT.CV", value = DUEenv$the.CVs.pop[[thisPop]] [2])
      updateNumericInput(session, "correlation", value = DUEenv$the.correlations.pop[thisPop])
      updateNumericInput(session, "thisPopFraction", value = DUEenv$proportions[thisPop])
      #      cat("AFTER: theta  medians is ", capture.output(DUEenv$the.medianThresholds.pop), '\n')
    })
    #invalidateLater(millis=5000)
    # })
    # thisFunc()
  })
  observe({
    input$thetaRmedian
    isolate({
      DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [1] = input$thetaRmedian
      })
    #DUEenv$the.medianThresholds.pop is NULL
  })
  
  observe({
    input$thetaTmedian
    isolate({
    DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [2]= input$thetaTmedian
    })
    #DUEenv$the.medianThresholds.pop is NULL
  })
  
  observe({
    input$thetaR.CV
    isolate({
      DUEenv$the.CVs.pop[[DUEenv$thisPop]] [1] = input$thetaR.CV
    })
  })
  
  observe({
    input$thetaT.CV
    isolate({
      DUEenv$the.CVs.pop[[DUEenv$thisPop]] [2]= input$thetaT.CV
    })
  })
  
  observe({
    input$correlation
    isolate({
    DUEenv$the.correlations.pop[DUEenv$thisPop] = input$correlation
    })
    #above value exists; unsure why input does not react
  })
  
  observe({
    DUEenv$refractory= input$probRefractory
  })
  
  observe({
    DUEenv$Kdeath= input$responseLimitingTox
  })
  
  observe({
    print(input$click_threshold)
    try({
      newFavoriteDose = mean(c(input$click_threshold$x, input$click_threshold$y))
      if (!is.na(newFavoriteDose)){
        updateNumericInput(session, 'favoriteDose', value = newFavoriteDose)
        DUEenv$favoriteDose = newFavoriteDose
      }
    })
  })
  
  observe({
    DUEenv$favoriteDose = input$favoriteDose
  })
  
  output$linePlot <- renderPlot({
    plotProbsAndEUsimplified(DUEenv)
  })
  
  output$ThresholdContour<- renderPlot({
    DUEenv$the.CVs.pop
    DUEenv$the.correlations.pop
    print(DUEenv$the.correlations.pop)
    print('called plotThresholdContour')
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
            log = "xy", axes = F, xlab="", ylab="")
    mtext(side=2, line=2.5, "Threshold of Toxicity", cex=2)
    mtext(side=1, line=2.5, "Threshold of Response", cex=2)
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
      text(DUEenv$the.medianThresholds.pop[[iPop]][1],
           DUEenv$the.medianThresholds.pop[[iPop]][2],
           as.character(iPop), font=4,
           cex = 5, col = "black")
  })
}
####Saving interesting results####

observe({
  input$save
  updateButton(session, "save", style = 'success')
  save(list = names(DUEenv), file = 'DUE Noteworthy Findings', envir = DUEenv)
  savehistory(file = 'DUE Noteworthy Findings')
  timestamp(stamp = date())

  #https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
  
  for(objname in names(DUEenv))
    eval(parse(text=print(paste0(
      "DUEenv$", objname, " <- get('",
      objname, "', DUEenv)"
    ))) )
})


# Run the application 
shinyApp(ui = ui, server = server)
