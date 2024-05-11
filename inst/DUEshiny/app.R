library(shiny)
library(shinyDebuggingPanel)
library(shinyBS)
library(DUE)
library(shinyjs)

desc <- packageDescription('DUE')
extraTitle = span(style='font-size:50%',
     desc$Date, "  version = ", 
     desc$Version)
# Let's try installExprFunction(), to aid debugging with breakpoints.
legendStyle = 'text-align:left; 
           text-color:blue; font-size:150%;'
defaultBackgroundColor = 'background-color:#F4FAFA;'
#defaultBackgroundColor = 'background-color:white;'
phase1backgroundcolor = defaultBackgroundColor
data('DUEinits.default', package='DUE')
source('quadrant_personalUtilities.R', local = TRUE)
source('quadrant_probGraph.R', local = TRUE)
source('quadrant_popThresholdContour.R', local = TRUE)
source('quadrant_popThresholdController.R', local = TRUE)
source('skinnyColumn.R', local = TRUE)
source('controlRow.R', local = TRUE)
browseUs = 'calculate.probabilities'

options.saved = options(warn=-2)
try(rm(DUEenv))
try(rm(DUEenvironmentDefault))
try(rm(DUEsaving))
options(options.saved)

optsaved = options(warn=-2)
try(file.symlink(system.file("doc", "DUE_vignette.html", package="DUE"), 'www'), silent = TRUE)
options(optsaved)

spaces = function(n)
  HTML(paste0(collapse="", rep('&nbsp;', n)))


####UI starts here####
ui <- fluidPage(
  useShinyjs(),
  tags$style("[type = 'number'] 
             {font-size:25px;height:25px;}"),
  
  # tags$style("#myNumericInput 
  #            {font-size:100px;height:100px;}"),
  title = "Dose Utility Explorer",
  includeCSS('DUE.css'),
  shiny::includeScript('www/zoom_triggers.js'),
  shiny::includeScript('www/readInnerWindow.js'),
  #shiny::includeScript('windowZoomWarning.js'),
  includeScript('www/KeyToToggleDebugging.js'),
  singleton(tags$head(tags$script(src = "pop_patch.js"))),
  uiOutput('JSstopPopups'),
  tags$style(".popover{max-width: 100%; font-size:large}"),
  tags$meta(
    name="viewport", 
    content="width=device-width, initial-scale=1.0, user-scalable=no"),
  # user-scalable=no does nothing in Safari.
  uiOutput('JSprimping'),
  titlePanel(fluidRow( style='text-align:center; color:blue;', 
                       column(4, ""),
                  column(3, "Dose Utility Explorer "), 
                  column(2, ""),
                  column(2, extraTitle)
  )),
  fluidRow(column(4, ""),
           column(2, style=legendStyle, 
                  "R = response", br(),
                  "r = no response"),
           column(2, style=legendStyle, 
                  "T = toxicity", br(),
                  "t = no toxicity")
  ),
  #### Four quadrants ####
  fluidRow(style='text-align:center', 
           #### Utilities column ####
           column(5, id='utilityProbabilityColumn',
                  ####  personalUtilities: 
                  quadrant_probGraph
                  #,hr(),
                  #### Probabilities and Expected Utility: 
                  ),
           ####  Skinny column: Doses and Files ####
           column(1, id='skinnyColumn',
                  skinnyColumn)
                  ,
           #  if desired, shinyjs::hide('skinnyColumn')
           #### Thresholds ####
           column(5, id='thresholdSide',
                  ####Contour plots ####
                  br(),
                  quadrant_popThresholdContour 
                  # ,br(), br(),
                  # br(), br()
           )
  ), 
  shiny::hr(style = 'margin-top: 0.5em; margin-bottom: 0.5em; border-style:inset; border-width: 2px'),
  fluidRow(
             column(5, quadrant_personalUtilities),
             column(1, ""),
             column(5, quadrant_popThresholdController)
           )
  ,
  controlRow,
  div(id = 'popFilePanel', uiOutput('SaveLoadPanel') ),
  shiny::hr(style = 'margin-top: 0.5em; margin-bottom: 0.5em; border-style:inset; border-width: 2px'),
  div(id = 'popDebugging', shinyDebuggingPanel::withDebuggingPanel() )
) 

####Server starts here####

server <- function(input, output, session) {
  
  shinyDebuggingPanel::makeDebuggingPanelOutput(
    session, toolsInitialState = FALSE,
    condition='ctrlDpressed === true')

  #### input$debugToolsCheckbox works to open R box ####
  observeEvent(input$debugToolsCheckbox,
                 try({
                   if(input$debugToolsCheckbox){
                     updateRadioButtons(
                       inputId='id_languageChoice', selected = 'R'
                     )
                   }
               })
  )
  DUEenv = reactiveValues()
  showRed33 = DUEenv$showRed33 = FALSE
  addProbsToQuadrants = DUEenv$addProbsToQuadrants = TRUE
  DUEenv$xpos = 0.5 ##  for location of probabilities in threshold quadrant
  DUEenv$ypos = 2.2 ##  for location of probabilities in threshold quadrant
  doseDelta = DUEenv$doseDelta = 10  # for numericInput
  
    
    #### zoomAdvice ####
  output$zoomAdvice = renderText({
    text = ""
    try({
      if(input$innerWidth > 1800)
        text = "Suggest zoom in. (Cmd+)"
      if(input$innerWidth < 1600)
        text = "Suggest zoom out. (Cmd-)"
    })
    paste0(text, '(', input$innerWidth, ')')
  })
  #### In place of setupProbLines(DUEenv) ####
  probLineWidthChoices <<- c(0, 1, 5)
  # invisible, thin, thick
  probLineWidths <- rep(probLineWidthChoices[1], 8)
  ### Set all to invisible at first.
  names(probLineWidths) <- probLineNames()
  probLineWidths["EU"] <- probLineWidthChoices[2] #1
  DUEenv$probLineWidths <- probLineWidths 
  
  logdose<<-1
  require("mvtnorm")
  data('DUEinits.default', package='DUE')
  isolate({
    for(objname in names(DUEinits.default))
      eval(parse(text=(paste0(
        "DUEenv$", objname, " <- get('",
        objname, "', DUEinits.default)"
      ))) )
    DUEenv$probLineWidths = probLineWidths
    DUEenv$probLineWidths =  ### back off by 1 because drawing the boxes jacks them by 1.
      #probLineWidthChoices[(-1 + match(DUEinits.default$probLineWidths, probLineWidthChoices)) %% 4 + 3*()]
      # NOT DUEinits.default$probLineWidths
      sapply(probLineWidths, function(n) {
        whichWidth = which(n == probLineWidthChoices)
        whichWidth = whichWidth - 1
        if(whichWidth == 0) whichWidth = length(probLineWidthChoices)
        return (probLineWidthChoices[whichWidth])
      })
   })
  #### End of DUEstartShiny ####    
  
  ## isolate({doseParameters(resetToDefaults = TRUE)})
  ## Unknown why this call to doseParameters does not change DUEenv.
  ## But this source'ing approach works.
  # source('doseParametersForApp.R', local=TRUE)
  
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
  
  
  linethicknessObserving= function(label)  { 
    inputId = paste0('linethickness_', label)
    input[[inputId]]  ## for reactivity
    #cat("observed click on linethicknessButton ", label, "\n")
    isolate({
      whichWidth = which(
        DUEenv$probLineWidths[label]==probLineWidthChoices)
      whichWidth = whichWidth + 1
      if(whichWidth > length(probLineWidthChoices))
        whichWidth = 1
      DUEenv$probLineWidths[label] <- probLineWidthChoices[whichWidth]
      updateButton(session, inputId, 
                   label=switch(whichWidth, `1`=HTML(paste0('<del>-',label,'-</del>')),
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
  observe(linethicknessObserving('RLT'))
  observe(linethicknessObserving('EU'))
  
  observe({
    updateNumericInput(session=session, 'U.rt', value=DUEenv$U.rt)
    updateNumericInput(session=session, 'U.Rt', value=DUEenv$U.Rt)
    updateNumericInput(session=session, 'U.rT', value=DUEenv$U.rT)
    updateNumericInput(session=session, 'U.RT', value=DUEenv$U.RT)
  })
  resetButtonStyles = function(whichButton) {
    #cat("resetButtonStyles: whichButton = ", whichButton, '\n')
    for(button in DUEenv$utilityChoiceNames) 
      updateButton(session, button,
                   style='default')
    updateButton(session, whichButton,
                 style='success')
  }
  
  #### primp the utility buttons ####
  #              $("#Aggressive").addClass("primped") ## works!
  output$JSprimping = renderUI({
    whichMatched = (DUEenv$utilityChoiceMatch==DUEenv$utilityChoiceNames)
    evalString = paste0( collapse='\n',
                         '$("#', DUEenv$utilityChoiceNames, '").',
                         ifelse(whichMatched, 'addClass', 'removeClass') ,
                         '("primped"); ')
    evalString = gsub('"', "'", evalString) # replace all DQ with SQ.
    #print(evalString)  # We know this works.
    div(list(tags$script(evalString)))
  })
  primpMyChoice = function(choice){
    updateButton(session, choice)
    #cat('====> primping ', choice, '\n')
  }
  unprimpMyChoice = function(choice){
    updateButton(session, choice)
    #cat('====> UNprimping ', choice, '\n')
  }
  
  updateUtilities = function(TheseUvalues) {
    DUEenv$U.rt = TheseUvalues$U.rt
    DUEenv$U.Rt = TheseUvalues$U.Rt
    DUEenv$U.rT = TheseUvalues$U.rT
    DUEenv$U.RT = TheseUvalues$U.RT
    choiceMatch = ""
    for(choice in names(DUEenv$utilityChoices)) {
      if(all(TheseUvalues == DUEenv$utilityChoices[[choice]]))
        primpMyChoice(choiceMatch<-choice)
      else
        unprimpMyChoice(choice)
    }
    ##### DUEenv$utilityChoiceMatch = choiceMatch  ####
    DUEenv$utilityChoiceMatch = choiceMatch
    # This line makes the box-primping work, 
    #  AND causes the initial plots to load,
    #  but partially breaks the Load button loading?
    # Not clear now.
    #cat("updateUtilities: match for ", choiceMatch, '\n')
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
    if(all(sapply(TheseUvalues, is.numeric))){
      DUEenv$utility = TheseUvalues
      updateUtilities(TheseUvalues)
      
      for(button in DUEenv$utilityChoiceNames) {
        if (all(TheseUvalues == DUEenv$utilityChoices[[button]]))
          updateButton(session, button, style='success')
        else
          updateButton(session, button, style='default')
      }
    }
  })
  ####thisPopFraction input#####
  #### When changing proportions (fractions), one has to be dependent.
  ####  Callback needs to check ranges and modify the dependent fraction to add to 1.
  
  observeEvent(eventExpr = input$thisPopFraction, handlerExpr = {
    tryval = input$thisPopFraction
    sanityCheck = try(isolate(DUEenv$whichFollows == DUEenv$thisPop))
    if(class(sanityCheck)=='try-error' 
       | sanityCheck==TRUE){
      isolate(
        if(DUEenv$nPops >1) {
          newWhichFollows = ifelse(input$thisPop==1, DUEenv$nPops, input$thisPop - 1 )
          updateNumericInput(session, 'whichFollows', value=newWhichFollows)
          DUEenv$whichFollows = newWhichFollows
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
        proportionWhichFollows = 1 - sum(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$whichFollows])
        #cat("proportionDifference = ", proportionDifference, "\n")
        #cat("proportionWhichFollows = ", proportionWhichFollows, "\n")
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
  observeEvent(input$nPops,  {
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
            DUEenv$the.means.pop[[i]] <- DUEenv$the.means.pop[[DUEenv$nPops]]
            DUEenv$the.medianThresholds.pop[[i]] <- DUEenv$the.medianThresholds.pop[[DUEenv$nPops]]
            DUEenv$the.variances.pop[[i]] <- DUEenv$the.variances.pop[[DUEenv$nPops]]
            DUEenv$the.CVs.pop[[i]] <- DUEenv$the.CVs.pop[[DUEenv$nPops]]
            DUEenv$the.correlations.pop[[i]] <- DUEenv$the.correlations.pop[[DUEenv$nPops]]
          }
        }
        DUEenv$nPops <- nPopsTemp
        #source('shiny.entrybox.nPops.f.R', local = TRUE)
        updateNumericInput(session = session, 'nPops', value = DUEenv$nPops)
        updateNumericInput(session = session, 'thisPop', value = 1, min = 1, max=DUEenv$nPops)
        
        #### enable the relevant pop boxes only when Npops > 1 ####

          try({
            if(input$nPops > 1){
              shinyjs::enable(id='thisPop')
              shinyjs::enable(id='thisPopFraction')
              shinyjs::enable(id='whichFollows')
            }
            else if(input$nPops == 1){
              shinyjs::disable(id='thisPop')
              shinyjs::disable(id='thisPopFraction')
              shinyjs::disable(id='whichFollows')
            }
          })
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
      # cat("thisPop is now ", thisPop, '\n')
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
  observeEvent(
    input$thetaRmedian,
    isolate({
      if(is.numeric(input$thetaRmedian))
        DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [1] = input$thetaRmedian
    })
  )
  
  observeEvent(
    input$thetaTmedian,
    isolate({
      if(is.numeric(input$thetaTmedian))
        DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [2]= 
          input$thetaTmedian
    })
  )
  
  observe({
    input$thetaR.CV
    isolate({
      if(is.numeric(input$thetaR.CV))
        DUEenv$the.CVs.pop[[DUEenv$thisPop]] [1] = 
          input$thetaR.CV
    })
  })
  
  observe({
    input$thetaT.CV
    isolate({
      if(is.numeric(input$thetaT.CV))
        DUEenv$the.CVs.pop[[DUEenv$thisPop]] [2]= 
          input$thetaT.CV
    })
  })
  
  observe({
    input$correlation
    isolate({
      if(is.numeric(input$correlation))
        DUEenv$the.correlations.pop[DUEenv$thisPop] = 
          input$correlation
    })
  })
  
  observe({
    if(is.numeric(input$probRefractory))
      DUEenv$refractory= input$probRefractory
  })
  
  observe({
    if(is.numeric(input$responseLimitingTox))
      DUEenv$Kdeath= input$responseLimitingTox
  })
  
  observeEvent(input$click_dose, {
    save_options = options(warn = -1)  #ignore initial warning
    try({
      xClick=input$click_dose$x
      newSelectedDose = xClick
      updateNumericInput(session, inputId = 'selectedDose', 
                         value = newSelectedDose, step = DUEenv$doseDelta )
      DUEenv$selectedDose = newSelectedDose
    })
  })
  
  output$showProbs = renderUI({
    if(DUEenv$addProbsToQuadrants == FALSE) {
    probs = calculate.probabilities(
      DUEenv, log10dose=log10(DUEenv$selectedDose))
    makeEntry = function(label)
      column(6, style=paste0(
        'font-weight: bold; font-style: italic;
        color:', rt.outcome.colors(label)), 
             label, stringr::str_pad(width=4, pad='0', side='right',
               round(digits=2, probs[label])))
    div(
      "Probabilities",
      
      fluidRow(
        makeEntry('Rt'), makeEntry('rt')
      ),
      fluidRow(
        makeEntry('RT'), makeEntry('rT')
      )
    )
    } else ""
  })
  observeEvent(input$click_threshold, {
    save_options = options(warn = -1)  #ignore initial warning
    try({
      xClick=input$click_threshold$x
      yClick=input$click_threshold$y
      logX = log10(xClick)
      logY = log10(yClick)
      logMean = mean(c(logX, logY))
      distance2ToDiag = sqrt((logX-logMean)^2 + (logY-logMean)^2)
      DUEenv$the.logmedians.pop
      distance2ToMedians = sapply(DUEenv$the.logmedians.pop[1:DUEenv$nPops],
                                  function(XY)
                                    sqrt((logX-XY[1])^2 + (logY-XY[2])^2)
      )
      distances = c(distance2ToMedians, distance2ToDiag)
      minDistance = min(... = distances)
      whichMinDistance = which(minDistance == distances)[1]
      if(whichMinDistance == DUEenv$nPops+1) { ## new dose
        newSelectedDose = 10^(logMean)
        if (!is.na(newSelectedDose)){
         updateNumericInput(session, inputId = 'selectedDose', value = newSelectedDose )
         DUEenv$selectedDose = newSelectedDose
        }
      }else 
        updateNumericInput(session, inputId = 'thisPop', value = whichMinDistance )
      options(save_options)
    })
  })
  
  observe({
    if(is.numeric(input$selectedDose))
      DUEenv$selectedDose = input$selectedDose
  })
  
  observeEvent(DUEenv$doseTicks, {
               DUEenv$log10doseValues = seq(log10(min(DUEenv$doseTicks)), 
                                           log10(max(DUEenv$doseTicks)), 
                                           length=25)
               DUEenv$doseValues = 10^DUEenv$log10doseValues 
  })
  
  write.probs.4 = function(P) {
    P = as.character(round(digits=2, P))
    if(P=='0') P = '0.00'
    if(nchar(P)==3) P=paste0(P, '0')
    P
  }
  ####output$skinny4probs####
  output$skinny4probs <- renderUI({
    bold_italic = 4
    probs = calculate.probabilities(
      DUEenv, log10dose=log10(DUEenv$selectedDose))
    div(
      div(
        style=paste('float: left; color:', rt.outcome.colors("Rt")),
        HTML('&nbsp;Rt'),
        write.probs.4(probs["Rt"])) ,
      div(
        style=paste('float: right; color:', rt.outcome.colors("rt")),
        'rt',
        write.probs.4(probs["rt"]), 
        HTML('&nbsp;')  ),
      br(),
      div(
        style=paste('float: left; color:', rt.outcome.colors("RT")),
        HTML('&nbsp;RT'),
        write.probs.4(probs["RT"]) ),
      div(
        style=paste('float: right; color:', rt.outcome.colors("rT")),
        'rT',
        write.probs.4(probs["rT"]), 
       HTML('&nbsp;') )
      )
  })
  output$linePlot <- renderPlot(
    height=reactive(ifelse(!is.null(input$innerWidth),
                           input$innerWidth*0.25,700)),
    width=reactive(ifelse(!is.null(input$innerWidth),
                          input$innerWidth*0.25,700)),
    expr = {
    plotProbsAndEU(DUEenv=DUEenv, context='shiny')
  })
  
  # summarycolumns is not used
  summarycolumns = strsplit('highest.EU,
                          OptDose.EU,
                         EUatMTDdose,
                         MTDdose,
                         lowest.EU,
                         highest.Rt,
                         best.dose.Rt,
                            TatMTDdose,
      ', 
                         ',([\n \t])*'
  )[[1]]
  
  output$utilitySummaries <- renderTable({
    EUcolumns = c(1,3,5)
    data.frame(EU = names(DUEenv$utilitySummaries)[EUcolumns],
               value = unlist(DUEenv$utilitySummaries[1,EUcolumns])
    )
  })
  
  output$probSummaries <- renderTable({
    dosecolumns = c(6,8)
    data.frame(prob = names(DUEenv$utilitySummaries)[ dosecolumns],
               value = unlist(DUEenv$utilitySummaries[1, dosecolumns])
    )
  })
  output$doseSummaries <- renderTable({
     dosecolumns = c(2,4,7)
    data.frame(dose = names(DUEenv$utilitySummaries)[ dosecolumns],
               value = unlist(DUEenv$utilitySummaries[1, dosecolumns])
    )
  })
  ####Plotting Threshold Contour####
  output$ThresholdContour<- renderPlot(
    height=reactive(ifelse(!is.null(input$innerWidth),
                             input$innerWidth*0.28,500)),
    width=reactive(ifelse(!is.null(input$innerWidth),
                            input$innerWidth*0.28,500)),
    ### See https://stackoverflow.com/questions/40538365/r-shiny-how-to-get-square-plot-to-use-full-width-of-panel/40539526#40539526
    expr =     {
    input$okWillLoadSelectedFile  ### Attempt to force the plot.
    DUEenv$the.CVs.pop
    DUEenv$the.correlations.pop
    cexQ = 4; OKfont = c("sans serif", "bold")
    isolate(recalculate.means.and.variances(DUEenv))
    ### must isolate the recalculate, or else infinite loop
    gridPoints = DUEenv$log10doseValues 
    the.grid = as.matrix(expand.grid(gridPoints, gridPoints) )
    the.dmvnorms = apply(as.array(1:DUEenv$nPops), 1, function(i) {
      #cat("plotThresholdContour: the.medianThresholds.pop[[i]] = ", DUEenv$the.medianThresholds.pop[[i]], "\n")
      return(DUEenv$proportions[i] * dmvnorm(the.grid, mean = DUEenv$the.logmedians.pop[[i]]/log(10),
                                             sigma = DUEenv$the.variances.pop[[i]]))
    })
    the.dmvnorms = array(the.dmvnorms, dim = c(nrow(the.grid),
                                               DUEenv$nPops))
    contour.values = matrix(apply(the.dmvnorms, 1, sum), nrow = length(gridPoints) )
    contour(DUEenv$doseValues, DUEenv$doseValues, contour.values,
            xlim = range(DUEenv$doseValues), 
            ylim = range(DUEenv$doseValues),
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
    axis(1, at = DUEenv$doseTicks,
         cex.axis=1.0) #, cex.lab=3
    #font.axis=2, font.lab=2, family="Arial")
    axis(2, at = DUEenv$doseTicks,
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
  
  #### loadModalUI for Select a parameter file.   ####
  loadModalUI <- function(failed = FALSE) {
    #tagAppendAttributes(#id='myloadModal',
    fileChoices = rev(dir('.', pattern = 'DUE.*rdata'))
    previouslySelected = isolate(DUEenv$lastFileLoaded)  # not the number but the actual string.
    modalDialog(#style="width:4000px",
      size="l", #fade=TRUE,
      strong("README for this selection"),
      #textOutput('READMEoutput'),
      shiny::hr(),
      selectInput(inputId = 'chooseFile', 
                  label = 'Select a parameter file', 
                  choices = fileChoices,
                  selected = NULL, #### initially, no pre-selected row. Maybe 1 is ok? ####
                  size=40,
                  width='800px',
                  selectize=FALSE)
      ,
      footer = tagList(
        modalButton(label = "Cancel"),
        actionButton(inputId = "okWillLoadSelectedFile", label = "OK, load it")
      )
    )
  }
  
  observeEvent(
    input$load,
    {
      showModal(ui = loadModalUI())
    }
  )
  
  #### Loading parameter <---> input name map ####
  load('nameMap.rdata')
  parName = function(inputName) {
    switch(EXPR = inputName,
           thisPopFraction='proportions'
           ,probRefractory='refractory'
           ,responseLimitingTox='Kdeath'
           ,correlation='the.correlations.pop'
           ,thetaR.CV='the.CVs.pop'
           ,thetaRmedian='the.medianThresholds.pop'
           ,thetaT.CV='the.CVs.pop'
           ,thetaTmedian='the.medianThresholds.pop',
           inputName)
  }

  observeEvent(input$loadthatfile, loadMyfile())
                 
  #### okWillLoadSelectedFile ####
  observeEvent(input$okWillLoadSelectedFile, 
               priority = 1, 
               {
                 loadMyfile()
                 removeModal()
                 loadMyfile()
                 showModal(ui = loadModalUI())
                 removeModal()
                 showModal(ui = loadModalUI())
                 removeModal()
               }
  )
  
  setLineBoxes = function(probLineWidths) {
    ## already copied to DUEenv
    for(label in probLineNames()) {
      whichWidth = which(
        probLineWidths[label] == probLineWidthChoices)
      if(is.null(whichWidth) | ! is.element(whichWidth, 1:3))
        whichWidth = 1
      inputId = paste0('linethickness_', label)
      updateButton(session, inputId, 
                   label=switch(whichWidth, `1`=paste0('-',label,'-'),
                                `2`=label, `3`=HTML(paste0('<b>',label,'</b>'))),
                   size = switch(whichWidth, `1`='small',
                                 `2`='', `3`='large'))
      
    }
  }
  
  observeEvent(input$gotoOptDose, {
    OptDose.EU = round(DUEenv$utilitySummaries['T', 'OptDose.EU'])
    updateNumericInput(session, inputId = 'selectedDose', 
                       value = OptDose.EU, step = DUEenv$doseDelta )
    DUEenv$selectedDose = OptDose.EU
  })
  loadMyfile = reactive({ 
                 #try({
                 load(input$selectingAFile)   
    #### Will pull in DUEsaving and README ####
                 DUEsaving[['thisPop']] = 1   # input$thisPop
                 DUEsaving[['whichFollows']] = 2
                 DUEsaving$showRed33 = FALSE
                 DUEsaving$addProbsToQuadrants = TRUE
                 DUEsaving$xpos = 0.5 ##  for location of probabilities in threshold quadrant
                 DUEsaving$ypos = 2.2 ##  for location of probabilities in threshold quadrant
                 DUEsaving$doseDelta = 10  # for numericInput
                 misMatches = character(0)
                 if(names(DUEsaving$probLineWidths)[7]=='EU')
                   DUEsaving$probLineWidths = DUEsaving$probLineWidths[c(1:6,8,7)]
                 names(DUEsaving$probLineWidths) = probLineNames()
                 for (n in names(DUEsaving))
                   DUEenv[[n]] = DUEsaving[[n]]
                 #  Because old files use RLE instead of RLT.
                 for(inputName in strsplit(
                   "selectedDose nPops thisPop thisPopFraction whichFollows probRefractory responseLimitingTox correlation thetaR.CV thetaRmedian thetaT.CV thetaTmedian U.rt U.rT U.Rt U.RT"
                   , split=" ")[[1]] ) {
                   # cat("--", inputName, '\n')
                   parValue = DUEenv[[parName(inputName)]]
                   if(inputName == 'selectedDose' & is.null(parValue))
                     parValue = DUEenv[['favoriteDose']]
                   misMatch = FALSE
                   tryResult = try( {
                     if(length(parValue) == 1)  { ### a single number; but careful, what if nPops=1?
                       updateNumericInput(session, inputName, value=parValue)
                       shiny:::.getReactiveEnvironment()$flush()
                       if(input[[inputName]] != parValue) misMatch = TRUE
                     }
                     else {
                       if(is.list(parValue)) { ### 
                         RorT = match(substr(inputName, 1, 6),  c('thetaR', 'thetaT'))
                         updateNumericInput(session, inputName, value=parValue [[DUEenv$thisPop]] 
                                            [ RorT ])
                         shiny:::.getReactiveEnvironment()$flush()
                         if(input[[inputName]] != parValue[[DUEenv$thisPop]] [ RorT ])
                           misMatch = TRUE
                       }
                       else {
                         updateNumericInput(session, inputName, value=parValue [DUEenv$thisPop])
                         shiny:::.getReactiveEnvironment()$flush()
                         if(input[[inputName]] != parValue[DUEenv$thisPop]) 
                           misMatch = TRUE
                       }
                     }
                     if(misMatch) misMatches = c(misMatches, misMatch)
                   })  #  end of "try"
                   shiny:::.getReactiveEnvironment()$flush()
                   DUEenv$misMatches = misMatches
                   # if (misMatch) {
                   #   cat("====== misMatch ", inputName, " ", parName(inputName), 
                   #       ifelse(class(tryResult) == 'try-error', "Ooops", "OK") , "\n")
                   #   # cat("in DUEsaving\n"); print(parValue)
                   #   cat("in DUEenv\n"); print(DUEenv[[parName(inputName)]])
                   #   cat("in numeric input\n"); print(input[[inputName]])
                   # }
                   
                 }  # end of "for" loop
                 DUEenv$lastFileLoaded = isolate(input$chooseFile)
               })
  
  
  output$lastFileLoaded = renderUI({
  #  input$okWillLoadSelectedFile   ### the trigger.
    input$loadthatfile
    isolate({
      DUEenv$lastFileLoaded = input$selectingAFile
      #### not the file row number but the actual string. ####
      if(!is.null(input$selectingAFile)) {
        updateSelectInput(session, 'selectingAFile', selected = DUEenv$lastFileLoaded)
        div("Last file loaded:", br(),
            HTML(paste(gsub('##------ | ------##', "<br>", DUEenv$lastFileLoaded)))
        )
      }
      else NULL
    })
  })
  
  #### saving parameter data ####
  observeEvent(
    input$openSave, {
      showModal(modalDialog(
        textInput(inputId = 'shortName', label = 'Short Description'),
        textAreaInput(inputId = 'README', label = 'Reason for saving:'),
        bsButton(inputId = 'saveFile', 'Save file'),
        title = 'Save current parameter settings'
      ) )
    }
  )
  
  observeEvent(
    input$saveFile,
    isolate({
      updateButton(session, 'saveFile', style = 'success')
      DUEsaving = new.env()
      for (n in names(DUEenv))
        DUEsaving[[n]] = DUEenv[[n]]
      fileName <- paste0('DUEsaved', timestamp(stamp = Sys.time()), input$shortName, '.rdata')
      # print(fileName)
      README = input$README
      save(DUEsaving, README,
           file = fileName)
      newchoices = rev(dir('.', pattern = 'DUE.*rdata'))
      updateSelectInput(session, inputId = 'selectingAFile',
                        #size=length(dir('.', pattern = 'DUE.*rdata')))
                        choices=newchoices)
                        #  Sadly, no "size" parameter for updateSelectInput
      message <- list(size = length(newchoices))
      session$sendInputMessage('selectingAFile', message)
      ## ok doesn't crash but doesn't make box bigger either.
      removeModal()
    })
  )
  observeEvent(
    input$selectingAFile, 
    {
      load(input$selectingAFile)
      DUEenv$READMEtext = ifelse(exists('README'), README, "")
      cat("README is ", DUEenv$READMEtext, '\n')
    }
  )
  
  output$READMEoutput<-renderText({
    DUEenv$READMEtext
    #HTML(paste0("<font size=14> ", DUEenv$READMEtext, " </font"))
  })
  
  observeEvent(
    input$doseComparison,
    {showModal(
      modalDialog(
        fluidRow(
          column(3, 
                 h5('Optimal Dose')
          ),
          column(3, 
                 h5('Dose at 33% Toxicity Prob')
          )
        ),
        fluidRow(
          column(3,
                 verbatimTextOutput('optimalDoseValue')
                 #output$optimalDoseValue<-renderPrint(the point of intersection between the dotted black line and EU line)
          ),
          column(3,
                 verbatimTextOutput('oneThirdDoseValue'))
          #output$oneThirdDoseValue<-renderPrint(the point of intersection between dotted Peru line and EU line)
        )
      )
    )
    }
  )
  
  
  # NOT NEEDED:
  # observeEvent(input$Info, {
  #   htmlFile = system.file(package="DUE", "DUEshiny/www/DUE_vignette.html") 
  #   htmlFile = 'www/DUE_vignette.html'   ### shinyapps.io can see either file.
  #   # system(paste0('open ', htmlFile ))
  #   # system('open https://trials.shinyapps.io/DUEshiny/www/DUE_vignette.html' )
  #   # no error but doesn't work.
  #   #browseURL( htmlFile ) #### Works at home, fails at shinyapps.io  (disabled)
  #   # True, but the button is linked to the doc as a URL. So ok.
  # })
  
  #### phase1Results ####
  output$phase1Results = renderTable({
    cat('renderTable DUEenv$phase_one_result\n')
    DUEenv$phase_one_result
  })
  #### selectPhase1doses ####
  output$selectPhase1doses = renderUI({
    if(is.null(DUEenv$phase1Doses)) 
      DUEenv$phase1Doses =
        DUEenv$phase_one_result$doses = 
        DUEenv$doseTicks
    textInput('phase1Doses', label = 'Phase 1 doses', 
              value = paste(DUEenv$phase1Doses, collapse=' ')
    )
  })
  
  #### When phase1Doses changes, save new doses and enable button.####
  observeEvent(input$phase1Doses, {
    newPhase1Doses = try( {
      cat('input$phase1Doses changed to ', input$phase1Doses, '\n')
      cat('DUEenv$phase1Doses changed to ', DUEenv$phase1Doses, '\n')
      as.numeric(unlist(strsplit(input$phase1Doses, ' ') ) )
    })
    if(class(newPhase1Doses) != 'try-error') {
      DUEenv$phase1Doses = newPhase1Doses[!is.na(newPhase1Doses)]
      updateButton(session, 'phase1Recalculate', disabled = FALSE)
    }
    # else
    #   updateButton(session, 'phase1Recalculate', disabled = TRUE)
    
  })
  
  setDoses = function(newdoses) {
    DUEenv$phase1Doses = newdoses
  }
  
  ####  observeEvent(input$updateDoses... ####
  observeEvent(input$updateDoses, {
    DUEenv$doseTicks = DUEenv$phase1Doses
    print('DUEenv$phase1Doses =', DUEenv$phase1Doses)
  })
  
  updatePhase1 = reactive({
    if(is.null(DUEenv$phase1Doses))
      setDoses(DUEenv$doseTicks) 
    doses = DUEenv$phase1Doses
    #print(doses)
    probabilityVectors = sapply(log10(doses), 
                                calculate.probabilities, 
                                DUEenv=DUEenv, utility=DUEenv$utility
    ) 
    toxProbabilities = probabilityVectors['T', ]
    EU = probabilityVectors['EU', ]
    phase_one_summary = as.data.frame(
      phase_one_exact(PrTox = toxProbabilities) )
    print(toxProbabilities)
    #print(phase_one_summary)
    #show the expected expected utility across all enrolled patients.
    # 3 enter initially, plus 3 more if neither Term_1 nor Go_1.
    DUEenv$EN_pts = EN_pts = 
      phase_one_summary$pr_enter_tier *
      (3 + 3*(1-phase_one_summary$pr_Term_1
              - phase_one_summary$pr_Go_1) ) 
    DUEenv$expected_total_EU = sum( EN_pts * EU )
    DUEenv$expected_average_EU = sum( EN_pts * EU )/sum(EN_pts)
    #show the expected expected utility of the Phase 1 MTD.
    # for now, stopping at the first tier means there is no MTD.
    #  and not stopping means MTD = highest tier
    phase_one_summary$probMTD = 
      c(phase_one_summary$pr_stop_at[-1],
        1 - sum(phase_one_summary$pr_stop_at) )
    DUEenv$expected_EU_at_MTD = sum(phase_one_summary$probMTD * EU)
    DUEenv$phase_one_result = 
      data.frame(doses=doses, pr_tox=toxProbabilities,
                 lapply(phase_one_summary, round, digits = 4 )
      )
  })
  
  
      ## standard 3+3 design
  ####  Show Phase1 modal window ####
  observeEvent(input$phase1ResultButton, {
    updatePhase1() # this solved the problem.
    if(input$phase1ResultButton > 0) {
      showModal(ui = 
                  modalDialog(
                    easyClose = TRUE, 
                    size="l", 
                    h2("Results of Phase 1 trials using the doses"),
                    uiOutput('selectPhase1doses'),
                    fluidRow(  # fluidRow doesnt work here.
                      column(6, textOutput('phase1Doses')),
                      column(6, actionButton('phase1Recalculate', 'recalculate', enabled=FALSE)),
                      column(6, actionButton('updateDoses', 'update doses on main graph', enabled=TRUE))
                    ),
                    shiny::hr(),
                    tagAppendAttributes(style="text-size:larger",
                                        tableOutput('phase1Results')),
                    shiny::hr(),
                    h2('Probability of stopping ("pr_stop_at")'),
                    plotOutput('phase1plot'),
                    h2("Expected total EU across all enrolled patients:",
                       textOutput('ID_expected_total_EU') ),
                    h2("Expected average EU across all enrolled patients:",
                       textOutput('ID_expected_average_EU') ),
                    h2("Expected EU for one patient at MTD:",
                       textOutput('ID_expected_EU_at_MTD') ),
                    footer = tagList(
                      modalButton(label = "Cancel")
                    )
                  )
      )} 
  })
  
  ### phase 1
  # observeEvent(list(doseValues, utilities),
  #              {
  #                #result = data.frame(pr_enter_tier, pr_Go_1, pr_Term_1, pr_Go_2, pr_stop_at)
  #               sapply()
  #                DUEenv$expected_total_EU
  #                DUEenv$expected_average_EU
  #                DUEenv$expected_EU_at_MTD
  #              })
  # 
  output$ID_expected_total_EU = 
    renderText({round(digits=2, DUEenv$expected_total_EU)})
  output$ID_expected_average_EU = 
    renderText({round(digits=2, DUEenv$expected_average_EU)})
  output$ID_expected_EU_at_MTD = renderText({round(digits=2, DUEenv$expected_EU_at_MTD)})
  
  output$phase1plot = renderPlot({
    plot(DUEenv$phase_one_result$doses, DUEenv$phase_one_result$pr_stop_at,
         log='x', cex=3, lwd=2, type='b', axes=F, xlab='', ylab='')
    axis(side = 1, at = DUEenv$phase_one_result$doses, lwd = 2)
    mtext('Dose at which trial stops', side = 1, line = 3, cex = 2)
    axis(side = 2, lwd = 2)
    mtext('Probability', side = 2, cex = 2, line=3)
    lines(DUEenv$phase_one_result$doses, DUEenv$phase_one_result$pr_stop_at,
          col='blue', lwd=3)
    mtext('MTD (blue)', side = 4, line = 3, cex = 2)
    
  })
  
  observeEvent(
    input$SaveLoadMainToggle, 
    {
      try(silent = TRUE,
          {updateCheckboxInput(session, 'SaveLoadCheckbox', 
                          value = input$SaveLoadMainToggle)
      })
    }
  )
  
  ### watch for race condition
  observeEvent(
    input$SaveLoadCheckbox, 
    {
      try(silent = TRUE,
          {updateCheckboxInput(session, 'SaveLoadMainToggle', 
                               value = input$SaveLoadCheckbox)
          })
    }
  )
  
  ####Changing axes#####
  observeEvent(
    input$changeAxes,
    {showModal(
      modalDialog(
        div(style = 'text-align:center',
            h4('Customize your dose strategy:')),
        fluidRow(
          style = 'text-align:center', column(4, offset = 4,
                                              numericInput('minDoseNumeric', "Dose minimum", value = min(DUEenv$doseTicks))
          )
        ),
        fluidRow(style = "text-align:center",
                 column(4, offset = 4,
                        numericInput('maxDoseNumeric', 'Dose maximum', value = max(DUEenv$doseTicks))
                 )
        ),
        fluidRow(style = "text-align:center",
                 column(4, offset = 4,
                        numericInput('nIncrements', 'Number of increments', value = 7)
                 )
        ),
        fluidRow(style = 'text-align:center',
                 bsButton(inputId = 'closeAxesModal', label = 'Update axes', size = 'medium')
        )
      )
    )
    }
  )
  
  #### observe closeAxesModal - when the modal window for setting the dose axes is closed ####
  observeEvent(
    input$closeAxesModal,
    {
      DUEenv$minDose = input$minDoseNumeric
      DUEenv$maxDose = input$maxDoseNumeric
      DUEenv$doseTicks =
        round(10^seq(log10(input$minDoseNumeric), 
                                      log10(input$maxDoseNumeric), 
                                      length= input$nIncrements), digits=1)
      updateButton(session, 'closeAxesModal', style = 'success')
      removeModal()
    }
  )
  
  #### SaveLoadPanel ####
  output$SaveLoadPanel = renderUI( {
    div(style="background:lightGrey",
        conditionalPanel(
          'input.SaveLoadCheckbox',
          fluidRow(
            #font-style: italic; font-weight: bold;; text-size:100%;
            style="text-align:center; background-color:light-grey;",
            ####Save/load inputs####
            column(1, offset=1, style='background:blue;',
                   bsButton(inputId = "openSave", 
                            label = HTML("<font color='blue' style='background:white;'>Save parameters</font>"))# , size = 'medium')
            ),
            ####  file selection ####
            column(2, offset=1, style='background:blue;', ### because text-align:center is not working
                   bsButton(inputId = "loadthatfile", 
                            label = HTML('<font color=blue >Load the file selected below</font>'))
            )
          ),
          fluidRow(
            style="background-color:light-grey; text-size:larger;  font-weight: bold;",
            column(2, offset=1, shiny::hr(), uiOutput(outputId = 'lastFileLoaded') ),
            column(4,  style="background-color:light-grey; ",
                   selectInput(inputId = 'selectingAFile', 
                               label=HTML("<span style='font-style: italic;'> Parameter files to select from:</span>"),
                               choices = currentChoices <- rev(dir('.', pattern = 'DUE.*rdata')),
                               selected = NULL,
                               size=length(currentChoices),
                               width='800px',
                               selectize=FALSE
                   )
            ),
            column(4, 
                   div(style="background-color:#F4FAFA",
                       HTML("<span style='font-size: 16; '> README for this selection </span>"),
                       shiny::hr(),
                       tagAppendAttributes(style="font-size: 16; ", textOutput('READMEoutput'))
                   )
            )
          )
        )
    )
  } )  ### end SaveLoadPanel
  
  #   https://getbootstrap.com/javascript/#popovers-options
  # addPopover(session, 'thresholdSide', title="Threshold side", 
  #            content="Joint distribution of thresholds for response and toxicity", placement = 'top')
  
  addAllPopovers = function() {
    cat("addAllPopovers\n")
    output$JSstopPopups = renderUI({
      " "
    } )
    addPopover(session, 'popFilePanel', title="File panel: save and load parameters",
               content=paste(sep='<br>',
                             'Click toggle checkbox, left side of grey bar',
                             'for saving and loading current settings.'))
    
    #### popOvers for left side ####
    addPopover(session, 'popThresholdContour', title="Joint distribution", 
               content=paste(sep='<br>', 
                             "Patients have two thresholds, for R and for T.",
                             "This is a contour plot for their joint distribution.",
                             "Click to change the 'selected dose', moving the green cross.",
                             "This divides the region into the four outcome combinations.",
                             "The big numbers are at the medians for each sub-group.") )
    addPopover(session, 'pop_nPops', title="Number of sub-groups", 
               content=paste(sep='<br>', 
                             "Number of distinct sub-groups in the joint threshold distribution."))
    addPopover(session, 'popThisPop', title="Which sub-group", 
               content=paste(sep='<br>', 
                             'Which sub-group that inputs in this box are currently showing.' ))
    addPopover(session, 'popThisPopFraction', title="Proportion for this sub-group", 
               content=paste(sep='<br>',
                             'Proportion of this sub-group in the patient population.' ))
    addPopover(session, 'popWhichFollows', title="Which sub-group's proportion will change", 
               content=paste(sep='<br>',
                             '(Relevant if # sub-groups is 3 or more.)',
                             'If this proportion changes, which other sub-group must change so sum=1?' ))
    addPopover(session, 'popParamsThisPop', title="Parameters for this sub-group",  placement = 'top',
               content=paste(sep='<br>',
                             'Theta R Median & Theta T Median:',
                             'Median (on the dose scale) for this sub-group.',
                             'Theta R CV & Theta T CV:',
                             'Coefficient of variation (on the log-dose scale) for this sub-group.',
                             'Correlation:',
                             'Correlation of R and T <strong>log</strong> thresholds for this sub-group.'))
    addPopover(session, 'popRefractory', placement = 'top',
               title="Probability of a refractory tumor", 
               content=paste(sep='<br>',
                             'Accounting for a proportion totally resistant to treatment,',
                             'regardless of which sub-group the patient is in.' ))
    addPopover(session, 'popRLT', placement = 'top',
               title='Response-limiting toxicity event (RLT)',
               content=paste(sep='<br>',
                             'RLT represents the case where a patient with a low',
                             ' threshold for toxicity has enough toxicity to prevent response, even if',
                             'the response threshold is low enough. ',
                             'This parameter is the #orders of magnitude between', 
                             'the upper and lower boundaries of the RT region. ',
                             'Below that, the patient will experience rT instead.',
                             'A number > 10 means every RT stays RT.',
                             'Zero means that every RTs converts to rT.')
    )
    #### popOvers for central column ####
    addPopover(session, 'pop_selectedDose', title="Dose", 
               content="Select a dose by clicking on plot (left),<br>or type or scroll here.")
    addPopover(session, 'popDoseAxes', title="Dose axes", 
               content=# div(style="text-width:200px;", 
                 paste(sep='<br>', 
                       "Set both axes in left plot, and horizontal axis in right plot.",
                       "Also used as dose tiers for Phase I trial (button below).") ) #)
    addPopover(session, 'popPhaseI', title="Phase I stopping", 
               content=paste(sep='<br>', 
                             "Click here to see the stopping probabilities",
                             'of a 3x3 trial with dose tiers = dose axis ticks.') )
    addPopover(session, 'popFileToggle', title="Toggle file panel (above)", 
               content=paste(sep='<br>', 
                             "Opens and closes the panel at the top",
                             "for saving and loading current settings.") )
    
    addPopover(session, 'popPopoverToggle', title="Toggle the popovers", 
               content=paste(sep='<br>', 
                             "Starts and stops", 
                             " the little popover windows",
                             "full of help") )
    
    #### popOvers for right side ####
    #h4('EU values of interest'),
    #tableOutput('utilitySummaries'),
    
    addPopover(session, 'utilitySummaries', 
               title="Utility summaries", 
               content='Expected utilities of interest') 
    addPopover(session, 'doseSummaries', 
               title="Dose summaries", 
               content='Info about doses of interest') 
    addPopover(session, 'probSummaries', 
               title="Probability summaries", 
               content='Probabilities of interest') 
    addPopover(session, 'popLineThickness', title="Line thickness buttons",  
               content=paste(sep='<br>',
                             'Each button is a three-way toggle for a plot line. ',
                             'Invisible (small box, line name in parentheses).',
                             'Thin line (small box, no parentheses)',
                             'Thick line (large box)',
                             'Click box to cycle through these 3 choices.' ))
    addPopover(session, 'linePlot', title="Plot of outcomes and E(U).",  
               content=paste(sep='<br>',
                             'Dose-probability curves:',
                             '---for R (total response), T (total toxicity),',
                             '---for the 4 quadrant outcomes and RLT, and ',
                             '---for E(U), the expected utility.' ))
    addPopover(session, 'popUtilities', title="Utility values for each outcome",  placement = 'top',
               content=paste(sep='<br>',
                             'Preset buttons in the bottom row.',
                             'You can also modify the values in the top row. ',
                             'The E(U) curve above will respond to utility changes.' ))
    addPopover(session, 'popCustomUtilities', title="Custom Utilities",  placement = 'top',
               content=paste(sep='<br>',
                             'You can change values by typing ',
                             'or by clicking the little up&down arrows.'
               ))
    addPopover(session, 'popPresetUtilities', title="Preset Utility Assignments",  placement = 'top',
               content=paste(sep='<br>',
                             'Additive: +1 for Response, -1 for Toxicity',
                             'Simple:  +1 for Rt',
                             'Aggressive: U(RT) = +1  (toxicity doesn\'t matter)',
                             'Cautious: U(RT) = -1  (response doesn\'t matter)' ))
    addPopover(session, 'popDebugging', title="Debugging panel",  placement = 'top',
               content=paste(sep='<br>',
                             'Click toggle checkbox, left side of grey bar, to open debugging panel.',
                             'See <a href="http://www.github.com/professorbeautiful/shinyDebuggingPanel"> www.github.com/professorbeautiful/shinyDebuggingPanel </a>for details.'))
  }
  stopAllPopovers = function(){
    cat("stopAllPopovers\n")
    output$JSstopPopups = renderUI({
      tags$script( "destroyAllPopovers();" )
    } )
  }
  stopOnePopover = function(pop) {
    scriptString = paste0(
      'stopOnePopoverString =', 
      '"$(\\"*[id=\\\'', pop, '\\\']\\").popover(\\\'destroy\\\');" ;',
      '   eval(stopOnePopoverString); '
    )
    cat(scriptString, '\n')
    output$JSstopPopups = renderUI({
      tags$script( scriptString )
      ## This works!  JS console shows an error, but it works.
      ## TypeError: null is not an object (evaluating 'a.$element.off')
    } )
  }
  
  
  observeEvent(input$togglePopovers, {
    cat('input$togglePopovers ', input$togglePopovers, '\n')
    #stopAllPopovers()
    if(input$togglePopovers == TRUE) {
      #output$JSstopPopups = NULL
      addAllPopovers()
    }
    else 
      stopAllPopovers()
    # $("*[id^='pop']")  # This works.
    # $("*[id^='pop']").popover('destroy')
    # https://stackoverflow.com/questions/20283308/want-to-enable-popover-bootstrap-after-disabled-it
  })
  
  observeEvent(input$ctrlDpressed, {
    try({
      if(input$ctrlDpressed)
        updateCheckboxInput(inputId = 'debugToolsCheckbox', value = TRUE)
    })
  }) # Seems to work to set checkbox. Also had to flush the ctrl-D press.
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
