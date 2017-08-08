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
                        Rt='#009215', RT='#7367D4', EU='#000000', RLE='#6C9291')
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


####UI starts here####
ui <- fluidPage(
  title = "Dose Utility Explorer",
  includeCSS('DUE.css'),
  uiOutput('SaveLoadPanel'),
  uiOutput('JSprimping'),
  titlePanel(div( style='text-align:center; color:blue;', 
                  paste("DUE Shiny app: date = ",
                        desc$Date, "  Version = ", desc$Version))),
  ####  LEFT SIDE: Contour plots ####
  fluidRow(style='text-align:center',
           column(5, 
                  h2("Joint Prob Density of Thresholds", br(), 
                     style='color:blue'),
                  fluidRow(style='text-align:center; text-color:blue;color:blue; font-size:150%;', 
                           column(4, offset=2, "R = response", br(), "r = non-response"), 
                           column(4, "T = toxicity", br(), "t = non-toxicity")
                  ),
                  #tagAppendAttributes(
                    # style="margin-left: 50%;
                    #   margin-right: -50%;
                    #   transform: translate(50%, 0%);",
                    #style="font-color:red;",
                  fluidRow(column(8, offset=2, #align='center',
                    plotOutput("ThresholdContour", 
                               click = 'click_threshold',
                               width="700px", height="700px")
                  )), 
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
                                  numericInput(inputId = "probRefractory", 
                                               HTML("<br>Pr(refractory tumor)"), value = .85, step = .1)),
                           column(6,
                                  # tagAppendAttributes(
                                  #   class='RLEtooltip',
                                    numericInput(inputId = "responseLimitingTox", 
                                                 HTML("RLE: log10 (response-limiting gap) <br> (RT->rT)"), value = .6)
                                  #)
                            , 
                            bsTooltip(id='responseLimitingTox', 
                                      title='so you wanna understand?', 
                                      placement = "top", 
                                      trigger = "hover",
                                      options = list(container = "body")) 
                           )
                  )
           )
           , 
           ####  MIDDLE: Doses and Files ####
           column(1, 
                  a(
                    href="DUE_vignette.html", rel="help", target="_blank",
                    span(
                      strong(em("Click for information:",
                                style="color:darkgreen; font-size:small"))
                      ,
                      actionButton(inputId = "Info", label="",
                                   style="background:lightgreen",
                                   icon=icon("info-sign", lib="glyphicon")))
                  ),
                  br(), br(),
                  br(), br(),
                  br(), br(),
                  br(), 
                  div(style=paste0(
                    "vertical-align:center;",
                    "border-left:1px solid #000;",
                    "border-bottom:1px solid #000;",
                    "border-top:1px solid #000;",
                    "border-right:1px solid #000;"),  ### height:1500px;
                    # See also https://stackoverflow.com/questions/571900/is-there-a-vr-vertical-rule-in-html
                    # especially the display:flex solution.
                    div(style='text-align:center; color:white; border-color:darkgreen; background-color:green;',
                        numericInput('favoriteDose', 'Selected dose', value=100, min=0)),
                    br(), br(),
                    div(style = "background-color:green;", 
                             #column(2, 
                             bsButton("changeAxes", HTML("Change <br> dose<br>axes"))
                             #)
                    ),
                    br(),
                    br(),
                    br(),
                    ####phase1resultbutton####
                    div(style=paste0("background-color:", "red"),
                        bsButton(inputId = "phase1ResultButton", label = HTML("Phase I <br> Results")
                        )
                    )
                    # hr(style = 'margin-top: 0.5em; margin-bottom: 0.5em; border-style:inset; border-width: 2px')
                    # fluidRow(style =  "font-size:large",
                    #          bsButton(inputId = "load", label = HTML("Load saved <br> parameters<br>(new window)"), size = 'medium')
                    # ),
                  )#,
                 # uiOutput(outputId = 'lastFileLoaded')
           ),
           ####  RIGHT SIDE: Probabilities and Expected Utility ####
           column(5, 
                  h2("Probabilities and Expected Utility, E(U)", style="color:blue")
                  , fluidRow(style='background-color:lightgrey;', column(2,  HTML("Line thickness controls")), 
                             linethicknessButtons)
                  , plotOutput("linePlot"),
                  div(
                    br(), br(), br(),
                    h3("Controller for utility values", style="text-align:center; color:blue"),
                    div(
                      fluidRow(style='background-color:lightgrey;',
                               fluidRow(
                                 column(4, 
                                        #        style="text-align:center; vertical-align:center;",                                        ,
                                        br(),br(),br(),br(),
                                        h3("Enter custom values here:", style="vertical-align:center;
                                            color:blue")),
                                 #column(2, HTML("&nbsp;")),
                                 column(6,
                                        fluidRow(
                                          column(offset=1, 5, h2("R", style="text-align:center;")),
                                          column(6, h2("r", style="text-align:center;"))
                                        ),
                                        fluidRow(
                                          #style='background-color:lightgrey;',
                                          column(2, br(), h2("t")),
                                          column(4,
                                                 tagAppendAttributes(
                                                   numericInput(inputId="U.Rt", "U.Rt", value=1),
                                                   style=paste0('color:', rt.outcome.colors['Rt'],
                                                                "; font-style:italic; font-size:200%;"
                                                   ))),
                                          column(4, offset=1,
                                                 tagAppendAttributes(
                                                   numericInput(inputId="U.rt", "U.rt", value=0),
                                                   style=paste0('color:', rt.outcome.colors['rt'],
                                                                "; font-style:italic; font-size:200%;"
                                                   )))
                                        ),
                                        fluidRow(
                                          #style='background-color:lightgrey;',
                                          column(2, br(), h2("T")),
                                          column(4,
                                                 tagAppendAttributes(
                                                   numericInput(inputId="U.RT", "U.RT", value=0),
                                                   style=paste0('color:', rt.outcome.colors['RT'],
                                                                "; font-style:italic; font-size:200%;"
                                                   ))),
                                          column(4, offset=1,
                                                 tagAppendAttributes(
                                                   numericInput(inputId="U.rT", "U.rT", value=-1),
                                                   style=paste0('color:', rt.outcome.colors['rT'],
                                                                "; font-style:italic; font-size:200%;"
                                                   )))
                                        )
                                 )
                                 
                               )
                               ,
                               hr(), br(), 
                               fluidRow(
                                 column(4, h3("or choose a preset option here:", style="color:blue")
                                 ),
                                 column(4, style=paste0('color:', rt.outcome.colors['RT']),
                                        #span( '⬋', style="font-size:200%;") ,   #SOUTH WEST BLACK ARROW Unicode: U+2B0B, UTF-8: E2 AC 8B)
                                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                        tagAppendAttributes(
                                          bsButton(inputId="Cautious", HTML("Cautious<br>U.RT=-1")),
                                          style=paste0('background-color:', rt.outcome.colors['RT'],
                                                       '; color:white;')),
                                        #                                    span('⬉', style="font-size:200%;") ,  #NORTH WEST BLACK ARROW  Unicode: U+2B09, UTF-8: E2 AC 89
                                        tagAppendAttributes(
                                          bsButton(inputId="Aggressive", HTML("Aggressive<br>U.RT=+1")),
                                          style=paste0('background-color:', rt.outcome.colors['RT'],
                                                       '; color:white;'))
                                 ),
                                 # we could also try transform: rotate(7deg);
                                 column(1, style=paste0('color:', rt.outcome.colors['rT']),
                                        # span(style=paste0('color:', rt.outcome.colors['rT']),
                                        #      '⬅︎') ,
                                        # # LEFTWARDS ARROW
                                        # Unicode: U+2190, UTF-8: E2 86 90,
                                        tagAppendAttributes(
                                          bsButton(inputId="Simple", HTML("Simple<br>U.rT=0")),
                                          style=paste0('background-color:', rt.outcome.colors['rT'],
                                                       '; color:white;')
                                        )
                                 ),
                                 column(3, #HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                        tagAppendAttributes(
                                          bsButton(inputId="Additive",
                                                   HTML("Additive<br>R=+1, T=-1")),
                                          style=paste0('background-color:black; color:white;'))
                                 ),
                                 br(), br()
                               ),
                               br()
                      )
                    )
                  )
           )
  ),
  hr(style = 'margin-top: 0.5em; margin-bottom: 0.5em; border-style:inset; border-width: 2px'),
shinyDebuggingPanel::withDebuggingPanel()
)

####Server starts here####

server <- function(input, output, session) {
  try(shinyDebuggingPanel::makeDebuggingPanelOutput() )
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
      eval(parse(text=(paste0(
        "DUEenv$", objname, " <- get('",
        objname, "', DUEenvironmentDefault$DUEinits.default)"
      ))) )
    DUEenv$bgWindow <- "darkblue"
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
    #cat("observed click on linethicknessButton ", label, "\n")
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
    {DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [1] = input$thetaRmedian
    }
  )
  
  observe({
    input$thetaTmedian
    isolate({
      DUEenv$the.medianThresholds.pop[[DUEenv$thisPop]] [2]= input$thetaTmedian
    })
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
  })
  
  observe({
    DUEenv$refractory= input$probRefractory
  })
  
  observe({
    DUEenv$Kdeath= input$responseLimitingTox
  })
  
  observe({
    save_options = options(warn = -1)  #ignore initial warning
    try({
      newFavoriteDose = mean(c(input$click_threshold$x, input$click_threshold$y))
      if (!is.na(newFavoriteDose)){
        updateNumericInput(session, 'favoriteDose', value = newFavoriteDose)
        DUEenv$favoriteDose = newFavoriteDose
      }
      options(save_options)
    })
  })
  
  observe({
    DUEenv$favoriteDose = input$favoriteDose
  })
  
  output$linePlot <- renderPlot({
    plotProbsAndEUsimplified(DUEenv)
  })
  ####Plotting Threshold Contour####
  output$ThresholdContour<- renderPlot({
    input$okWillLoadSelectedFile  ### Attempt to force the plot.
    DUEenv$the.CVs.pop
    DUEenv$the.correlations.pop
    cexQ = 4; OKfont = c("sans serif", "bold")
    isolate(recalculate.means.and.variances(DUEenv))
    ### must isolate the recalculate, or else infinite loop
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
    axis(1, at = DUEenv$doseTicks,
         cex.axis=1.0) #, cex.lab=3)
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
      hr(),
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
  loadMyfile = reactive({ 
                 #try({
                 load(input$selectingAFile)   #### Will pull in DUEsaving and README ####
                 DUEsaving[['thisPop']] = 1   # input$thisPop
                 DUEsaving[['whichFollows']] = 2
                 for (n in names(DUEenv))
                   DUEenv[[n]] = DUEsaving[[n]]
                 misMatches = character(0)
                 for(inputName in strsplit(
                   "favoriteDose nPops thisPop thisPopFraction whichFollows probRefractory responseLimitingTox correlation thetaR.CV thetaRmedian thetaT.CV thetaTmedian U.rt U.rT U.Rt U.RT"
                   , split=" ")[[1]] ) {
                   # cat("--", inputName, '\n')
                   parValue = DUEenv[[parName(inputName)]]
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
          #output$oneThirdDoseValue<-renderPrint(the point of intersection between dotted red line and EU line)
        )
      )
    )
    }
  )
  
  #### phase1Results ####
  output$phase1Results = renderTable({
    DUEenv$phase_one_result
  })
  
  observeEvent(input$phase1ResultButton, {
    if(input$phase1ResultButton > 0){
      DUEenv$phase1Doses = DUEenv$doseTicks  ### Temporary for testing
      doses = DUEenv$phase1Doses
      print(doses)
      toxProbabilities = sapply(log10(doses), 
                                calculate.probabilities, DUEenv=DUEenv, utility=DUEenv$utility
      ) ['T', ]
      DUEenv$phase_one_result = 
        data.frame(doses=doses,
                   round(digits = 3, phase_one_exact(PrTox = toxProbabilities) )
        )
      print(DUEenv$phase_one_result )
      ## standard 3+3 design
      showModal(ui = 
                  modalDialog(easyClose = TRUE, 
                              size="l", 
                              h2("Results of Phase 1 trials using the doses"),
                              textOutput('phase1Doses'),
                              hr(),
                              tagAppendAttributes(style="text-size:larger",
                                                  tableOutput('phase1Results')),
                              hr(),
                              h2('Probability of stopping ("pr_stop_at"'),
                              plotOutput('phase1plot'),
                              footer = tagList(
                                modalButton(label = "Cancel")
                              )
                  )
      ) 
    }
  })
  output$phase1plot = renderPlot({
    plot(DUEenv$phase_one_result$doses, DUEenv$phase_one_result$pr_stop_at,
         log='x', cex=3, lwd=2, type='b', axes=F, xlab='', ylab='')
    axis(side = 1, at = DUEenv$phase_one_result$doses, lwd = 2)
    mtext('Dose at which trial stops', side = 1, line = 3, cex = 2)
    axis(side = 2, lwd = 2)
    mtext('Probability', side = 2, cex = 2, line=3)
  })
  
  
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
  observeEvent(
    input$closeAxesModal,
    {
      DUEenv$minDose = input$minDoseNumeric
      DUEenv$maxDose = input$maxDoseNumeric
      DUEenv$nDoseTicks = input$nIncrements
      # DUEenv$doseValues [[1]] = input$minDoseNumeric
      # DUEenv$doseValues [[50]] = input$maxDoseNumeric
      DUEenv$doseValues = 10^seq(log10(input$minDoseNumeric), log10(input$maxDoseNumeric), length= DUEenv$nDoses)
      #DUEenv$doseTicks = seq(input$minDoseNumeric, input$maxDoseNumeric, length.out = input$nIncrements)
      DUEenv$doseTicks = round(10^seq(log10(input$minDoseNumeric), log10(input$maxDoseNumeric), length= input$nIncrements), digits=1)
      #DUEenv$nDoseTicks = input$nIncrements
      updateButton(session, 'closeAxesModal', style = 'success')
      removeModal()
    }
  )
  
  output$SaveLoadPanel = renderUI( {
    div(style="background:lightGrey",
        checkboxInput(inputId='SaveLoadCheckbox', value=FALSE,
                      label=em(strong("Save & load parameter files"))),
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
            column(2, offset=1, hr(), uiOutput(outputId = 'lastFileLoaded') ),
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
                   div(style="background-color:lightgrey",
                       HTML("<span style='font-size: 16; '> README for this selection </span>"),
                       hr(),
                       tagAppendAttributes(style="font-size: 16; ", textOutput('READMEoutput'))
                   )
            )
          )
        )
    )
  } )
}

shinyApp(ui = ui, server = server)
