quadrant_popThresholdController.f = function(){
  div(
    h3("Controller for thresholds", style="text-align:center; color:blue"),
    fluidRow(id = 'pop_nPops',
             column(4, offset=4, div(
               style=paste(defaultBackgroundColor, '; align-items:center; text-align:center'),
               numericInput(inputId = "nPops",  "Number of groups", 
                            value = DUEinits.default$nPops, min=1))
             )
    ),
    br(),
    fluidRow( 
      style=paste(defaultBackgroundColor, 
                  '; vertical-align:center; min-height: 100%;'),
      column(4, id='popThisPop',
             numericInput(inputId = "thisPop", "This group #", 
                          value = 1, min = 1, max = DUEinits.default$nPops)),
      column(4, id='popThisPopFraction',
             numericInput(inputId = "thisPopFraction", "This group's proportion", 
                          value = DUEinits.default$proportions[DUEinits.default$thisPop],
                          min=0, max=1, step=0.1)),
      column(4, id = 'popWhichFollows',
             numericInput(inputId = "whichFollows", 
                          HTML("Dependent group #"), value = 2))
    ),
    shiny::hr(style='margin-top:1em; margin-bottom:1em; border-color:white'),
    br(),
    fluidRow(id = 'popParamsThisPop', 
             style=paste(defaultBackgroundColor, '; vertical-align:center; min-height: 100%;'),
             column(4, style=defaultBackgroundColor,
                    numericInput(inputId = "thetaRmedian", "Theta R Median", 
                                 value=DUEinits.default$the.medianThresholds.pop[[DUEinits.default$thisPop]][1]),
                    numericInput(inputId = "thetaR.CV", "Theta R CV", 
                                 value=DUEinits.default$the.CVs.pop[[DUEinits.default$thisPop]][1])
             ),
             column(4, 
                    #                             style='background-color:#F4FAFA; min-height: 100%; display: flex;
                    #    align-items: center; vertical-align:center;display:inline-block;vertical-align:middle;',  ### none of this works!
                    br(style='background-color:white;'),
                    div(style=defaultBackgroundColor, 
                        numericInput(inputId = "correlation", "Correlation", value = DUEinits.default$the.correlations.pop[1],
                                     min = -(1-0.01), max = 1-0.01, step = 0.1))
             ),
             column(4, style=defaultBackgroundColor,
                    numericInput(inputId = "thetaTmedian", "Theta T Median", 
                                 value=DUEinits.default$the.medianThresholds.pop[[DUEinits.default$thisPop]][2]),
                    
                    numericInput(inputId = "thetaT.CV", "Theta T CV", 
                                 value=DUEinits.default$the.CVs.pop[[DUEinits.default$thisPop]][2])
             )
    ),
    shiny::hr(style='margin-top:0em; margin-bottom:0em; border-color:white'),
    h3("Auxiliary parameters", style='color:blue;'),
    fluidRow(style=defaultBackgroundColor,
             column(6, id = 'popRefractory',
                    numericInput(inputId = "probRefractory", 
                                 HTML("<br>Pr(refractory tumor)"), 
                                 value = DUEinits.default$refractory, step = .1, min=0,max=1)
             ),
             column(6, id = 'popRLT',
                    # tagAppendAttributes(
                    #   class='RLTtooltip',
                    numericInput(inputId = "responseLimitingTox", 
                                 HTML("RLT: log10 (response-limiting gap) <br> (RT->rT)"), 
                                 value = DUEinits.default$Kdeath, step = 0.5, min=0)
                    #)
             )
    )
  )
}
quadrant_popThresholdController = quadrant_popThresholdController.f()