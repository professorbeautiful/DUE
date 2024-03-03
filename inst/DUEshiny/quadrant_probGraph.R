make_linethicknessButton = function(labelNum)
  column(1,
         tagAppendAttributes(
           bsButton(paste0('linethickness_', 
                           label<-probLineNames(labelNum)), label=label),
           style=paste0('color:', rt.outcome.colors(label), ';',
                        'border-color:', rt.outcome.colors(label), ';') ) 
  )
linethicknessButtons = 
  lapply(probLineNames(), make_linethicknessButton)   

quadrant_probGraph.f = function() {
  div(id = 'probGraph',
  h3("Probabilities and Expected Utility, E(U)", style="color:blue")
  , fluidRow(id = 'popLineThickness', 
             style=defaultBackgroundColor, 
             linethicknessButtons,
             column(4,  HTML("<b>Line thickness controls</b>")
                    )),
  hr()
  , fluidRow(id = 'popLinePlot',
             column(8, offset=0, #align='center',
                    plotOutput("linePlot",
                               click = 'click_dose'
                               #, height="700px", width="700px"
                    ) ),
             column(4, style=
                      paste0("background-color:", "#F4FAFA",
                             '; font-size:18px;'),
                    #h4('Doses of interest'),
                    tableOutput('doseSummaries'),
                    #h4('EU values of interest'),
                    tableOutput('utilitySummaries'),
                    tableOutput('probSummaries'))
  )
  
  )
}
quadrant_probGraph = quadrant_probGraph.f()
