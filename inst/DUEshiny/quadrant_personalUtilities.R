#  quadrants
quadrant_personalUtilities.f = function() {
  div(id = 'personalUtilities',
      h3("Personal utility values", style="text-align:center; color:blue"),
      div(
        fluidRow(id = 'popCustomUtilities',
                 style=defaultBackgroundColor,
                 column(1, ""), 
                 column(11,  
                        fluidRow(
                          column(2, 
                                 #        style="text-align:center; vertical-align:center;",                                        ,
                                 br(),br(),br(),br(),br(),br(),
                                 h4(HTML("Enter <br>custom values <br>here:"), 
                                    style="vertical-align:center;
                                            color:blue")),
                          #column(2, HTML("&nbsp;")),
                          column(8,
                                 fluidRow(
                                   column(offset=1, 5, h2("R", style="text-align:center;")),
                                   column(6, h2("r", style="text-align:center;"))
                                 ),
                                 fluidRow(
                                   #style='background-color:#F4FAFA;',
                                   column(2, br(), h2("t")),
                                   column(4,
                                          tagAppendAttributes(
                                            numericInput(inputId="U.Rt", "U.Rt", value=1),
                                            style=paste0('color:', rt.outcome.colors('Rt'),
                                                         "; font-style:italic; font-size:200%;"
                                            ))),
                                   column(4, offset=1,
                                          tagAppendAttributes(
                                            numericInput(inputId="U.rt", "U.rt", value=0),
                                            style=paste0('color:', rt.outcome.colors('rt'),
                                                         "; font-style:italic; font-size:200%;"
                                            )))
                                 ),
                                 fluidRow(
                                   #style='background-color:#F4FAFA;',
                                   column(2, br(), h2("T")),
                                   column(4,
                                          tagAppendAttributes(
                                            numericInput(inputId="U.RT", "U.RT", value=0),
                                            style=paste0('color:', rt.outcome.colors('RT'),
                                                         "; font-style:italic; font-size:200%;"
                                            ))),
                                   column(4, offset=1,
                                          tagAppendAttributes(
                                            numericInput(inputId="U.rT", "U.rT", value=-1),
                                            style=paste0('color:', rt.outcome.colors('rT'),
                                                         "; font-style:italic; font-size:200%;"
                                            )))
                                 )
                          )
                        )
                        ,
                        shiny::hr(), 
                        h4("or choose a preset option here:", style="color:blue")
                        ,
                        br(), 
                        fluidRow(id = 'popPresetUtilities',
                                 column(2, offset=0, #HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                        tagAppendAttributes(
                                          bsButton(inputId="Additive",
                                                   HTML("Additive<br>R=+1, T=-1")),
                                          style=paste0('background-color:black; color:white;'))
                                 ),
                                 column(7, style=paste0('color:', rt.outcome.colors('RT')),
                                        #span( '⬋', style="font-size:200%;") ,   #SOUTH WEST BLACK ARROW Unicode: U+2B0B, UTF-8: E2 AC 8B)
                                        HTML("&nbsp;&nbsp;&nbsp;"),
                                        tagAppendAttributes(
                                          bsButton(inputId="Cautious", HTML("Cautious<br>U.RT=-1")),
                                          style=paste0('background-color:', rt.outcome.colors('RT'),
                                                       '; color:white;')),
                                        #                                    span('⬉', style="font-size:200%;") ,  #NORTH WEST BLACK ARROW  Unicode: U+2B09, UTF-8: E2 AC 89
                                        tagAppendAttributes(
                                          bsButton(inputId="Aggressive", HTML("Aggressive<br>U.RT=+1/2")),
                                          style=paste0('background-color:', rt.outcome.colors('RT'),
                                                       '; color:white;')),
                                        tagAppendAttributes(
                                          bsButton(inputId="Crazy", HTML("Crazy<br>U.RT=+1")),
                                          style=paste0('background-color:', rt.outcome.colors('RT'),
                                                       '; color:white;'))
                                 ),
                                 # we could also try transform: rotate(7deg);
                                 column(1, style=paste0('color:', rt.outcome.colors('rT')),
                                        ""
                                        # span(style=paste0('color:', rt.outcome.colors('rT')),
                                        #      '⬅︎') ,
                                        # # LEFTWARDS ARROW
                                        # Unicode: U+2190, UTF-8: E2 86 90,
                                 ),
                                 column(2,
                                        tagAppendAttributes(
                                          bsButton(inputId="Simple", HTML("Simple<br>U.rT=0")),
                                          style=paste0('background-color:', rt.outcome.colors('rT'),
                                                       '; color:white;')
                                        ))
                        )
                 )
        )
      )
  )
}
quadrant_personalUtilities = quadrant_personalUtilities.f()
