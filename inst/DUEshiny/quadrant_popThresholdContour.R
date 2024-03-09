quadrant_popThresholdContour.f = function() {
  div(
    h3("Thresholds: Joint Probability Density",  
       style='color:blue'),
    wellPanel( id='popThresholdContour',
                    style=defaultBackgroundColor,
                    plotOutput("ThresholdContour", 
                               click = 'click_threshold'
                               #, width="150%", height="150%"  
                               #No plot appears.
                               # , width="35vw", height="35vw"
                               # Looks ok but too big on zoom out.
                               # , width="700px", height="700px" 
                               # original. Bad on zoom in.
                               # , height=reactive(ifelse(!is.null(input$innerWidth),
                               #                        input$innerWidth*3/5,700))
                               # , width=reactive(ifelse(!is.null(input$innerWidth),
                               #                        input$innerWidth*3/5,700))
                               # OK, but belongs in SERVER renderPlot
                    )
               ,br()
               ,br()
               ,br()
    )
  )
}
quadrant_popThresholdContour = quadrant_popThresholdContour.f()