quadrant_popThresholdContour.f = function() {
  div(
    h3("Thresholds: Joint Probability Density",  
       style='color:blue'),
    fluidRow(column(12, id='popThresholdContour',
                    style=defaultBackgroundColor,
                    plotOutput("ThresholdContour", 
                               click = 'click_threshold'
                               #, width="100%", height="100%"  
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
    )))
}
quadrant_popThresholdContour = quadrant_popThresholdContour.f()