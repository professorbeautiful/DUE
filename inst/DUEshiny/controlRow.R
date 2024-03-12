controlRow.f = function() {
  fluidRow(
    div(style=paste0(
      "vertical-align:center;",
      "horizontal-align:center;",
      "text-align:center;",
      "border-left:1px solid #000;",
      "border-bottom:1px solid #000;",
      "border-top:1px solid #000;",
      "border-right:1px solid #000;"),  ### height:1500px;
      # See also https://stackoverflow.com/questions/571900/is-there-a-vr-vertical-rule-in-html
      # especially the display:flex solution.
      column(4, a(
        href="DUE_vignette.html", rel="help", target="_blank",
        ### must be in www.
        span(
          strong(em("Click for info:",
                    style="color:darkgreen; font-size:150%"))
          ,
          #  the action isn't used, only the URL href above.
          actionButton(inputId = "Info", label="",
                       style="background:yellow",
                       icon=tagAppendAttributes(
                         style="font-size: 3em;",
                         icon("info-sign", lib="glyphicon"))) )
      )),
      column(1, span('zoom in/out?', textOutput('zoomAdvice'))),
      column(2, checkboxInput(inputId='SaveLoadCheckbox', value=FALSE,
                    label=em(strong("Save & load parameter files")))
             ),
      column(3, ""),
      column(2, div(style=defaultBackgroundColor, id='popPopoverToggle',
                    checkboxInput(inputId = "togglePopovers", 
                                  label = HTML("<=Show/hide the helpful popovers"),
                                  value=FALSE))
      ),
      # div(style=defaultBackgroundColor, id='popFileToggle',
      #     checkboxInput(inputId = "SaveLoadMainToggle", 
      #                   label = HTML("Toggle <br> file <br> panel")
      #     )
      # )
      # hr(style = 'margin-top: 0.5em; margin-bottom: 0.5em; border-style:inset; border-width: 2px')
      # fluidRow(style =  "font-size:large",
      #          bsButton(inputId = "load", label = HTML("Load saved <br> parameters<br>(new window)"), size = 'medium')
      # ),
    )#,
    # uiOutput(outputId = 'lastFileLoaded')
 ) }
controlRow = 
  controlRow.f()
  
  