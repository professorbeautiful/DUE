skinnyColumn.f = function() {
  div(
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
      textOutput('zoomAdvice'),
      a(
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
      ),
      div(style=defaultBackgroundColor, id='popPopoverToggle',
          checkboxInput(inputId = "togglePopovers", 
                        label = HTML("Show/hide <br> the helpful <br> popovers"),
                        value=FALSE)
      ),
      br(), br(), br(), 
      
      div(id='pop_selectedDose', style='text-align:center; color:black; border-color:honeydew; background-color:honeydew;',
          numericInput('selectedDose', 'Selected dose', value=100, min=0)
          # , "Probabilities", uiOutput('showProbs')  # redundant now.
      ),
      div(style = "background-color:honeydew;", id='popDoseAxes',
          #column(2, 
          bsButton("changeAxes", HTML("Change <br> dose<br>axes"))
          #)
      ),
      br(),
      br(),
      hr(), br(), br(),
      ####phase1resultbutton###
      div(style=phase1backgroundcolor, 
          id='popPhaseI',
          bsButton(inputId = "phase1ResultButton", label = HTML("Phase I <br> Results")
          )
      ),
      hr(),br(), br(),
      div(style=defaultBackgroundColor, id='popFileToggle',
          checkboxInput(inputId = "SaveLoadMainToggle", 
                        label = HTML("Toggle <br> file <br> panel")
          )
      )
      # hr(style = 'margin-top: 0.5em; margin-bottom: 0.5em; border-style:inset; border-width: 2px')
      # fluidRow(style =  "font-size:large",
      #          bsButton(inputId = "load", label = HTML("Load saved <br> parameters<br>(new window)"), size = 'medium')
      # ),
    )#,
    # uiOutput(outputId = 'lastFileLoaded')
  )}
skinnyColumn = 
  skinnyColumn.f()
  
  