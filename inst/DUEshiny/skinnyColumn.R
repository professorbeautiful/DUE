skinnyColumn.f = function() {
  div(id='skinnyColumnDiv',
    br(), br(), br(), 
    br(), br(), br(), 
    br(), br(), br(), 
    
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
      div(id='pop_selectedDose', 
          style='text-align:center; color:black; border-color:honeydew; background-color:honeydew;',
          numericInput('selectedDose', 'Selected dose', value=100, min=0, step=10)
          # , "Probabilities", uiOutput('showProbs')  # redundant now.
      ),
      div(style='text-align:center; color:black; border-color:honeydew; background-color:honeydew;',
          'Probabilities'),
      div(id='4probs',
          uiOutput('skinny4probs')),
      br(), 
      shiny::hr(style = 'margin-top: 0.5em; margin-bottom: 0.5em; border-style:inset; border-width: 2px'),
      actionButton(inputId = 'gotoOptDose', 
                   label = HTML('Go to <br>Optimal<br>Dose'), 
                   style='background-color:honeydew;'),
      br(),br(),
      div(style = "background-color:honeydew;", id='popDoseAxes',
          #column(2, 
          bsButton("changeAxes", HTML("Change <br> dose<br>axes"))
          #)
      ),
      br(),br(),
      
      ####phase1resultbutton###
      #column(2, 
      div(style=phase1backgroundcolor, 
          id='popPhaseI',
          bsButton(inputId = "phase1ResultButton", label = HTML("Phase I <br> Results")
          )
      #)
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
  
  