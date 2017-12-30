library("shiny")
library("shinyWidgets")

ui <- fluidPage(
  actionButton(inputId = "go", label = "Launch long calculation"), #, onclick = "$('#my-modal').modal().focus();"
  
  # You can open the modal server-side, you have to put this in the ui :
  tags$script("Shiny.addCustomMessageHandler('launch-modal', function(d) {$('#' + d).modal().focus();})"),
  tags$script("Shiny.addCustomMessageHandler('remove-modal', function(d) {$('#' + d).modal('hide');})"),
  
  # Code for creating a modal
  tags$div(
    id = "my-modal",
    class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
    tags$div(
      class="modal-dialog",
      tags$div(
        class = "modal-content",
        tags$div(class="modal-header", tags$h4(class="modal-title", "Calculation in progress")),
        tags$div(
          class="modal-body",
          shinyWidgets::progressBar(id = "pb", value = 0, display_pct = TRUE)
        ),
        tags$div(class="modal-footer", tags$button(type="button", class="btn btn-default", `data-dismiss`="modal", "Dismiss"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  value <- reactiveVal(0)
  
  observeEvent(input$go, {
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0) # reinitialize to 0 if you run the calculation several times
    session$sendCustomMessage(type = 'launch-modal', "my-modal") # launch the modal
    # run calculation
    for (i in 1:10) {
      Sys.sleep(0.5)
      newValue <- value() + 1
      value(newValue)
      shinyWidgets::updateProgressBar(session = session, id = "pb", value = 100/10*i)
    }
    Sys.sleep(0.5)
    # session$sendCustomMessage(type = 'remove-modal', "my-modal") # hide the modal programmatically
  })
  
}

shinyApp(ui = ui, server = server)