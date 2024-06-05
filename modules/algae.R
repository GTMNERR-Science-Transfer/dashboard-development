HABPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Harmful Algal Bloom Data"),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page")
  )
}

HABPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
      })
  })
}