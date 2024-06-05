subPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Harmful Algal Bloom Data"),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page")
  )
}

subPageServer <- function(input, output, session, parentSession) {
  ns <- session$ns
  observeEvent(input$go_back, {
    print("Go back button clicked")
    updateTabItems(parentSession, "tabs", selected = "main_page")
  })
}