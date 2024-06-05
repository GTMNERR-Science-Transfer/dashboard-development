# This is the main app page, which will run and read in all the other pages
# and modules, and render the dashboard

# Note: not necessary to load packages here: this all happens in global.R

source("global.R")
source("modules/main_page.R")
source("modules/waterquality.R")
source("modules/algae.R")

ui <- dashboardPage(
  dashboardHeader(title = "Guana River Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Main Page", tabName = "main_page", icon = icon("home")),
      menuItem("Water Quality Data", tabName = "waterquality", icon = icon("link")),
      menuItem("Harmful Algal Bloom Data", tabName = "algae", icon = icon("link"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main_page", mainPageUI("main_page")),
      tabItem(tabName = "waterquality", WQPageUI("waterquality")),
      tabItem(tabName = "algae", subPageUI("algae"))
    )
  )
)

server <- function(input, output, session) {
  moduleServer(module = mainPageServer, id = "main_page", session = session)
  moduleServer(module = WQPageServer, id = "waterquality", session = session)
  moduleServer(module = subPageServer, id = "algae", session = session)
}

shinyApp(ui, server)
