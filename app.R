# This is the main app page, which will run and read in all the other pages
# and modules, and render the dashboard

# Note: not necessary to load packages here: this all happens in global.R

source("global.R")
source("modules/main_page.R")
source("modules/waterquality.R")
source("modules/algae.R")
source("modules/win.R")

ui <- dashboardPage(
  dashboardHeader(title = "Guana River Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Main Page", tabName = "main_page", icon = icon("home")),
      menuItem("Water Quality Data", tabName = "waterquality", icon = icon("link")),
      menuItem("Harmful Algal Bloom Data", tabName = "algae", icon = icon("link")),
      menuItem("Water Infrastructure Network", tabName = "win", icon = icon("bridge-water", lib="font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main_page", mainPageUI(id = "main_page")),
      tabItem(tabName = "waterquality", WQPageUI(id = "waterquality")),
      tabItem(tabName = "algae", HABPageUI(id = "algae")),
      tabItem(tabName = "win", WINPageUI(id = "win"))
    )
  )
)

server <- function(input, output, session) {
  moduleServer(module = mainPageServer, id = "main_page", session = session)
  WQPageServer("waterquality", parentSession = session)
  HABPageServer("algae", parentSession = session)
  WINPageServer("win", parentSession = session)
}

shinyApp(ui, server)
