# This is the main app page, which will run and read in all the other pages
# and modules, and render the dashboard

# Note: not necessary to load packages here: this all happens in global.R

source("global.R")
source("functions.R")
source("modules/main_page.R")
source("modules/waterquality.R")
source("modules/algae.R")
#source("modules/win.R")

ui <- dashboardPage(
  dashboardHeader(title = "Guana River Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Main Page", tabName = "main_page", icon = icon("home")),
      menuItem("Water Quality Data", tabName = "waterquality", 
               icon = icon("flask-vial", lib="font-awesome"),
               badgeLabel = "New!", badgeColor = "green"),
      menuItem("Harmful Algal Bloom Data", tabName = "algae", 
               icon = icon("microscope", lib = "font-awesome"),
               badgeLabel = "Coming soon", badgeColor = "yellow"),
      menuItem("Water Level Data", tabName = "waterlevel", 
               icon = icon("water", lib="font-awesome"),
               badgeLabel = "Under construction", badgeColor = "light-blue"),
      menuItem("Fish and Shellfish", tabName = "shellfish", 
               icon = icon("fish", lib="font-awesome"),
               badgeLabel = "Under construction", badgeColor = "light-blue"),
      menuItem("Terrestrial Animal Data", tabName = "animal", 
               icon = icon("paw", lib="font-awesome"),
               badgeLabel = "Under construction", badgeColor = "light-blue"),
      menuItem("Vegetation Data", tabName = "animal", 
               icon = icon("seedling", lib="font-awesome"),
               badgeLabel = "Under construction", badgeColor = "light-blue")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main_page", mainPageUI(id = "main_page")),
      tabItem(tabName = "algae", HABPageUI(id = "algae")),
      tabItem(tabName = "waterquality", WINPageUI(id = "waterquality"))
    )
  )
)

server <- function(input, output, session) {
  moduleServer(module = mainPageServer, id = "main_page", session = session)
  #WQPageServer("waterquality", parentSession = session)
  HABPageServer("algae", parentSession = session)
  WINPageServer("waterquality", parentSession = session)
}

shinyApp(ui, server)
