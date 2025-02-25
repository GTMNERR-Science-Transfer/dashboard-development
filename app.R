# This is the main app page, which will run and read in all the other pages
# and modules, and render the dashboard

# 02/14/2025 New version that uses bslib instead of shinydashboard for creating
# the layout and theme (https://rstudio.github.io/bslib/index.html)

# Note: not necessary to load packages here: this all happens in global.R

source("global.R")
source("functions.R")
source("modules/main_page.R")
source("modules/waterquality.R")
source("modules/algae.R")
source("modules/explore.R")
#source("modules/win.R")

library(shiny)
library(bslib)

ui <- page_navbar(
  theme = bs_theme(version=5, # Documentation recommends to hardcode this, in case 
                   # the versions / dependencies change or get updated 
                   bootswatch  = "sandstone", # See https://bootswatch.com/sandstone/ # or cosmo?
                   navbar_bg = "#174A7C",   # Dark blue color (adjust as needed)
                   navbar_fg = "white",      # White text for contrast
                   "navbar-dark" = TRUE),        # Ensures text is styled for dark background
  title = "Guana Estuary Data Dashboard",
  nav_panel(title = "Home",
            mainPageUI(id = "main_page")
            ),
  nav_panel(title = "Explore",
            icon = icon("binoculars", 
                        lib="font-awesome"),
            explPageUI(id = "explore")
  ),
  nav_panel(title = "Water Quality", 
            icon = icon("flask-vial", 
                        lib="font-awesome"), 
            WINPageUI(id = "waterquality")
            ),
  nav_panel(title = "Harmful Algal Blooms",
            icon = icon("microscope", 
                        lib = "font-awesome"),
            HABPageUI(id = "algae"), 
            #badge = badge("NEW!", color = "green")
  ),
  
  nav_panel(title = "Water Levels", 
            icon = icon("water", 
                        lib="font-awesome"),
            "Under construction", 
            #badge = badge("Under construction", color = "lightblue")
  ),
  
  nav_panel(title = "Fish and Shellfish", 
            icon = icon("fish", 
                        lib="font-awesome"),
            "Coming soon", 
            #badge = badge("Coming soon", color = "yellow")
  )#,
  # 
  # nav_panel("Terrestrial Animal Data", 
  #           "Under construction", 
  #           #badge = badge("Under construction", color = "lightblue")
  # ),
  # 
  # nav_panel("Vegetation Data", 
  #           "Under construction", 
  #           #badge = badge("Under construction", color = "lightblue")
  # )
)

server <- function(input, output, session) {
  moduleServer(module = mainPageServer, id = "main_page", session = session)
  explPageServer("explore", parentSession = session)
  HABPageServer("algae", parentSession = session)
  WINPageServer("waterquality", parentSession = session)
}

shinyApp(ui, server)



# ui <- dashboardPage(
#   dashboardHeader(title = "Guana Estuary Data Dashboard"),
#   dashboardSidebar(
#     sidebarMenu(id = "tabs",
#                 menuItem("MAIN PAGE", tabName = "main_page", icon = icon("home")),
#                 menuItem("Water Quality Data", tabName = "waterquality", 
#                          icon = icon("flask-vial", lib="font-awesome"),
#                          badgeLabel = "UPDATED!", badgeColor = "fuchsia"),
#                 menuItem("Harmful Algal Bloom Data", tabName = "algae", 
#                          icon = icon("microscope", lib = "font-awesome"),
#                          badgeLabel = "NEW!", badgeColor = "green"),
#                 menuItem("Water Level Data", tabName = "waterlevel", 
#                          icon = icon("water", lib="font-awesome"),
#                          badgeLabel = "Under construction", badgeColor = "light-blue"),
#                 menuItem("Fish and Shellfish", tabName = "shellfish", 
#                          icon = icon("fish", lib="font-awesome"),
#                          badgeLabel = "Coming soon", badgeColor = "yellow"),
#                 menuItem("Terrestrial Animal Data", tabName = "animal", 
#                          icon = icon("paw", lib="font-awesome"),
#                          badgeLabel = "Under construction", badgeColor = "light-blue"),
#                 menuItem("Vegetation Data", tabName = "animal", 
#                          icon = icon("seedling", lib="font-awesome"),
#                          badgeLabel = "Under construction", badgeColor = "light-blue")
#     ),
#     # Custom CSS to adjust the vertical position of the menu items
#     tags$style(HTML("
#       .main-sidebar {
#         display: flex;
#         flex-direction: column;
#       }
#       .sidebar-menu > li {
#         margin-top: 10px;
#         margin-bottom: 10px;
#       }
#     "))
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "main_page", mainPageUI(id = "main_page")),
#       tabItem(tabName = "algae", HABPageUI(id = "algae")),
#       tabItem(tabName = "waterquality", WINPageUI(id = "waterquality"))
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   moduleServer(module = mainPageServer, id = "main_page", session = session)
#   #WQPageServer("waterquality", parentSession = session)
#   HABPageServer("algae", parentSession = session)
#   WINPageServer("waterquality", parentSession = session)
# }
# 
# shinyApp(ui, server)