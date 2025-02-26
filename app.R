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

dash_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone"
) #|>
  # bs_add_variables(
  #   "navbar-bg" = "$primary",
  #   "navbar-color" = "$light",
  #   #"progress-bar-bg" = "$secondary",
  #   .where = "declarations"
  # ) |>
  # bs_add_rules("
  #   .navbar { color: var(--bs-light) !important; }
  #   .navbar .navbar-brand, .navbar .nav-link { color: var(--bs-light) !important; }
  # ")

ui <- page_navbar(
  theme = dash_theme,
  title = "Guana Estuary Data Dashboard",
  navbar_options = list(class = "bg-primary", theme = "dark"),
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
  ),
  
  nav_panel(title = "Water Levels", 
            icon = icon("water", 
                        lib="font-awesome"),
            "Under construction", 
  ),
  
  nav_panel(title = "Fish and Shellfish", 
            icon = icon("fish", 
                        lib="font-awesome"),
            "Coming soon", 
  )
)

server <- function(input, output, session) {
  moduleServer(module = mainPageServer, id = "main_page", session = session)
  explPageServer("explore", parentSession = session)
  HABPageServer("algae", parentSession = session)
  WINPageServer("waterquality", parentSession = session)
}

shinyApp(ui, server)