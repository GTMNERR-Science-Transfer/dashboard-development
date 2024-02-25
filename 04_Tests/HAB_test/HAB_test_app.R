
# This is a Shiny web application. 

# Test with HAB data to create a dropdown menu for species
# Played around with a different theme as well

if(!require(shiny)){ install.packages("shiny") } ;  library(shiny)
if(!require(tidyverse)){ install.packages("tidyverse") } ;  library(tidyverse)

# Load the required dataset

# For some reason I need to specify my whole path to the data here (even though
# I am working in an Rproject)

# NOTE: .RData restores the object to the name it had when you saved it as .RData
load("~/github/GTMNERR Science Transfer/App_dev/03_Data_for_app/HAB.RData") 
# Alternatively, with .Rds you can give it a different name
#HAB_data <- readRDS("HAB.Rds")

############################################
#  Shiny user interface (ui)
# This part determines what users see on their
# screens
############################################
ui <- fluidPage(

    # Layout for user options
    navbarPage(title = "FWC Harmful Algae Bloom (HAB) data",
               theme = bslib::bs_theme(version = 5, bootswatch = "darkly" ),
               # Station dropdown menu
               div(style="display:inline-block;vertical-align:top;",
                   selectInput(inputId = "Species",
                               label = "Pick an algae species:",
                               choices = c("species 1" = "Odontella aurita",
                                           "species 2" = "Coscinodiscus sp.",
                                           "species 3" = "Tropidoneis sp."), 
                               selected = "Odontella aurita", 
                               width = "350px")),
               
               # # Horizontal space
               # div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
               # # Variable dropdown menu
               # div(style="display:inline-block;vertical-align:top;",
               #     selectInput(inputId = "variable", 
               #                 label = "Variable",
               #                 choices = vars,
               #                 selected = "cells/L*")
               # )
    ),

    # Application title
        titlePanel("FWC HAB Data"),

        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
            ),

            # Show a plot of the generated distribution
            mainPanel(
               plotOutput("distPlot")
            )
        )
    )
    
    ############################################
    #  Shiny server
    ############################################
    server <- function(input, output) {
        # Filter for algal species
        filtered_HAB <- reactive({
            selected_species <- input$Species
            filtered_data <- HAB[HAB$Species == selected_species, ]
            return(filtered_data)
        })
        
        output$distPlot <- renderPlot({
            species_data = filtered_HAB()
            species_name = input$Species
            # draw the histogram with the specified number of bins using input$bins from ui.R
            ggplot(species_data %>% filter(vars == "Temperature (C)"), aes(x = vals))+
                geom_histogram(bins = input$bins) +
                labs(title = paste0("Temperature distribution for ", species_name),
                     x = "Temperature (degrees Celsius)",
                     y = "Count")

        })
    }

############################################
#  Shiny app
############################################
shinyApp(ui = ui,
         server = server)
