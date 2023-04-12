#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!require(shiny)){ install.packages("shiny") } ;  library(shiny)
if(!require(tidyverse)){ install.packages("tidyverse") } ;  library(tidyverse)

# Load the required dataset

# For some reason I need to specify my whole path to the data here (even though
# I am working in an Rproject)

# NOTE: .RData restores the object to the name it had when you saved it as .RData
load("~/Library/CloudStorage/OneDrive-UniversityofFlorida/NERRS project/App_dev/HAB.RData") 
# Alternatively, with .Rds you can give it a different name
#HAB_data <- readRDS("HAB.Rds")

############################################
#  Shiny user interface (ui)
# This part determines what users see on their
# screens
############################################
ui <- fluidPage(

    # Layout for user options
    
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
        
        output$distPlot <- renderPlot({
            # draw the histogram with the specified number of bins using input$bins from ui.R
            ggplot(HAB %>% filter(vars == "Temperature (C)"), aes(x = vals))+
                geom_histogram(bins = input$bins) +
                labs(x = "Temperature (degrees Celsius)",
                     y = "Count")

        })
    }

############################################
#  Shiny app
############################################
shinyApp(ui = ui,
         server = server)
