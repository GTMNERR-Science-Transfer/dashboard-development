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

# For some reason I need to specify my whole path to the data here (even though
# I am working in an Rproject)

# NOTE: .RData restores the object to the name it had when you saved it as .RData
load("~/Library/CloudStorage/OneDrive-UniversityofFlorida/NERRS project/App_dev/HAB.RData") 
# Alternatively, with .Rds you can give it a different name
#HAB_data <- readRDS("HAB.Rds")

############################################
#  Shiny user interface (ui)
############################################
ui <- fluidPage(
    # Layout for user options
    
    navbarPage(title = "FWC Harmful Algae Bloom (HAB) data",
               theme = bs_theme( version = 5, bootswatch = "cerulean" ),
               # Station dropdown menu
               div(style="display:inline-block;vertical-align:top;",
                   selectInput(inputId = "site",
                               label = "Location",
                               choices = Site, 
                               selected = "North Ponte Vedra", 
                               width = "350px")),
               
               # Horizontal space
               div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
               # Variable dropdown menu
               div(style="display:inline-block;vertical-align:top;",
                   selectInput(inputId = "variable", 
                               label = "Variable",
                               choices = vars,
                               selected = "cells/L*")),
            
               ###### ADD SPECIES HERE???    
               # # Horizontal space
               #  div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
               #  # Trend period radio buttons
               #  div(style="display:inline-block;vertical-align:top;",
               #      radioButtons( "trendperiod", "Trend Period"
               #                    , choiceNames  = list('5 years','10 years')
               #                    , choiceValues = list('5y','10y')
               #                    , selected = list('5y')
               #                    , inline = TRUE)),
               # ),
    # # Layout for map and plots
    # fluidRow(
    #     # Map occupies 1st column
    #     column( width = 5, leafletOutput("basemap",height=750),
    #     ),
    #     # GAM and trend plots occupy rows in the 2nd column
    #     column( width = 7, ecs.output("gam"),
    #             fluidRow(
    #                 column( width = 12, ecs.output("trend") )
    #             )
    #     )
    # )
)

#     # Application title
#     titlePanel("FWC HAB Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

############################################
#  Shiny server
############################################
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- HAB$`Temperature (C)`
        bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
                    length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Temperature (degrees Celsius)',
             main = 'Histogram of water temperatures')
    })
}

############################################
#  Shiny app
############################################
shinyApp( ui = ui, server = server )
