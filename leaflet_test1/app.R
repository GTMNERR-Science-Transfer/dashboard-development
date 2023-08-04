#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Guana Tolomato Matanzas NERR"),
    leafletOutput("map")
)

#### GTMNERR boundary and aquatic preserves ####
GTMNERR <- st_read("shapefiles/GTMNERR.shp")# CRS: Albers Conical Equal Area

GTMNERR <- st_transform(GTMNERR, crs = 4326)

# Define server logic required to draw the map
server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet(data = GTMNERR, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
            setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addPolygons(color = "purple", fill = NA) %>%
            addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
