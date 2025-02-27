########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Chad Palmer
# Email: palmer.cr@ufl.edu
# University of Florida
# Last updated: see commit history

### Reef Data------------------------------
reefData <- readRDS("./03_Data_for_app/reefs.Rds")
reefPlotData <- reefData %>%
  group_by(Date, ReefID) %>%
  summarize(
    Live_Oyster = mean(`LiveCover_%`, na.rm = FALSE),
    Shell = mean(`ShellCover_%`, na.rm = FALSE),
    Box = mean(`BoxCover_%`, na.rm = FALSE),
    Substrate = mean(`SubstrateCover_%`, na.rm = FALSE),
    Other = mean(`OtherCover_%`, na.rm = FALSE),
    NoData = mean(`NoDataCover_%`, na.rm = FALSE),
    .groups = "drop"
  )

checkTotal <- reefPlotData %>%
  mutate(total = Live_Oyster + Shell + Box + Substrate + Other + NoData) %>%  # Compute total sum
  mutate(
    Live_Oyster = (Live_Oyster / total) * 100,
    Shell = (Shell / total) * 100,
    Box = (Box / total) * 100,
    Substrate = (Substrate / total) * 100,
    Other = (Other / total) * 100,
    NoData = (NoData / total) * 100
  ) %>%
  select(-total)  # Remove temporary total column

for(i in 1:length(checkTotal$NoData)){
  if(checkTotal$NoData[i]<0){
    checkTotal$NoData[i] = 0
  }
}

normalReefData <- checkTotal %>%
  mutate(total = Live_Oyster + Shell + Box + Substrate + Other + NoData) %>%  # Compute total sum
  mutate(
    Live_Oyster = (Live_Oyster / total) * 100,
    Shell = (Shell / total) * 100,
    Box = (Box / total) * 100,
    Substrate = (Substrate / total) * 100,
    Other = (Other / total) * 100,
    NoData = (NoData / total) * 100
  ) %>%
  select(-total)

### Count Data------------------------------
countData <- readRDS("./03_Data_for_app/counts.Rds")

#Averaging counts across date and reef
countPlotData <- countData %>%
  group_by(Date, ReefID) %>%
  summarize(
    Oysters = mean(`Oysters_#/0.0625m^2`, na.rm = FALSE),
    Barnacles = mean(`Barnacles_#/0.0625m^2`, na.rm = FALSE),
    Mussels = mean(`Mussels_#/0.0625m^2`, na.rm = FALSE),
    .groups = "drop"
  )

reef_locs <- reefData %>% 
  select(Lat, Long, Region, County, ReefID) %>% 
  distinct() %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = FALSE)
reef_locs <- reef_locs %>%
  mutate(labelID = paste(Region, ReefID, sep = " "))

SHELLPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Shellfish Data"),
    fluidRow(
      # First row - explanation
      column(width = 12,
             div(style = "margin-bottom: 20px;",
                 p(htmltools::HTML('This section provides an overview of shellfish data.
                 This page is still a work in progress and data displayed is being updated daily. <br>
                 A greater explaination of data shown and functionality will be displayed here once renovations are complete.'))
             )
      )
    ),
    fluidRow(
      #User inputs in 1st column
      column(width = 6,
             selectInput(ns("reefID"), 
                         label = "Select the ID for the Reef you want data for.", 
                         choices = c(unique(reef_locs$labelID))),
             #uiOutput(ns("selectStation")),
             sliderInput(
               inputId = ns("date_range"),
               label = "Select a Date Range",
               min = min(mdy(normalReefData$Date)), #NULL
               max = max(mdy(normalReefData$Date)), #NULL
               value = c(min(mdy(normalReefData$Date)), 
                         max(mdy(normalReefData$Date))),
               timeFormat = "%m/%d/%Y",
               width = "100%"
             ),
      ),
      # Map occupies 2nd column
      column(width = 6, 
             div(style = "margin-bottom: 20px;",
                 leafletOutput(ns("map"), height="350px"))
      )
    ),
    fluidRow(
      # Plot in the next row, below inputs and map
      column(width = 12, 
             plotOutput(ns("countPlot")), 
      )
    ),
    fluidRow(
      # Plot in the next row, below inputs and map
      column(width = 12, 
             plotOutput(ns("areaPlot")), 
      )
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page") #All input IDs need to be inside ns()
  )
}

SHELLPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns
    # output$selectStation <- renderUI(selectInput(ns("station"), 
    #                                              "Select what station you are interested in", 
    #                                              unique(HAB$Site[HAB$type %in% input$algae_type])))
    # output$selectDate <- renderUI(sliderInput(ns("date_range"), 
    #                                           "The following dates have data for your selected algae type. Set a range to narrow data on the map", 
    #                                           min = ymd(min(HAB$`Sample Date`[HAB$type %in% input$algae_type])), max = ymd(max(HAB$`Sample Date`[HAB$type %in% input$algae_type]))))
    # 
    ### Create the map upon startup -------------------------------
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
        clearBounds() %>%
        addTiles() %>%
        addPolygons(data = GTMNERR, color = "purple", fill = NA, 
                    weight = 2, opacity = 1, group = "GTMNERR boundaries") %>%
        addPolygons(data = counties_select, 
                    color = "black", weight = 2, opacity = 1,
                    fill = TRUE, fillColor = "white", fillOpacity = 0.01,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    group = "Counties", popup = ~NAME) %>%
        addMarkers(data = reef_locs, 
                   icon = blue_icon,  # Set default markers to blue
                   popup = ~paste("Region: ", Region, "<br>", "ID: ", ReefID),
                   group = "Reef") %>%
        addLayersControl(overlayGroups = c("Counties", "GTMNERR boundaries"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })
    
    observeEvent(input$reefID, {
      leafletProxy("map") %>%
        clearMarkers() %>%  # Clear existing markers before re-adding them
        addMarkers(
          data = reef_locs,
          lng = ~Long, lat = ~Lat,
          icon = ~ifelse(labelID == input$reefID, red_icon, blue_icon), # Corrected condition
          popup = ~paste("Region: ", Region, "<br>", "ID: ", ReefID),
          group = "Reef",
          popupOptions = popupOptions(direction = "auto", offset = c(0, -20))
        )
    })
    
    counts_Filtered <- reactive({
      countPlotData %>%
        filter(ReefID == reef_locs$ReefID[which(reef_locs$labelID == input$reefID)] & mdy(Date) >= input$date_range[1] & mdy(Date) <= input$date_range[2])
    })
    
    area_Filtered <- reactive({
      normalReefData %>%
        filter(ReefID == reef_locs$ReefID[which(reef_locs$labelID == input$reefID)] & mdy(Date) >= input$date_range[1] & mdy(Date) <= input$date_range[2])
    })
    
    #### Create plot ####
    
    output$countPlot <- renderPlot({
      
      df <- counts_Filtered()
      
      if (nrow(df) == 0) {
        print("Error: counts_Filtered() returned an empty dataset.")
      }
      
      # Convert to long format
      df_long <- df %>%
        pivot_longer(cols = c(Oysters, Barnacles, Mussels), 
                     names_to = "Count_Type", values_to = "Count_Value")
      
      # Create scatter plot
      ggplot(df_long, aes(x = mdy(Date), y = Count_Value, color = Count_Type, group = Count_Type)) +
        geom_point(size = 2) +
        geom_line(size = 1) +
        labs(title = "Shellfish Counts Over Time",
             x = "Date",
             y = "Count Value",
             color = "Species") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$areaPlot <- renderPlot({
      df <- area_Filtered()
      
      if (nrow(df) == 0) {
        print("Error: area_Filtered() returned an empty dataset.")
      }
      
      df_long <- df %>%
        pivot_longer(cols = c(Live_Oyster, Shell, Box, Substrate, Other, NoData),
                     names_to = "Cover_Type",
                     values_to = "Percentage") %>%
        mutate(
          Cover_Type = factor(Cover_Type, 
                              levels = rev(c("Live_Oyster", "Box", "Shell", "Substrate", "Other", "NoData")), 
                              labels = rev(c("Live Oyster", "Box", "Shell", "Substrate", "Other", "No Data")))
        )
      
      ggplot(df_long, aes(x = mdy(Date), y = Percentage, fill = Cover_Type)) +
        geom_area(alpha = 0.7) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Cover Composition Over Time",
             x = "Date",
             y = "Percentage of Reef",
             fill = "Ground Cover Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    })
    
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}
