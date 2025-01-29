########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Chad Palmer
# Geraldine Klarenberg, PhD
# Email: gklarenberg@ufl.edu
# University of Florida
# Last updated: see commit history

### HAB Data------------------------------
HAB <- readRDS("./03_Data_for_app/HAB.Rds")

# Create long format so it can be used in the Shiny app
# Only doing this with numeric variables for now

# GeneraData <- separate_wider_delim(data = HAB, cols = Species, delim = " ",
#                                    names = c("genus", "species"), too_few = "align_start", too_many = "merge")
# 
# 
# GeneraData$userMessage <- vector(length = length(GeneraData$genus))
# GeneraData$userMessage[] <- "System Error; please report"
# 
# # Move this to the cleaning script. Also rewrite as a vectorized operation (is faster)
# for(i in 1:length(GeneraData$userMessage)){
#   if(!is.na(GeneraData$Description[i])){
#     GeneraData$userMessage[i] = "Algae is Present"  
#   } else{
#     GeneraData$userMessage[i] = paste("Algae is present at ", toString(GeneraData$'cells/L*'[i]), " cells/L")
#   }
# }

HAB_locs <- HAB %>% 
  select(Latitude, Longitude, Site, County) %>% 
  distinct() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  

HABPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Harmful Algal Bloom Data"),
    fluidRow(
      # First row - explanation
      column(width = 12,
             div(style = "margin-bottom: 20px;",
                 p(htmltools::HTML('This section provides an overview of harmful algal bloom data.
                 Currently the dashboard is only showing numerical data (not presence / absence). A
                 value of 0 means that the water sample was tested for this algal type, but it was not
                 detected. <br>
                 Start by selecting a type of algae you are interested in. <br>
                 Next, select the stations and date range that you are interested in. <br>
                 The plot below the map will show total daily values in total cells/liter for each type of algae. <br>
                 <br>
                 Note the different y-axis scales, and also note that high numbers do not necessarily indicate
                 blooms or toxic conditions [add a link to info here].'))
             )
      )
    ),
    fluidRow(
      #User inputs in 1st column
      column(width = 3, 
             selectInput(ns("algae_type"), 
                         label = "What type of algae do you want data for?", 
                         choices = c(unique(HAB$type)), 
                         multiple = TRUE),
             selectInput(ns("station"), 
                         label = "What station do you want data for?", 
                         choices = c(unique(HAB$Site))),
             #uiOutput(ns("selectStation")),
             sliderInput(
               inputId = ns("date_range"),
               label = "Select a Date Range",
               min = min(dmy(HAB$'Sample Date')), #NULL
               max = max(dmy(HAB$'Sample Date')), #NULL
               value = c(min(dmy(HAB$'Sample Date')), 
                         max(dmy(HAB$'Sample Date'))),
               timeFormat = "%m/%d/%Y",
               width = "100%"
             )
             #uiOutput(ns("selectDate")) #Keeping as old code in case I need to go back to a single date option
             ),
      # Map occupies 2nd column
      column(width = 8, 
             div(style = "margin-bottom: 20px;",
                 leafletOutput(ns("map"), height="500px"))
            )
      ),
    fluidRow(
      #User inputs in 1st column
      column(width = 12, 
      plotlyOutput(ns("timePlot")), 
      )
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page") #All input IDs need to be inside ns()
  )
}

HABPageServer <- function(id, parentSession) {
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
        #setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
        clearBounds() %>% # centers map on all min/max coords
        # Base map
        addTiles() %>%  # Add default OpenStreetMap map tiles
        # Polygons, add groups
        addPolygons(data = GTMNERR, color = "purple", fill = NA, 
                    weight = 2, opacity = 1, group = "GTMNERR boundaries") %>% 
        addPolygons(data = counties_select, 
                    color = "black", weight = 2, opacity = 1,
                    fill = TRUE, fillColor = "white", fillOpacity = 0.01,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    group = "Counties", popup = ~NAME) %>% 
        addMarkers(data = HAB_locs, # Initialize without reactive dataframe
                   #color = ~colorQuantile("YlOrRd",`cells/L*`)(`cells/L*`), #This is currently not working because data is location only
                   popup = ~paste("Site: ", Site, "<br>",
                                  "County: ", County, "<br>"),
                   group = "HAB") %>% 
        # Layers control (turning layers on and off)
        addLayersControl(overlayGroups = c("Counties", "GTMNERR boundaries"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })
    
    ### Selected Data (for map) -----------------------
    select_HAB_data <- reactive({
      req(input$algae_type, input$date_range)
      
      HAB %>%
        filter(type %in% input$algae_type) %>% 
        select(Latitude, Longitude, Date, Site, County)
    })
    
    ### Selected Data (for map) -----------------------
    HAB_data_locations <- reactive({
      req(input$algae_type, input$date_range)
      
      HAB %>%
        filter(type %in% input$algae_type,
               dmy(`Sample Date`) >= input$date_range[1] & dmy(`Sample Date`) <= input$date_range[2]) %>%
        select(Latitude, Longitude, Site, County, 'Sample Date') %>%
        distinct() %>% 
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    })
    
    # Make the HAB dataframe reactive for plotting
    HAB_df <- reactiveVal()
    selected_type <- reactiveVal()
    
    #### Filter if algae type changes ####
    observeEvent({ # If the selected algae type changes
      input$algae_type
    },{ # Filter dataframe
      req(input$algae_type, input$station, input$date_range)
      selected_type(input$algae_type)
      
      print(paste0("You selected algae type(s) ", selected_type()))
      
      HAB_df(HAB %>%
               HAB_filter(algae_type = selected_type(),
                          site = input$station,
                          date_range = input$date_range))
    })
    
    #### Filter if station changes ####
    observeEvent({ # If the selected station changes
      input$station
    },{ # Filter dataframe
      req(selected_type(), input$station, input$date_range)
      
      HAB_df(HAB %>%
               HAB_filter(algae_type = selected_type(),
                          site = input$station,
                          date_range = input$date_range))
    }, ignoreInit = TRUE)
    
    #### Filter if date range changes ####
    observeEvent({ # If the selected station changes
      input$date_range
    },{ # Filter dataframe
      req(selected_type(), input$station, input$date_range)
      
      HAB_df(HAB %>%
               HAB_filter(algae_type = selected_type(),
                          site = input$station,
                          date_range = input$date_range))
    }, ignoreInit = TRUE)
    
    #### Create plot ####
    output$timePlot <- renderPlotly({
      req(HAB_df())
      #### Have to move this to functions and add an if-else set up so it does not
      # use facet_grid if only one algae type is selected.
      # For numeric data only
      
      # Render an empty plot if no data is selected or no data available
      if (is.null(HAB_df()) || nrow(HAB_df()) == 0) {
        # Render empty plot
        return(plot_ly(type = 'scatter', mode = 'markers') %>%
                 layout(title = "No data selected", 
                        xaxis = list(visible = FALSE), 
                        yaxis = list(visible = FALSE)))
      }
      
      print(paste0("You are plotting ", unique(HAB_df()$type)))
      print(paste0("data length is ", length(unique(HAB_df()$type))))
      
      p <- ggplot(data = HAB_df(), aes(x = date, y = total, color = type)) +
        geom_segment(aes(x = date, xend = date, y = 0, yend = total)) +
        geom_point(size = 2, pch = 1) +
        labs(x = "", y = "Total cells/liter") +
        theme_bw() +
        facet_wrap(
          ~ Site_type, 
          ncol = 1, 
          scales = "free_y"
        ) +
        theme(
          strip.text = element_text(size = 12), # Adjust strip text size
          #strip.placement = "outside",         # Place strips outside plot area
          strip.background = element_rect(fill = NA),
          legend.position = "none"
        )

      gp <- ggplotly(p,
               dynamicTicks = TRUE) %>%
        layout(margin = list(r = 50, l=70)) # Add more margin space to the left and the right
      # Set the y-axis label position (more to the left)
      gp[["x"]][["layout"]][["annotations"]][[1]][["x"]] <- -0.02

      gp
    })
    
    
    ### Update map based on filtered data --------------------
    # Updates every time HAB_data_locations() is changed
    observe({
      leafletProxy("map") %>%
        # First remove original markers (otherwise they just keep plotting over each other)
        clearMarkers() %>%
        # Make / keep unselected stations blue
        addMarkers(data = HAB_data_locations(), # Use reactive (filtered) dataframe
                   #color = ~colorQuantile("YlOrRd",`cells/L*`)(`cells/L*`), #This is currently not working because data is location only
                   popup = ~paste("Site: ", Site, "<br>",
                                  "County: ", County, "<br>"#,
                                  #"Sample Date: ", `Sample Date`, "<br>",
                                  #userMessage
                                  ),
                   group = "HAB")
    })
    
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
      })
  })
}
