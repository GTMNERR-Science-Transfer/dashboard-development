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
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Create colors for the algae
algae_colors <- c("Diatoms" = "goldenrod2", 
                  "Cyanobacteria" = "cadetblue3", 
                  "Dinoflagellates" = "indianred1", 
                  "Other" = "darkolivegreen4")

HABPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Harmful Algal Bloom Data"),
    fluidRow(
      # First row - explanation
      column(width = 12,
             div(style = "margin-bottom: 20px;",
                 p(htmltools::HTML('This section provides an overview of (harmful) algal bloom data.
                 Currently the dashboard is only showing numerical data (not presence / absence). A
                 value of 0 means that the water sample was tested for this algal type, but it was not
                 detected. If there are no values for a particular day or month, there was no testing
                 performed. <br>
                 <br>
                 Start by selecting the <strong>station</strong> and <strong>date range</strong> that you are interested in. <br>
                 <br>
                 Then select a <strong>type of algae</strong> you are interested in. The 4 algae types 
                 available are the types that could potentially cause blooms or toxins [note to users: there
                 will be an option added in the future to download raw data that shows algae genera and species] <br>
                 <br>
                 High numbers do not necessarily indicate blooms or toxic conditions! You can find more information
                 <a href = "https://shellfish.ifas.ufl.edu/clams_eat/algal-groups.php" target = "new">here</a> about the 4 algae types. <br>
                 <br>
                 The plot below the map will show total daily values in total cells/liter for each type of algae.
                  The tables display monthly average values in cells/liter.'))
             )
      )
    ),
    fluidRow(
      #User inputs in 1st column
      column(width = 6, 
             selectInput(ns("station"), 
                         label = "What station do you want data for?", 
                         choices = c("", unique(HAB$Site)),
                         selected = ""),
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
             ),
             checkboxGroupInput(ns("algae_type"),
                                label = "What type of algae do you want data for?", 
                                choices = c(unique(HAB$type))
             )
             ),
      # Map occupies 2nd column
      column(width = 6, 
             div(style = "margin-bottom: 20px;",
                 leafletOutput(ns("map"), height="500px"))
            )
      ),
    fluidRow(
      # Plot in the next row, below inputs and map
      column(width = 12, 
             div(style = "margin-bottom: 20px;",
                 plotlyOutput(ns("timePlot"))
                 )
      )
    ),
    fluidRow(
      # Plot in the next row, below the plot
      column(width = 12,
             div(style = "margin-bottom: 20px;",
                 conditionalPanel(
                   condition = "input.algae_type.length >= 1",
                   DTOutput(ns("HAB_table"))
                 )),
              # Only show this panel if there are 2 algae types selected
             div(style = "margin-bottom: 20px;",
                 conditionalPanel(
                   condition = "input.algae_type.length >= 2",
                   DTOutput(ns("HAB_table2"))
                 )),
             # Only show this panel if there are 3 algae types selected
             div(style = "margin-bottom: 20px;",
                 conditionalPanel(
                   condition = "input.algae_type.length >= 3",
                   DTOutput(ns("HAB_table3"))
                 )),
             # Only show this panel if there are 4 algae types selected
             div(style = "margin-bottom: 20px;",
                 conditionalPanel(
                   condition = "input.algae_type.length >= 4",
                   DTOutput(ns("HAB_table4"))
                 ))
            )
    # ),
    # fluidRow(
    #   # Plot in the next row, below the plot
    #   column(width = 12, 
    #          DTOutput(ns("HAB_table2"))
    #          #gt::gt_output(ns("HAB_table")), 
    #   )
    # ),
    # fluidRow(
    #   # Plot in the next row, below the plot
    #   column(width = 12, 
    #          DTOutput(ns("HAB_table3"))
    #          #gt::gt_output(ns("HAB_table")), 
    #   )
    # ),
    # fluidRow(
    #   # Plot in the next row, below the plot
    #   column(width = 12, 
    #          DTOutput(ns("HAB_table4"))
    #   )
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
    HAB_data_loc_selected <- reactive({
      req(HAB_df())
      
      HAB_df() %>% 
        left_join(HAB_locs) %>% 
        ungroup() %>% 
        select(Latitude, Longitude, geometry, Site, County) %>%
        distinct() # Some sites have 2 coordinate sets?
    })
    
    ### Not -selected data (for map) --------------------------
    HAB_data_loc_unselected <- reactive({
      req(input$station)
      
      HAB_locs %>% 
        filter(Site != input$station) %>% 
        distinct()
    })
    
    ### Update map based on filtered data --------------------
    # Updates every time HAB_data_locations() is changed
    observe({
      leafletProxy("map") %>%
        # First remove original markers (otherwise they just keep plotting over each other)
        clearMarkers() %>%
        # Make / keep unselected stations blue
        addMarkers(data = HAB_data_loc_unselected(),
                   #layerId = unselected_coords()$geometry,
                   options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
                   popup = ~paste("<strong>Site:</strong> ", Site, "<br>",
                                  "<strong>County:</strong> ", County, "<br>")%>%
                     lapply(htmltools::HTML)
        ) %>% 
        # Make selected stations red
        addMarkers(data = HAB_data_loc_selected(),
                   icon = redIcon, 
                   #layerId = selected_coords()$geometry,
                   options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
                   popup = ~paste("<strong>Site:</strong> ", Site, "<br>",
                                  "<strong>County:</strong> ", County, "<br>"
                                  )%>%
                     lapply(htmltools::HTML),
                   # Had to play around with labelOptions to kind of get it in the correct place
                   popupOptions = popupOptions(direction = "auto",
                                               offset = c(0, -20),
                                               style = list(
                                                 "color" = "gray27",
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )
                   )
        )
    })
    
    # Make the HAB dataframe reactive for plotting
    HAB_df <- reactiveVal()
    selected_type <- reactiveVal()
    
    ### Filter if algae type changes ####
    observeEvent({ # If the selected algae type changes
      input$algae_type
    },{ # Filter dataframe
      req(input$algae_type, input$station != "", input$date_range)
      selected_type(input$algae_type)
      
      print(paste0("You selected algae type(s) ", selected_type()))
      
      HAB_df(HAB %>%
               HAB_filter(algae_type = selected_type(),
                          site = input$station,
                          date_range = input$date_range))
    }) # , ignoreInit = TRUE
    
    ### Filter if station changes ####
    observeEvent({ # If the selected station changes
      input$station
    },{ # Filter dataframe
      req(input$algae_type, input$station, input$date_range)
      selected_type(input$algae_type)
      
      print(paste0("Site has been changed to ", input$station))
      
      HAB_df(HAB %>%
               HAB_filter(algae_type = selected_type(),
                          site = input$station,
                          date_range = input$date_range))
    }, ignoreInit = TRUE)
    
    ### Filter if date range changes ####
    observeEvent({ # If the selected station changes
      input$date_range
    },{ # Filter dataframe
      req(selected_type(), input$station != "", input$date_range)
      
      HAB_df(HAB %>%
               HAB_filter(algae_type = selected_type(),
                          site = input$station,
                          date_range = input$date_range))
    })
    
    ### Create plot ####
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
      
      p <- ggplot(data = HAB_df(), aes(x = date, y = total, color = type)) +
        geom_segment(aes(x = date, xend = date, y = 0, yend = total)) +
        geom_point(size = 2, pch = 1) +
        scale_color_manual(values = algae_colors) +
        labs(x = "", y = "Total cells/liter") +
        theme_bw() +
        facet_wrap(
          ~ Site_type, 
          ncol = 1, 
          scales = "free_y"
        ) +
        theme(
          strip.text = element_text(size = 12), # Adjust strip text size
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
    
    ### Create tables ####
    # Render table with values
    # Create year and month and show monthly average. Color the value
    output$HAB_table <- DT::renderDT(server = FALSE, {
      req(HAB_df(), input$algae_type, length(input$algae_type) >= 1)
      
      caption_color <- get_hex_color(algae_colors[input$algae_type[1]])
      
      formattable(
        HAB_df() %>%
          ungroup() %>%
          mutate(year = year(date), month = month(date)) %>%
          group_by(year, month, Site, type) %>%
          summarize(month_ave = mean(total, na.rm = TRUE)) %>%
          ungroup() %>%
          filter(type == input$algae_type[1]) %>%
          select(Site, type, year, month, month_ave),
        list(
          `month_ave` = color_tile("white", caption_color)
        )
      ) %>%
        as.datatable(colnames = c("Site", "Algae type", "Year", "Month", "Monthly average (cells/liter)"),
                     escape = FALSE,
                     extensions = 'Buttons',
                     options = list(scrollX = TRUE,
                                    dom = 'Blfrtip',
                                    paging = TRUE,
                                    server = FALSE,
                                    buttons = list(
                                      list(extend = "copy", text = "Copy", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = "print", text = "Print", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = 'collection',
                                           text = 'Download',
                                           buttons = list(
                                             list(extend = "csv", text = "CSV", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "excel", text = "Excel", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "pdf", text = "PDF", exportOptions = list(modifier = list(page = "all")))
                                           )
                                      )
                                    )
                     ),
                     rownames = FALSE,
                     caption = htmltools::tags$caption(
                       style = paste0('caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: ',
                                     caption_color, ";"),
                       paste("Station:", input$station, " | Algae Type:", input$algae_type[1])
                     )
        )
    })
    
    output$HAB_table2 <- DT::renderDT(server = FALSE, {
      req(HAB_df(), input$algae_type, length(input$algae_type) >= 2)
      
      caption_color <- get_hex_color(algae_colors[input$algae_type[2]])
      
      formattable(
        HAB_df() %>%
          ungroup() %>%
          mutate(year = year(date), month = month(date)) %>%
          group_by(year, month, Site, type) %>%
          summarize(month_ave = mean(total, na.rm = TRUE)) %>%
          ungroup() %>%
          filter(type == input$algae_type[2]) %>%
          select(Site, type, year, month, month_ave),
        list(
          `month_ave` = color_tile("white", caption_color)
        )
      ) %>%
        as.datatable(colnames = c("Site", "Algae type", "Year", "Month", "Monthly average (cells/liter)"),
                     escape = FALSE,
                     extensions = 'Buttons',
                     options = list(scrollX = TRUE,
                                    dom = 'Blfrtip',
                                    paging = TRUE,
                                    server = FALSE,
                                    buttons = list(
                                      list(extend = "copy", text = "Copy", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = "print", text = "Print", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = 'collection',
                                           text = 'Download',
                                           buttons = list(
                                             list(extend = "csv", text = "CSV", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "excel", text = "Excel", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "pdf", text = "PDF", exportOptions = list(modifier = list(page = "all")))
                                           )
                                      )
                                    )
                     ),
                     rownames = FALSE,
                     caption = htmltools::tags$caption(
                       style = paste0('caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: ',
                                      caption_color, ";"),
                       paste("Station:", input$station, " | Algae Type:", input$algae_type[2])
                     )
        )
    })
    
    output$HAB_table3 <- DT::renderDT(server = FALSE, {
      req(HAB_df(), input$algae_type, length(input$algae_type) >= 3)
      
      caption_color <- get_hex_color(algae_colors[input$algae_type[3]])
      
      formattable(
        HAB_df() %>%
          ungroup() %>%
          mutate(year = year(date), month = month(date)) %>%
          group_by(year, month, Site, type) %>%
          summarize(month_ave = mean(total, na.rm = TRUE)) %>%
          ungroup() %>%
          filter(type == input$algae_type[3]) %>%
          select(Site, type, year, month, month_ave),
        list(
          `month_ave` = color_tile("white", caption_color)
        )
      ) %>%
        as.datatable(colnames = c("Site", "Algae type", "Year", "Month", "Monthly average (cells/liter)"),
                     escape = FALSE,
                     extensions = 'Buttons',
                     options = list(scrollX = TRUE,
                                    dom = 'Blfrtip',
                                    paging = TRUE,
                                    server = FALSE,
                                    buttons = list(
                                      list(extend = "copy", text = "Copy", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = "print", text = "Print", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = 'collection',
                                           text = 'Download',
                                           buttons = list(
                                             list(extend = "csv", text = "CSV", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "excel", text = "Excel", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "pdf", text = "PDF", exportOptions = list(modifier = list(page = "all")))
                                           )
                                      )
                                    )
                     ),
                     rownames = FALSE,
                     caption = htmltools::tags$caption(
                       style = paste0('caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: ',
                                      caption_color, ";"),
                       paste("Station:", input$station, " | Algae Type:", input$algae_type[3])
                     )
        )
    })
    
    output$HAB_table4 <- DT::renderDT(server = FALSE, {
      req(HAB_df(), input$algae_type, length(input$algae_type) >= 4)
      
      caption_color <- get_hex_color(algae_colors[input$algae_type[4]])
      
      formattable(
        HAB_df() %>%
          ungroup() %>%
          mutate(year = year(date), month = month(date)) %>%
          group_by(year, month, Site, type) %>%
          summarize(month_ave = mean(total, na.rm = TRUE)) %>%
          ungroup() %>%
          filter(type == input$algae_type[4]) %>%
          select(Site, type, year, month, month_ave),
        list(
          `month_ave` = color_tile("white", caption_color)
        )
      ) %>%
        as.datatable(colnames = c("Site", "Algae type", "Year", "Month", "Monthly average (cells/liter)"),
                     escape = FALSE,
                     extensions = 'Buttons',
                     options = list(scrollX = TRUE,
                                    dom = 'Blfrtip',
                                    paging = TRUE,
                                    server = FALSE,
                                    buttons = list(
                                      list(extend = "copy", text = "Copy", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = "print", text = "Print", exportOptions = list(modifier = list(page = "all"))),
                                      list(extend = 'collection',
                                           text = 'Download',
                                           buttons = list(
                                             list(extend = "csv", text = "CSV", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "excel", text = "Excel", exportOptions = list(modifier = list(page = "all"))),
                                             list(extend = "pdf", text = "PDF", exportOptions = list(modifier = list(page = "all")))
                                           )
                                      )
                                    )
                     ),
                     rownames = FALSE,
                     caption = htmltools::tags$caption(
                       style = paste0('caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: ',
                                      caption_color, ";"),
                       paste("Station:", input$station, " | Algae Type:", input$algae_type[4])
                     )
        )
    })
    
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
      })
  })
}
