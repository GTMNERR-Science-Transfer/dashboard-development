# import data for this page
load("./03_Data_for_app/WIN.RData")

#### Process data further ####
# make datframe for map display and hover data
WIN_data_locations = WIN_df %>%
  filter(variable %in% c("geometry", 
                       "HUC12.Name", 
                       "Start.Date",
                       "latitude",
                       "longitude")
         ) %>%
  select(c(RowID, variable, value)) %>%
  distinct(RowID, variable, value) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA)
  ) %>%
  distinct(geometry, HUC12.Name, Start.Date, latitude, longitude) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

WIN_data_units = WIN_df %>%
  filter(variable %in% c("DEP.Analyte.Name", 
                         "DEP.Result.Unit")
  ) %>%
  select(c(RowID, variable, value)) %>%
  distinct(RowID, variable, value) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA),
    values_fn = list(value = ~ first(.))
  ) %>%
  distinct(DEP.Analyte.Name, DEP.Result.Unit)

#### Functions ####
# make dataframe for click plots
filter_dataframe <- function(df, filter_value = NULL) {
  if (!is.null(filter_value)) {
    # Step 1: Identify the relevant RowIDs
    relevant_row_ids <- df %>%
      filter(value == filter_value) %>%
      pull(RowID)
    
    # Step 2: Filter the entire dataframe to keep only rows with the relevant RowIDs
    filtered_df <- df %>%
      filter(RowID %in% relevant_row_ids)
  } else {
    # If no filter_value is provided, skip the filtering step
    filtered_df <- df
  }
  
  # Step 3: Create wide dataframe
  wide_df <- filtered_df %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    select(Activity.Start.Date.Time, 
           DEP.Analyte.Name, 
           Org.Result.Value) %>%
    pivot_wider(names_from = DEP.Analyte.Name, 
                values_from = Org.Result.Value,
                values_fn = list(Org.Result.Value = ~ mean(as.numeric(.), na.rm = TRUE))) %>%
    mutate(Activity.Start.Date.Time = ymd_hms(Activity.Start.Date.Time)) %>%
    mutate(across(-Activity.Start.Date.Time, ~ as.numeric(.))) %>%
    mutate(Activity.Start.Date.Time = as.Date(Activity.Start.Date.Time)) %>%
    group_by(Activity.Start.Date.Time) %>%
    summarize(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
    select(where(~ n_distinct(.) > 2))
  
  return(wide_df)
}

# New function to create the dropdown
create_dropdown <- function(df, ns) {
  column_names <- sort(colnames(df)[colnames(df) != "Activity.Start.Date.Time"])
  print(paste("Creating dropdown with choices:", paste(column_names, collapse=", ")))
  
  selectInput(
    inputId = ns("column_selector"),
    label = "Select a Column",
    choices = column_names,
    selected = column_names[1]
  )
}

# Modified create_plot function
create_plot <- function(df, units_df, loc_name = "GTMNERR", selected_column) {
  print(paste("Creating plot for", selected_column))
  # Initialize the plot with the x-axis
  fig <- plot_ly(df, x = ~Activity.Start.Date.Time)
  
  # Get the column names except the one for the x-axis
  column_names <- sort(colnames(df)[colnames(df) != "Activity.Start.Date.Time"])
  
  # Create a named vector for Y-axis titles
  y_axis_titles <- setNames(units_df$DEP.Result.Unit, units_df$DEP.Analyte.Name)
  
  # Ensure selected_column is not NULL or empty
  if (is.null(selected_column) || selected_column == "") {
    selected_column <- column_names[1]
  }
  
  # Loop through each column and add a trace
  for (i in seq_along(column_names)) {
    fig <- fig %>%
      add_trace(y = df[[column_names[i]]], name = column_names[i], type = 'scatter', mode = 'lines',
                visible = if (column_names[i] == selected_column) TRUE else FALSE)
  }
  
  # Customize the layout
  fig <- fig %>%
    layout(xaxis = list(title = 'Date'),
           yaxis = list(title = y_axis_titles[selected_column]),
           title = paste("Mean ", selected_column, " for ", loc_name))
  
  return(fig)
}


#### Run the app #### 
find_directory_of_file("app.R")

# Updated UI in WINPageUI
WINPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Water Infrastructure Network"),
    fluidRow(
      column(width = 7, leafletOutput(ns("map"), height="500px")),
      column(width = 5, plotlyOutput(ns("plot"), height="500px"))
    ),
    fluidRow(
      column(width = 12, uiOutput(ns("dropdown_ui")))
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page")
  )
}

# Updated Server logic in WINPageServer
WINPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value for the selected column
    selected_column <- reactive({
      print(paste("Selected column changed to:", input$column_selector))
      input$column_selector
    })
    
    # Reactive value to track popup bubble state
    popup_visible <- reactiveVal(FALSE)
    
    # Create the dropdown UI
    output$dropdown_ui <- renderUI({
      print("Rendering dropdown UI")
      df <- filter_dataframe(WIN_df)
      create_dropdown(df, ns)
    })
    
    # Update plot when dropdown selection changes
    observeEvent(selected_column(), {
      print(paste("Updating plot for", selected_column()))
      output$plot <- renderPlotly({
        df <- filter_dataframe(WIN_df)
        create_plot(df, units_df = WIN_data_units, selected_column = selected_column())
      })
    }, ignoreInit = FALSE)
    
    # Default plot
    output$plot <- renderPlotly({
      df <- filter_dataframe(WIN_df)
      create_plot(df, units_df = WIN_data_units, selected_column = selected_column())
    })
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
        clearBounds() %>%
        addTiles() %>%
        addPolygons(
          data = GTMNERR, color = "purple", fill = NA,
          weight = 2, opacity = 1, group = "GTMNERR boundaries"
        ) %>%
        addPolygons(
          data = counties_select,
          color = "black", weight = 2, opacity = 1,
          fill = TRUE, fillColor = "white", fillOpacity = 0.01,
          highlightOptions = highlightOptions(
            color = "white", weight = 2,
            bringToFront = TRUE
          ),
          group = "Counties", popup = ~NAME, layerId = ~NAME
        ) %>%
        addMarkers(
          data = WIN_data_locations,
          popup = ~ paste("Station Name: ", HUC12.Name, "<br>",
                          "Start Date: ", Start.Date, "<br>"
          ),
          group = "WIN",
          layerId = ~geometry
        ) %>%
        addLayersControl(
          overlayGroups = c("Counties", "GTMNERR boundaries", "WIN"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })
    
    # Observe marker clicks
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click)) {
        clicked_id <- click$id
        clicked_data <- WIN_data_locations %>%
          filter(geometry == clicked_id)
        
        popup_visible(TRUE)
        
        output$plot <- renderPlotly({
          df <- filter_dataframe(WIN_df, filter_value = clicked_data$HUC12.Name)
          create_plot(df, units_df = WIN_data_units, loc_name = clicked_data$HUC12.Name, selected_column = selected_column())
        })
      }
    })
    
    # Add observeEvent function for input$map_shape_click
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (!is.null(click)) {
        clicked_id <- click$id
        clicked_data <- counties_select %>%
          filter(NAME == clicked_id)
        
        popup_visible(TRUE)
        
        output$plot <- renderPlotly({
          df <- filter_dataframe(WIN_df, filter_value = clicked_data$NAME)
          create_plot(df, units_df = WIN_data_units, loc_name = clicked_data$NAME, selected_column = selected_column())
        })
      }
    })
    
    # Observe map clicks to hide the popup
    observeEvent(input$map_click, {
      popup_visible(FALSE)
      
      # Default plot
      output$plot <- renderPlotly({
        df <- filter_dataframe(WIN_df)
        create_plot(df, units_df = WIN_data_units, selected_column = selected_column())
      })
    })
    
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}
