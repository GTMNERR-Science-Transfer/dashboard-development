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

# create lineplot of recordings
create_plot <- function(df, units_df, loc_name = "GTMNERR", selected_column = NULL) {
  # Initialize the plot with the x-axis
  fig <- plot_ly(df, x = ~Activity.Start.Date.Time, source = "A")
  
  # Get the column names except the one for the x-axis
  column_names <- sort(colnames(df)[colnames(df) != "Activity.Start.Date.Time"])
  
  # Print statements for debugging
  print(paste("Selected column: ", selected_column))
  print("Available column names: ")
  print(column_names)
  
  # Create a named vector for Y-axis titles
  y_axis_titles <- setNames(units_df$DEP.Result.Unit, units_df$DEP.Analyte.Name)
  
  # Default to the first column if none selected or if the selected column doesn't exist
  if (is.null(selected_column) || !selected_column %in% column_names) {
    selected_column <- column_names[1]
  }
  
  # Loop through each column and add a trace
  for (i in seq_along(column_names)) {
    fig <- fig %>%
      add_trace(y = df[[column_names[i]]], name = column_names[i], type = 'scatter', mode = 'lines',
                visible = if (column_names[i] == selected_column) TRUE else FALSE)
  }
  
  # Create the dropdown buttons
  buttons <- list()
  for (i in seq_along(column_names)) {
    buttons[[i]] <- list(
      method = "update",
      args = list(
        list(visible = rep(FALSE, length(column_names))),
        list(
          title = paste("Mean ", column_names[i], " for ", loc_name),
          yaxis = list(title = y_axis_titles[column_names[i]])
        )
      ),
      label = column_names[i]
    )
    buttons[[i]]$args[[1]]$visible[i] <- TRUE
  }
  
  # Customize the layout
  fig <- fig %>%
    layout(xaxis = list(title = 'Date'),
           yaxis = list(title = y_axis_titles[selected_column]),
           title = paste("Mean ", selected_column, " for ", loc_name),
           updatemenus = list(list(
             y = 0.8,
             buttons = buttons,
             showactive = TRUE
           ))) %>%
    event_register("plotly_relayout") %>%
    event_register("plotly_click")
  
  return(fig)
}






#### Run the app #### 

find_directory_of_file("app.R")

WINPageUI <- function(id) {
  ns <- NS(id) # This is an important part to add to all sub pages so they use the
  # correct sessions / ID's that connect the ui and server here
  tagList(
    h2("Water Infrastructure Network"),
    fluidRow(
      # Map occupies 1st column
      column(width = 7, leafletOutput(ns("map"), height="500px")), # make sure to put the input inside ns()
      # histogram occupies rows in the 2nd column
      column(width = 5, plotlyOutput(ns("plot"), height="500px")),
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page")
  )
}

WINPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is 
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns
    
    # Reactive value to track the selected dropdown item
    selected_column <- reactiveVal(NULL)
    
    # Reactive value to track popup bubble state
    popup_visible <- reactiveVal(FALSE)
    
    # Default plot
    output$plot <- renderPlotly({
      df <- filter_dataframe(WIN_df)
      create_plot(df, 
                  units_df = WIN_data_units, 
                  selected_column = selected_column())
    })
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
        clearBounds() %>% # centers map on all min/max coords
        # Base map
        addTiles() %>% # Add default OpenStreetMap map tiles
        # Polygons, add groups
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
        )  %>%
        # Layers control (turning layers on and off)
        addLayersControl(
          overlayGroups = c("Counties", "GTMNERR boundaries", "WIN"), # 
          options = layersControlOptions(collapsed = FALSE)
        )  %>%
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
          df <- filter_dataframe(WIN_df, 
                                 filter_value = clicked_data$HUC12.Name)
          create_plot(df, 
                      units_df = WIN_data_units, 
                      loc_name = clicked_data$HUC12.Name, 
                      selected_column = selected_column())
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
          df <- filter_dataframe(WIN_df, 
                                 filter_value = clicked_data$NAME)
          create_plot(df, 
                      units_df = WIN_data_units, 
                      loc_name = clicked_data$NAME, 
                      selected_column = selected_column())
        })
      }
    })
    
    # Observe map clicks to hide the popup
    observeEvent(input$map_click, {
      popup_visible(FALSE)
      
      # Default plot
      output$plot <- renderPlotly({
        df <- filter_dataframe(WIN_df)
        create_plot(df, units_df = WIN_data_units, 
                    selected_column = selected_column())
      })
    })
    
  observeEvent(event_data("plotly_relayout", source = "A"), {
    layout <- event_data("plotly_relayout", source = "A")
    print("plotly_relayout event triggered")
    print(layout)
    if (!is.null(layout$'yaxis.title.text')) {
      print(paste("Updating selected_column to:", layout$'yaxis.title.text'))
      selected_column(layout$'yaxis.title.text')
      
      # Re-render the plot with the updated selected column
      df <- filter_dataframe(WIN_df)
      output$plot <- renderPlotly({
        create_plot(df, units_df = WIN_data_units, selected_column = selected_column())
      })
    }
  })

  observeEvent(event_data("plotly_click", source = "A"), {
    selected_trace <- event_data("plotly_click", source = "A")$curveNumber
    print("plotly_click event triggered")
    print(selected_trace)
    if (!is.null(selected_trace)) {
      print(paste("Updating selected_column to:", names(df)[selected_trace + 1]))
      selected_column(names(df)[selected_trace + 1])
      
      # Re-render the plot with the updated selected column
      df <- filter_dataframe(WIN_df)
      output$plot <- renderPlotly({
        create_plot(df, units_df = WIN_data_units, selected_column = selected_column())
      })
    }
  })


    
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}