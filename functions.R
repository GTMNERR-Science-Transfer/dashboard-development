#### FUNCTIONS ####

##### Find the directory of a file named app.R #####
find_directory_of_file <- function(file_name, start_dir=getwd()) {
  # Recursively list all files starting from the start_dir
  app_dir <- fs::dir_ls(start_dir, recurse = TRUE, glob=file_name)
  
  # Check if any file named app.R is found
  if (length(app_dir) > 0) {
    # Assuming you want the directory of the first matching file
    file_dir <- fs::path_dir(app_dir[1])
    return(file_dir)
  } else {
    return(NULL) # Return NULL if the file is not found
  }
}

##### Filter df for station (click on plot) and date range, and make wide #####
# make dataframe for click plots - filter for correct stations
# and the date picker: so this takes 2 reactive values, stationCode and
# selected_date_range
filter_dataframe <- function(df, filter_value = NULL, date_range = NULL) {
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
    pivot_wider(names_from = variable, values_from = value, values_fn = first) %>%
    filter(!is.na(ComponentLong)) %>% # stop gap measure because there are NAs from
    # replacing ComponentLong names in the cleaning script
    select(SampleDate, # we could also make these arguments for the function?
           ComponentLong, 
           Result,
           geometry, StationCode, site_friendly) %>%
    pivot_wider(names_from = ComponentLong, 
                values_from = Result,
                values_fn = list(Result = ~ mean(as.numeric(.), na.rm = TRUE))) %>%
    #mutate(SampleDate = ymd_hms(SampleDate)) %>% # 
    mutate(SampleDate = str_extract(SampleDate, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% 
    mutate(SampleDate = ymd(SampleDate)) %>% # these two lines are another option to only get ymd,
    # needed to use this since some datasets only have ymd (no hms) so that makes ymd_hms fail
    #mutate(across(-c(SampleDate, geometry, StationCode, site_friendly), ~ as.numeric(.))) %>%
    #mutate(SampleDate = as.Date(SampleDate)) %>%
    group_by(SampleDate, geometry, StationCode, site_friendly) %>%
    summarize(across(everything(), ~mean(.x, na.rm = TRUE))) #%>% # across(everything()) is not necessary,
  # strictly speaking, but it's nice to keep for if we ever want to adjust this function
  # to work for more than 1 variable
  #select(where(~ n_distinct(.) > 2))
  
  if(!is.null(date_range)){
    wide_df <- filter(wide_df, between(SampleDate, date_range[1], date_range[2]))
  }
  
  return(wide_df)
}

##### Filter df for station (click on plot) and date range, and variable, and make wide #####
# Adjusted to filter for more than 1 station
filter_dataframe2 <- function(df, filter_station = NULL, date_range = NULL, filter_value = NULL) {
  if (!is.null(filter_station)) {
    # Step 1: Identify the relevant RowIDs
    relevant_row_ids <- df %>%
      filter(value %in% filter_station) %>%
      pull(RowID)
    
    # Step 2: Filter the entire dataframe to keep only rows with the relevant RowIDs
    filtered_df <- df %>%
      filter(RowID %in% relevant_row_ids)
  } else {
    # If no filter_station is provided, skip the filtering step
    filtered_df <- df
  }
  
  # Step 3: Create wide dataframe
  wide_df <- filtered_df %>%
    pivot_wider(names_from = variable, values_from = value, values_fn = first) %>%
    filter(!is.na(ComponentLong)) %>% # stop gap measure because there are NAs from
    # replacing ComponentLong names in the cleaning script
    select(SampleDate, # we could also make these arguments for the function?
           ComponentLong, 
           Result,
           geometry, StationCode, site_friendly) %>%
    pivot_wider(names_from = ComponentLong, 
                values_from = Result,
                values_fn = list(Result = ~ mean(as.numeric(.), na.rm = TRUE))) %>%
    #mutate(SampleDate = ymd_hms(SampleDate)) %>% # 
    mutate(SampleDate = str_extract(SampleDate, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% 
    mutate(SampleDate = ymd(SampleDate)) %>% # these two lines are another option to only get ymd,
    # needed to use this since some datasets only have ymd (no hms) so that makes ymd_hms fail
    #mutate(across(-c(SampleDate, geometry, StationCode, site_friendly), ~ as.numeric(.))) %>%
    #mutate(SampleDate = as.Date(SampleDate)) %>%
    group_by(SampleDate, geometry, StationCode, site_friendly) %>%
    summarize(across(everything(), ~mean(.x, na.rm = TRUE))) #%>% # across(everything()) is not necessary,
  # strictly speaking, but it's nice to keep for if we ever want to adjust this function
  # to work for more than 1 variable
  #select(where(~ n_distinct(.) > 2))
  
  if(!is.null(date_range)){
    wide_df <- filter(wide_df, between(SampleDate, ymd(date_range[1]), ymd(date_range[2])))
  }
  
  if(!is.null(filter_value)){
    if(filter_value %in% names(wide_df)){
      wide_df <- wide_df %>% 
        select(SampleDate, geometry, StationCode, site_friendly, all_of(filter_value))
    }
  }
  
  return(wide_df)
}
##### Create dropdown with variables to plot #####
create_dropdown <- function(df, ns) {
  # Get the column names except the dates and column names and geometry
  column_names <- sort(colnames(df)[!colnames(df) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])
  print(paste("Creating dropdown with choices:", paste(column_names, collapse=", ")))
  
  selectInput(
    inputId = ns("column_selector"),
    label = "Select a Variable of Interest",
    choices = column_names,
    selected = column_names[1]
  )
}

##### Create the date picker #####
create_date <- function(df, ns) {
  # Get the column names except the dates and column names and geometry
  date_column <- df %>% 
    pull(SampleDate)
  print(paste("Creating date range"))
  
  airDatepickerInput(
    inputId = ns("date_range"),
    label = "Select a Date Range",
    range = TRUE,
    minDate = min(date_column),
    maxDate = max(date_column),
    dateFormat = "MM/dd/yyyy",
    separator = " - "
  )
  
  # dateRangeInput( # This creates a date vector of length 2
  #   inputId = ns("date_range"),
  #   label = "Select a Date Range",
  #   start = NULL, 
  #   end = NULL, # will use current date
  #   min = min(date_column),
  #   max = max(date_column),
  #   format = "mm/dd/yyyy",
  #   separator = " - "
  # )
}

##### Create plot #####
# Modified create_plot function: takes 1 reactive value: the variable (selected
# column) 
create_plot <- function(df, units_df, selected_column) { # The input here 
  # is an already filtered df, so there is a lot we do not have to supply (e.g. date range and stations)
  
  # Check if the variable exists in the dataframe columns - if not, give error 
  # (and this will stop further execution)
  # validate(
  #   need(selected_column %in% names(df), paste("Sorry! Variable", selected_column,
  #                                              "does not exist for the selected station(s) and time frame"))
  # )
  if (!(selected_column %in% names(df))) {
    showNotification(paste("Sorry! Variable", selected_column,
                           "does not exist for the selected station(s) and time frame"), 
                     type = "warning",
                     duration = NULL)
    return(NULL)  # Prevents further code from running if variable doesn't exist
  }
  
  print(paste("Creating plot for", selected_column, "for", length(unique(df$StationCode)), "stations"))
  print(paste("Creating plot for", selected_column, "at", paste(unique(df$StationCode), collapse = ", ")))
  # At this point, the dataframe has already been filtered for the correct station
  # Initialize the plot
  
  # # Count unique StationCode values because plot_ly wants at least 3
  # num_stations <- n_distinct(df$StationCode)
  # # Determine colors without named vectors
  # color_mapping <- if (num_stations > 2) {
  #   unname(as.character(df$StationCode))  # Ensure it's a regular vector
  # } else if (num_stations == 2) {
  #   c("royalblue", "darkorange")
  # } else {
  #   "royalblue"
  # }
  # 
  # fig <- plot_ly(data = df, 
  #                x = ~ SampleDate,
  #                y = ~ .data[[selected_column]], # changed this because we're only doing one variable at a time
  #                type = 'scatter',
  #                mode = 'lines+markers',
  #               # The following code is there because plot_ly does not like colors
  #               # for less than 3 categories. When you plot it on its own, you get a 
  #               # warning, but in Shiny (for some reason), it's an error. So if there
  #               # are less than 3 categories (stations), you should use split. Who knew.
  #                color = if (num_stations > 2){
  #                  ~factor(StationCode, labels = df$site_friendly[unique(df$StationCode)])
  #                } else {
  #                  NULL
  #                }, 
  #                split = if (num_stations <= 2) {
  #                  ~factor(StationCode, labels = df$site_friendly[unique(df$StationCode)])
  #                } else {
  #                  NULL
  #                }
  #                )
  
  # Get the column names except the dates and column names and geometry
  #column_names <- sort(colnames(df)[!colnames(df) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])
  
  # Create a named vector for Y-axis titles
  y_axis_titles <- setNames(paste0(units_df$ComponentLong, " (", units_df$Unit, ")"), units_df$ComponentLong)
  
  # Ensure selected_column is not NULL or empty
  # if (is.null(selected_column) || selected_column == "") {
  #   selected_column <- column_names[1]
  # }
  
  # Initialize the plot with the x-axis
  fig <- plot_ly()
  
  # Loop through each station name and add a trace
  unique_stations <- unique(df$StationCode)
  unique_friendly <- unique(df$site_friendly)
  for (i in seq_along(unique_stations)) {
    station_data <- df %>% filter(StationCode == unique_stations[i])
    
    fig <- fig %>%
      add_trace(x = station_data$SampleDate,          # Define x explicitly for each trace
                y = station_data[[selected_column]],  # Define y explicitly for each trace
                name = paste0(unique_friendly[i], " (", unique_stations[i], ")"),
                type = 'scatter', 
                mode = 'lines+markers',
                showlegend = TRUE)
  }
  
  station_name <- unique(df$StationCode)
  
  # Customize the layout
  fig <- fig %>%
    layout(showlegend = TRUE, # by also adding this here, you will also get a legend when plotting one variable
           xaxis = list(title = ""),
           yaxis = list(title = y_axis_titles[selected_column]), 
           title = list(text = paste0("Daily ", selected_column), 
                        y = 0.90),
           legend = list(orientation = 'h'),
           margin = list(t = 60),
           plot_bgcolor = '#e5ecf6',
           xaxis = list(zerolinecolor = 'darkgrey',
                        zerolinewidth = 2,
                        gridcolor = 'azure1'),
           yaxis = list(zerolinecolor = 'darkgrey',
                        zerolinewidth = 2,
                        gridcolor = 'azure1')) %>% 
    config(displayModeBar = TRUE) # Make sure the top-right tool bar always shows (not just on hover)
  
  return(fig)
}
