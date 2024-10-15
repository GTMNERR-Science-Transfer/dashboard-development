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
  
  dateRangeInput( # This creates a date vector of length 2
    inputId = ns("date_range"),
    label = "Select a Date Range",
    start = NULL, 
    end = NULL, # will use current date
    min = min(date_column),
    max = max(date_column),
    format = "mm/dd/yyyy",
    separator = " - "
  )
}

##### Create plot #####
# Modified create_plot function: takes 1 reactive value: the variable (selected
# column) 
create_plot <- function(df, units_df, loc_name = NULL, selected_column, date_range) {
  print(paste("Creating plot for", selected_column, "at", loc_name))
  # At this point, the datafrme has already been filtered for the correct station
  # Initialize the plot with the x-axis
  fig <- plot_ly(df, x = ~SampleDate)
  
  # Get the column names except the dates and column names and geometry
  column_names <- sort(colnames(df)[!colnames(df) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])
  
  # Create a named vector for Y-axis titles
  y_axis_titles <- setNames(units_df$Unit, units_df$ComponentLong)
  
  # Ensure selected_column is not NULL or empty
  # if (is.null(selected_column) || selected_column == "") {
  #   selected_column <- column_names[1]
  # }
  
  # Loop through each column and add a trace -> Check if this is the best way (maybe rather filter df?)
  for (i in seq_along(column_names)) {
    fig <- fig %>%
      add_trace(y = df[[column_names[i]]], name = column_names[i], type = 'scatter', mode = 'lines+markers',
                showlegend = FALSE,
                visible = if (column_names[i] == selected_column) TRUE else FALSE)
  }
  
  station_name <- unique(df$StationCode)
  
  # Customize the layout
  fig <- fig %>%
    layout(xaxis = list(title = 'Date'),
           yaxis = list(title = y_axis_titles[selected_column]),
           title = list(text = paste0("Mean daily ", selected_column, "<br>for ", loc_name, " (station code: ", station_name, ")"), 
                        y = 0.90), 
           margin = list(t = 60),
           plot_bgcolor = '#e5ecf6',
           xaxis = list(zerolinecolor = 'black',
                        zerolinewidth = 2,
                        gridcolor = 'azure1'),
           yaxis = list(zerolinecolor = 'black',
                        zerolinewidth = 2,
                        gridcolor = 'azure1'))
  
  return(fig)
}
