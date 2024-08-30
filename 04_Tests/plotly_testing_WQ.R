# Plotly practice / test

# filter first based on filter_value, which is the StationCode from the clicked station
# then create dropdown with available data -> create reactive selected_column()
# create plot using selected_column()

# inputs: df, units_df, loc_name = "GTMNERR", selected_column()

WQ_df <- readRDS("./03_Data_for_app/WQ_all.Rds")

filter_value = "GTMLSNUT"
df = WQ_df

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

wide_df <- filtered_df %>%
  # 30 August 2024: after updating the station names for some of the Guana station,
  # it seems like there are duplicates for some variable/value pairs. So that would/
  # should mean that it's exactly the same, so we can just pick one. If we don't, we 
  # end up with a vector for the ComponentLong name. Keep the first value.
  pivot_wider(names_from = variable, values_from = value, values_fn = first) %>%
  select(SampleDate, # we could also make these arguments for the function?
         ComponentLong, 
         Result,
         geometry, StationCode, site_friendly) %>%
  pivot_wider(names_from = ComponentLong, 
              values_from = Result,
              values_fn = list(Result = ~ mean(as.numeric(.), na.rm = TRUE))) %>%
  mutate(SampleDate = ymd_hms(SampleDate)) %>% # 
  #mutate(SampleDate = str_extract(SampleDate, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% 
  #mutate(SampleDate = ymd(SampleDate)) %>% # these two lines are another option to only get ymd
  mutate(across(-c(SampleDate, geometry, StationCode, site_friendly), ~ as.numeric(.))) %>%
  mutate(SampleDate = as.Date(SampleDate)) %>%
  group_by(SampleDate, geometry, StationCode, site_friendly) %>%
  summarize(across(everything(), ~mean(.x, na.rm = TRUE))) #%>% # across(everything()) is not necessary,
# strictly speaking, but it's nice to keep for if we ever want to adjust this function
# to work for more than 1 variable
#select(where(~ n_distinct(.) > 2))
##############

# CHECK: there is a column called NA??
df <- wide_df

selected_column = "Salinity"
units_df = WQ_data_units

fig <- plot_ly(df, x = ~SampleDate)

# Get the column names except the dates and column names and geometry
column_names <- sort(colnames(df)[!colnames(df) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])

# Create a named vector for Y-axis titles
y_axis_titles <- setNames(units_df$Unit, units_df$ComponentLong)

# Ensure selected_column is not NULL or empty
if (is.null(selected_column) || selected_column == "") {
  selected_column <- column_names[1]
}

# Loop through each column and add a trace
for (i in seq_along(column_names)) {
  fig <- fig %>%
    add_trace(y = df[[column_names[i]]], name = column_names[i], type = 'scatter', mode = 'lines+markers',
              showlegend = FALSE,
              visible = if (column_names[i] == selected_column) TRUE else FALSE)
}

fig
