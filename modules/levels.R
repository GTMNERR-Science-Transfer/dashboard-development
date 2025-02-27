########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# Created Feb 2025
# Last updated: 25 Feb 2025

# This page displays water level data

#### Location data ------------------------------------------------
precip <- readRDS("./03_Data_for_app/precip.Rds")
dam_levels <- readRDS("./03_Data_for_app/dam_level.Rds")



### Define the UI -------------------------------------------------------------
dash_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone"
) |>
  bs_add_variables(
    "navbar-bg" = "$primary",
    "navbar-color" = "$light",
    #"progress-bar-bg" = "$secondary",
    .where = "declarations"
  ) |>
  bs_add_rules("
    .navbar { color: var(--bs-light) !important; }
    .navbar .navbar-brand, .navbar .nav-link { color: var(--bs-light) !important; }
  ")

levelsPageUI <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "sandstone"),
    
    title = "Hydrological Data",
    
    sidebar = sidebar(
      title = "Data Selection",
      selectInput(ns("data_type"), 
                  label = "What kind of hydrological data are you looking for?", 
                  choices = c("Precipitation", "Dam levels"),
                  selected = ""),
      sliderInput(
        inputId = ns("date_range"),
        label = "Select a Date Range",
        min = min(precip$date), #NULL
        max = max(precip$date), #NULL
        value = c(min(precip$date), 
                  max(precip$date)),
        timeFormat = "%m/%d/%Y",
        width = "100%"
      ),
      selectInput(ns("aggregation"),
                  label = "How do you want the data aggregated?",
                  choices = c("Daily", "Monthly average", "Annual average"),
                  selected = "")
    ),
    
    # Main content (text, map, plots)
    layout_columns(
      col_widths = c(12), # Ensures full width for the header card
      card(
        fill = TRUE, # Ensures no scroll bars as long as height is set
        height = "400px",
        card_header("Hydrological Data"),
        card_body(
          p(HTML("There is data available for precipitation (inches) at the GTMNERR Welcome
                 Center, and water level data at the dam (unitless, relative to surveyed elevation"))
        )
      )
    ),
    
    layout_column_wrap( # two plots: time and histogram
      width = 1/2,
      card(
        full_screen = TRUE, # Let's you click and enlarge the card to full screen
        card_header("Values over time"),
        card_body(
          plotOutput(ns("timePlot"))
        )

      ),
      card(
        full_screen = TRUE,
        card_header("Distribution of values"),
        card_body(
          plotOutput(ns("distribution"))
        )
      )
    )
  )
}

### Define the server logic ----------------------------------------------------

levelsPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns

    # Make the dataframes reactive for plotting
    #precip_df <- reactiveVal()
    #dam_levels_df <- reactiveVal()
    plot_data <- reactiveVal(data.frame())

    ### Update if dataset changes ####
    observeEvent({ # If the selected algae type changes
      input$data_type
    },{ # Filter dataframe
      req(input$data_type, input$date_range, input$aggregation)

      if (input$data_type == "Precipitation"){
        plot_data(precip)
      } else if (input$data_type == "Dam levels"){
        plot_data(dam_levels)
      }

      print(paste0("You selected data type(s) ", input$data_type))
      
      df <- plot_data()

      if (input$aggregation == "Daily"){
        plot_data(df)
      } else if (input$aggregation == "Monthly average"){
        plot_data(df %>%
                    group_by(year, month, location) %>%
                    summarize(mean_vals = mean(value, na.rm=TRUE)) %>%
                    ungroup() %>% 
                    mutate(date = dmy(paste("1", month, year)))
        )
      } else if (input$aggregation == "Annual average"){
        plot_data(df %>%
                    group_by(year, location) %>%
                    summarize(mean_vals = mean(value, na.rm=TRUE)) %>% 
                    ungroup()
        )
      }
      print(plot_data())
    }, ignoreInit = TRUE)

    ### Update if aggregation changes ####
    observeEvent({ # If the selected aggregation type changes
      input$aggregation
    },{ # Filter dataframe
      req(input$data_type != "", input$date_range, input$aggregation != "")
      
      print(paste0("Aggregation is now ", input$aggregation))

      if (input$data_type == "Precipitation"){
        plot_data(precip)
      } else if (input$data_type == "Dam levels"){
        plot_data(dam_levels)
      }
      
      df <- plot_data()

      print(paste0("You selected data type(s) ", input$data_type))

      if (input$aggregation == "Daily"){
        plot_data(df)
      } else if (input$aggregation == "Monthly average"){
        plot_data(df %>%
                    group_by(year, month, location) %>%
                    summarize(mean_vals = mean(value, na.rm=TRUE)) %>%
                    ungroup() %>% 
                    mutate(date = dmy(paste("1", month, year)))
        )
      } else if (input$aggregation == "Annual average"){
        plot_data(df %>%
                    group_by(year, location) %>%
                    summarize(mean_vals = mean(value, na.rm=TRUE)) %>%
                    ungroup()
        )
      }
      print(plot_data())
    }, ignoreInit = TRUE)

    ### Create plots ####
    output$timePlot <- renderPlot({
      req(nrow(plot_data()) > 0, input$aggregation != "")
      
      if (input$data_type == "Precipitation"){
        col_loc <- c("purple", "orange")
        axis_text <- "Precipitation (inches)"
      } else if (input$data_type == "Dam levels"){
        col_loc <- c("royalblue")
        axis_text <- "Dam level (ft above reference)"
      }

      if (input$aggregation == "Daily"){
        p <- ggplot(data = plot_data(), aes(x = date, y = value, color = location)) +
          geom_point() +
          geom_line()+
          labs(x = "Date", y = axis_text)+
          scale_color_manual(name = "Location", values = col_loc)+
          theme_bw()+
          theme(legend.position = "bottom")
      } else if (input$aggregation == "Monthly average"){
        p <- ggplot(data = plot_data(), aes(x = date, y = mean_vals, color = location)) +
          geom_point() +
          geom_line()+
          labs(x = "Date", y = axis_text)+
          scale_color_manual(name = "Location", values = col_loc)+
          theme_bw()+
          theme(legend.position = "bottom")
      } else if (input$aggregation == "Annual average"){
        p <- ggplot(data = plot_data(), aes(x = year, y = mean_vals, color = location)) +
          geom_point() +
          geom_line()+
          labs(x = "Year", y = axis_text)+
          scale_color_manual(name = "Location", values = col_loc)+
          theme_bw()+
          theme(legend.position = "bottom")
      }
      
      p

      # gp <- ggplotly(p,
      #                dynamicTicks = TRUE)
      # 
      # gp
    })

    output$distribution <- renderPlot({
      req(nrow(plot_data()) > 0, input$aggregation != "")
      
      if (input$data_type == "Precipitation"){
        col_loc <- c("purple", "orange")
        axis_text <- "Precipitation (inches)"
      } else if (input$data_type == "Dam levels"){
        col_loc <- c("royalblue")
        axis_text <- "Dam level (ft above reference)"
      }

      if (input$aggregation == "Daily"){
        p <- ggplot(data = plot_data(), aes(x = value, fill = location)) +
          geom_histogram() +
          labs(x = axis_text, y = "Counts")+
          scale_fill_manual(name = "Location", values = col_loc)+
          theme_bw()+
          theme(legend.position = "bottom")
      } else if (input$aggregation == "Monthly average"){
        p <- ggplot(data = plot_data(), aes(x = mean_vals, fill = location)) +
          geom_histogram() +
          labs(x = axis_text, y = "Counts")+
          scale_fill_manual(name = "Location", values = col_loc)+
          theme_bw()+
          theme(legend.position = "bottom")
      } else if (input$aggregation == "Annual average"){
        p <- ggplot(data = plot_data(), aes(x = mean_vals, fill = location)) +
          geom_histogram() +
          labs(x = axis_text, y = "Counts")+
          scale_fill_manual(name = "Location", values = col_loc)+
          theme_bw()+
          theme(legend.position = "bottom")
      }
      
      p

      # gp <- ggplotly(p,
      #                dynamicTicks = TRUE)
      # 
      # gp
    })
  })
}