#' upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput dateRangeInput icon tabPanel dataTableOutput renderDataTable incProgress bindEvent
#' @importFrom shinydashboard box tabBox
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput ggplotly renderPlotly layout plot_ly event_data
#' @importFrom dplyr select filter mutate mutate_if group_by summarise across n
#' @importFrom lubridate as_datetime parse_date_time2 round_date floor_date
#' @importFrom shinipsum random_ggplotly
#' @importFrom ggplot2 ggplot aes geom_line annotate geom_boxplot labs scale_color_manual theme facet_wrap
#' @importFrom tidyr gather
mod_geoh_upload_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(

      # Inputs ----
      column(
        width = 2,
        box(
          title = tagList("Inputs", shiny::icon("flask-vial")), id = "inputs_tab",
          status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
          ## file button ----
          fileInput(
            inputId     = ns("file_trip"),
            label       = "Upload trip data",
            multiple    = FALSE,
            accept      = ".csv",
            buttonLabel = "Browse",
            placeholder = "No file selected"
          ),
          ## button ----
          div(
            actionButton(
              inputId = ns("button_analyze"),
              label = "Analyze",
              style = "color: #fff; background-color: #3c8dbc"
            ),
            style = "text-align: center;"
          )
        ),
      ),

      # Info ----
      column(
        width = 5,
        ## plots ----
        box(
          title = tagList("Plots", shiny::icon("chart-line")), id = "plot_tab",
          status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
          plotlyOutput(ns("plot_variables")) %>% withSpinner(),
          fluidRow(
            valueBoxOutput(ns("total_co2"), width = 12),
            valueBoxOutput(ns("avg_fuel_rate"), width = 12),
            valueBoxOutput(ns("avg_gps_speed"), width = 12),
            valueBoxOutput(ns("avg_wheel_acc"), width = 12)
          )
        ),

        ## map ----
        box(
          title = tagList("Map", shiny::icon("map")), id = "map_tab",
          status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
          plotlyOutput(ns("plot_map")) %>% withSpinner()
        )
      ),
      column(
        width = 5,
        ## summary ----
        box(
          title = tagList("Fleet Breakdown", shiny::icon("gauge")), id = "summary_tab",
          status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, style = "overflow-y: scroll;overflow-x: scroll;",
          dataTableOutput(ns("table_summary")) %>% withSpinner()
        ),

        ## table ----
        box(
          title = tagList("Raw Data", shiny::icon("table")), id = "table_tab",
          status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, style = "overflow-y: scroll;overflow-x: scroll;",
          dataTableOutput(ns("table_data")) %>% withSpinner()
        )
      )
    )
  )
}

#' upload_data Server Functions
#'
#' @noRd
mod_geoh_upload_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # read file ----
    trip_data <- reactive({
      req(input$file_trip)

      result <- read.csv(input$file_trip$datapath)

      if ("datetime" %in% colnames(result)) {
        result <- result |>
          dplyr::mutate(datetime = lubridate::parse_date_time2(datetime, orders = "%Y/%m/%d %H:%M:%S"))
      } else {
        result$datetime <- lubridate::now() + 1:nrow(result)
      }

      if (!("truck" %in% colnames(result))) {
        result$truck <- 1
      }

      return(result)
    })

    # plot ----
    output$plot_variables <- renderPlotly({
      req(trip_data)

      trip_data <- trip_data()
      # trip_data <- readr::read_csv("./inst/join_trip_data.csv") |>
      #   mutate(datetime = parse_date_time2(datetime, orders = "%Y/%m/%d %H:%M:%S"))

      aux <- trip_data %>%
        dplyr::select(datetime, fcr, co2) %>%
        dplyr::group_by(datetime) |>
        dplyr::summarise_all(sum) |>
        tidyr::gather(
          key = "variable",
          value = "value",
          -datetime
        )

      plot <- aux %>%
        ggplot(aes(x = datetime, y = value, color = variable)) +
        geom_line() +
        facet_wrap(~variable, nrow = 3, scales = "free_y")

      ggplotly(plot, source = "plot_timeseries") %>% layout(xaxis = list(rangeslider = list(type = "date")))
    })

    output$total_co2 <- shinydashboard::renderValueBox({
      req(trip_data)

      shinydashboard::valueBox(
        format(round(sum(trip_data()$co2), 2), big.mark = ".", decimal.mark = ","),
        subtitle = "Total CO2 (grams)",
        icon = shiny::icon("wind"),
        color = "maroon"
      )
    })

    output$avg_fuel_rate <- shinydashboard::renderValueBox({
      req(trip_data)

      shinydashboard::valueBox(
        format(round(mean(trip_data()$fcr), 2), big.mark = ".", decimal.mark = ","),
        subtitle = "Avg. Fuel Rate (L/km)",
        icon = shiny::icon("gas-pump"),
        color = "maroon"
      )
    })

    output$avg_gps_speed <- shinydashboard::renderValueBox({
      req(trip_data)

      shinydashboard::valueBox(
        format(round(mean(trip_data()$gps_speed), 2), big.mark = ".", decimal.mark = ","),
        subtitle = "Avg. Speed (km/H)",
        icon = shiny::icon("gauge"),
        color = "maroon"
      )
    })

    output$avg_wheel_acc <- shinydashboard::renderValueBox({
      req(trip_data)

      shinydashboard::valueBox(
        format(round(mean(trip_data()$wheel_acc), 2), big.mark = ".", decimal.mark = ","),
        subtitle = "Avg. Acceleration",
        icon = shiny::icon("gauge-simple-high"),
        color = "maroon"
      )
    })

    ### reactive to plot slider ---
    datetime_range <- reactive({
      req(trip_data)

      result <- list()

      aux <- trip_data()
      zoom <- event_data("plotly_relayout", source = "plot_timeseries")
      if (is.null(zoom) || names(zoom[1]) %in% c("xaxis.autorange", "width")) {
        result$start <- min(aux$datetime)
        result$end <- max(aux$datetime)
        result$range <- aux$datetime
      } else {
        result$start <- zoom$xaxis.range[1] %>%
          as_datetime() %>%
          round_date("second")
        result$end <- zoom$xaxis.range[2] %>%
          as_datetime() %>%
          round_date("second")
        result$range <- seq(from = result$start, to = result$end, by = "sec")
      }

      return(result)
    })

    # map ----
    output$plot_map <- renderPlotly({
      req(trip_data, datetime_range)
      print("check map")

      datetimes <- datetime_range()

      trip_data <- trip_data()

      map <- trip_data %>%
        dplyr::filter(datetime >= datetimes$start & datetime <= datetimes$end) %>%
        dplyr::select(datetime, truck, Latitude, Longitude)

      map %>%
        dplyr::mutate(truck = as.character(truck)) |>
        plot_ly(
          lat = ~Latitude,
          lon = ~Longitude,
          color = ~truck,
          # marker = list(
          #   color = app_colors$map_marker,
          #   size = 5,
          #   line = list(
          #     color = "#292928",
          #     width = 3
          #   )
          # ),
          type = "scattermapbox",
          hovertext = map$datetime %>% floor_date(unit = "second")
        ) |>
        layout(
          mapbox = list(
            style  = "open-street-map",
            zoom   = 10,
            center = list(lon = mean(map$Longitude), lat = mean(map$Latitude))
          )
        )
    }) %>% bindEvent(input$button_analyze)


    # summary ----
    output$table_summary <- renderDataTable({
      req(trip_data, datetime_range)

      datetimes_range <- datetime_range()
      trip_data <- trip_data()

      aux <- trip_data %>%
        # filter(datetime >= datetimes_range$start &
        #   datetime <= datetimes_range$end) %>%
        dplyr::select(datetime, truck, gps_speed, wheel_acc, slope) %>%
        dplyr::mutate(truck = as.character(truck)) |>
        tidyr::gather(
          key = "variable", value = "value",
          -datetime,
          -truck
        ) %>%
        group_by(truck, variable) %>%
        summarise(
          across(
            .cols = "value",
            .names = "{.fn}",
            .fns = list(
              observations = ~ n(),
              mean         = mean,
              std_dev      = sd,
              min          = min,
              quantile_25  = ~ quantile(., probs = 0.25),
              quantile_50  = ~ quantile(., probs = 0.50),
              quantile_75  = ~ quantile(., probs = 0.75),
              max          = max
            )
          )
        ) %>%
        mutate_if(is.numeric, ~ round(., 3))
    }) %>% bindEvent(input$button_analyze)

    # table ----
    output$table_data <- renderDataTable({
      req(trip_data, datetime_range)

      datetimes_range <- datetime_range()

      aux <- trip_data() %>%
        filter(datetime >= datetimes_range$start &
          datetime <= datetimes_range$end)
    }) %>% bindEvent(input$button_analyze)

    return(result <- reactive({
      trip_data()
    }))
  })
}

## To be copied in the UI
# mod_geoh_upload_data_ui("upload_data_1")

## To be copied in the server
# mod_geoh_upload_data_server("upload_data_1")
