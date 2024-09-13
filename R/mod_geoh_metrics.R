#' metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geoh_metrics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # bg = "darkgrey",
          open = "open",
          width = 350,
          shiny::fileInput(
            inputId = ns("file_trip"),
            label = "Upload trip data",
            multiple = TRUE,
            accept = ".csv",
            buttonLabel = "Browse",
            placeholder = "No file selected"
          )
        ),
        fluidRow(
          bslib::layout_columns(
            col_widths = c(2, 6, 4),
            reactable::reactableOutput(ns("trip_data_files")),
            shiny::div(
              style = "width: 100%",
              leaflet::leafletOutput(ns("trip_map"), height = 600),
              gt::gt_output(ns("trip_data_summarized_by_vehicle"))
            ),
            uiOutput(ns("indicators"))
          )
        )  
        # fluidRow(
        #   reactable::reactableOutput(ns("trip_data_raw"))
        # )
      )
    )
  )
}

#' metrics Server Functions
#'
#' @noRd
mod_geoh_metrics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    aux <- reactiveValues(
      trip_data_files_load = NULL
    )

    trip_data <- reactive({
      req(input$file_trip)

      # NOTE assuming that the headers will be always like that .. variable [unit]
      datapath <- input$file_trip
      datapath$id <- 1:nrow(datapath)

      data <- tibble::tibble()

      for (i in 1:nrow(datapath)) {
        data_new <- readr::read_csv(datapath$datapath[i])
        # data <- readr::read_csv("./data/test_data/020_Honda_Civic_2014_(1.8L_Auto).csv")

        units <- stringr::str_extract(colnames(data_new), "\\[.+\\]")
        units <- stringr::str_replace_all(units, "[\\[\\].-]+", "")

        cs <- stringr::str_extract(colnames(data_new), ".+(?=\\[)")
        cs[which(is.na(cs))] <- colnames(data_new)[which(is.na(cs))]
        cs <- stringr::str_replace_all(cs, "[[:punct:]]+", "")
        cs <- tolower(cs)
        cs <- trimws(cs)
        cs <- stringr::str_replace_all(cs, "\\s+", "_")
        cs <- paste0(cs, "_", units)
        cs <- stringr::str_replace_all(cs, "_NA", "")

        colnames(data_new) <- cs

        data_new$id <- datapath$id[i]
        data_new$filename <- datapath$name[i]

        data <- data |>
          dplyr::bind_rows(data_new)
      }

      aux$trip_data_files_load <- datapath |>
        dplyr::select(id, name) |>
        dplyr::rename(filename = name)

      return(data)
    })

    output$trip_data_files <- reactable::renderReactable({
      req(trip_data())

      reactable::reactable(
        aux$trip_data_files_load |>
          dplyr::arrange(id) |>
          dplyr::select(filename),
        selection = "multiple",
        onClick = "select",
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultSelected = 1:nrow(aux$trip_data_files_load),
        columns = list(
          filename = reactable::colDef(name = "File Name")
        ),
        theme = reactable::reactableTheme(
          color = "hsl(233, 9%, 87%)",
          backgroundColor = "hsl(233, 9%, 19%)",
          borderColor = "hsl(233, 9%, 22%)",
          stripedColor = "hsl(233, 12%, 22%)",
          highlightColor = "hsl(233, 12%, 24%)",
          inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
        )
      )
    })
    selected <- reactive(reactable::getReactableState("trip_data_files", "selected"))

    trip_data_selected <- reactive({
      req(trip_data())

      trip_data() |>
        dplyr::filter(id %in% aux$trip_data_files_load[selected(), ]$id)
    })

    output$trip_map <- leaflet::renderLeaflet({
      req(trip_data_selected())

      trip_data <- trip_data_selected()

      validate(
        need(nrow(trip_data) > 0, "No data selected")
      )

      # saveRDS(trip_data, "trip_data.rds")
      # trip_data <- readRDS("trip_data.rds")

      pal <- RColorBrewer::brewer.pal(length(unique(trip_data$id)), "Set1")

      pal_data <- trip_data |>
        dplyr::distinct(id)
      pal_data$color <- pal[1:nrow(pal_data)]
      pal_data <- pal_data |>
        dplyr::left_join(aux$trip_data_files_load, by = "id")

      trip_data_list <- split(trip_data, trip_data$id)

      map <- leaflet::leaflet() |>
        leaflet::addTiles()
      # leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

      purrr::walk(trip_data_list, function(.x) {
        map <<- map |>
          leaflet::addPolylines(
            data = .x,
            lng = ~longitude_deg,
            lat = ~latitude_deg,
            color = pal_data[pal_data$id == unique(.x$id), ]$color,
            group = pal_data[pal_data$id == unique(.x$id), ]$filename
          )
      })

      map |>
        leaflet::addLegend(
          colors = pal_data$color,
          labels = pal_data$filename,
          position = "bottomright"
        )
    })

    output$indicators <- renderUI({
      req(trip_data_selected())

      trip_data <- trip_data_selected()

      vbs <- list(
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "Total CO2"),
          value = paste0(round(sum(trip_data$co2_v) / 1000, 2), " kg"),
          showcase = bsicons::bs_icon("wind"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "AVG. Fuel Rate"),
          value = paste0(round(mean(trip_data$fuel_rate1_gs1[trip_data$fuel_rate1_gs1 != 0]), 2), " g/s"),
          showcase = bsicons::bs_icon("fuel-pump-fill"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "AVG. Speed"),
          value = paste0(round(mean(trip_data$gps_speed_kmh1[trip_data$gps_speed_kmh1 != 0]), 2), " km/h"),
          showcase = bsicons::bs_icon("speedometer2"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "AVG. CO2 per Vehicule"),
          value = paste0(round(trip_data |>
            dplyr::filter(co2_v != 0) |>
            dplyr::group_by(id) |>
            dplyr::summarise(co2 = mean(co2_v)) |>
            dplyr::pull(co2) |>
            mean(), 2), " g/s"),
          showcase = bsicons::bs_icon("speedometer2"),
          class = "bg-primary"
        )
      )

      tagList(
        vbs[[1]],
        bslib::layout_column_wrap(
          width = "250px",
          !!!vbs[2:4]
        )
      )
    })

    output$trip_data_summarized_by_vehicle <- gt::render_gt({
      req(trip_data_selected())

      trip_data <- trip_data_selected()
      # saveRDS(trip_data, "tmp.rds")
      # trip_data <- readRDS("tmp.rds")

      trip_data |>
        dplyr::mutate(filename = stringr::str_replace(filename, ".csv", "")) |>
        dplyr::group_by(filename) |>
        dplyr::summarise(
          total_co2 = sum(co2_v) / 1000,
          avg_fuel_rate = mean(fuel_rate1_gs1[fuel_rate1_gs1 != 0]),
          avg_speed = mean(gps_speed_kmh1[gps_speed_kmh1 != 0]),
          avg_co2 = mean(co2_v[co2_v != 0])
        ) |>
        gt::gt() |>
        # gtExtras::gt_theme_espn() |>
        gt::fmt_number(-1) |>
        gt::cols_align(
          "center",
          gt::everything()
        ) |> 
        gt::cols_label(
          filename = gt::md("<b>File Name</b>"),
          total_co2 = gt::md("<b>Total CO2 (kg)</b>"),
          avg_fuel_rate = gt::md("<b>AVG. Fuel Rate (g/s)</b>"),
          avg_speed = gt::md("<b>AVG. Speed (km/s)</b>"),
          avg_co2 = gt::md("<b>AVG. CO2 (g/s)</b>")
        )
    }, width = "auto", align = "center")

    output$trip_data_raw <- reactable::renderReactable({
      req(trip_data_selected())

      trip_data <- trip_data_selected()

      reactable::reactable(
        trip_data,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_geoh_metrics_ui("metrics_1")

## To be copied in the server
# mod_geoh_metrics_server("metrics_1")
