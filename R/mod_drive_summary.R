#' describe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_drive_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      id = ns("layout"),
      style = "padding: 37px",
      bslib::layout_column_wrap(
        bslib::layout_column_wrap(
          class = "button-container",
          width = 1/2,
          height = "800px",
          fillable = FALSE,
          fill = FALSE,
          shiny::actionButton(
            inputId = ns("fleet_information_bttn"),
            label = shiny::div(class = "button-content", shiny::icon("van-shuttle"), "Fleet information"),
            class = "btn-primary"
          ),
          shiny::actionButton(
            inputId = ns("route_and_scheduling_bttn"),
            label = shiny::div(class = "button-content", shiny::icon("route"), "Route and scheduling"),
            class = "btn-primary"
          ),
          shiny::actionButton(
            inputId = ns("charging_infrastructure_bttn"),
            label = shiny::div(class = "button-content", shiny::icon("gas-pump"), "Charging infrastructure"),
            class = "btn-primary"
          ),
          shiny::actionButton(
            inputId = ns("energy_and_tariffs_bttn"),
            label = shiny::div(class = "button-content", shiny::icon("plug-circle-bolt"), "Energy and tariffs"),
            class = "btn-primary"
          )
        ),
        shiny::div(
          shinyjs::hidden(
            shiny::div(
              id = ns("fleet_information_layout"),
                bslib::card(
                bslib::card_header(
                  class = "d-flex justify-content-center",
                  shiny::h1(shiny::strong("FLEET INFORMATION"))
                  # shiny::icon("van-shuttle")
                ),
                bslib::card_body(
                  shiny::tableOutput(ns("fleet_table"))
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny::div(
              id = ns("route_and_scheduling_layout"),
              bslib::card(
                height = "100%",
                bslib::card_header(
                  class = "d-flex justify-content-center",
                  shiny::h1(shiny::strong("ROUTE AND SCHEDULING"))
                ),
                bslib::card_body(
                  leaflet::leafletOutput(ns("route_map"), height = 675) |> 
                    shinycssloaders::withSpinner(),
                  shiny::uiOutput(ns("route_info"))
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny::div(
              id = ns("charging_infrastructure_layout"),
              bslib::card(
                bslib::card_header(
                  class = "d-flex justify-content-center",
                  shiny::h1(shiny::strong("CHARGING INFRASTRUCTURE"))
                ),
                bslib::card_body(
                  shiny::tableOutput(ns("charging_table"))
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny::div(
              id = ns("energy_and_tariffs_layout"),
              bslib::card(
                bslib::card_header(
                  class = "d-flex justify-content-center",
                  shiny::h1(shiny::strong("ENERGY AND TARIFSS"))
                ),
                bslib::card_body(
                  bslib::layout_column_wrap(
                    width = 1,
                    echarts4r::echarts4rOutput(ns("energy_plot1"),height = "300px"),
                    echarts4r::echarts4rOutput(ns("energy_plot2"),height = "300px")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' describe Server Functions
#'
#' @noRd
mod_drive_summary_server <- function(id, aux) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$fleet_information_bttn, {
      golem::invoke_js(
        "bttn_active",
        list(
          active = ns("fleet_information_bttn"),
          desactive = c(
            ns("route_and_scheduling_bttn"),
            ns("charging_infrastructure_bttn"),
            ns("energy_and_tariffs_bttn")
          )
        )
      )
      shinyjs::show("fleet_information_layout")
      shinyjs::hide("route_and_scheduling_layout")
      shinyjs::hide("charging_infrastructure_layout")
      shinyjs::hide("energy_and_tariffs_layout")
    })
    output$fleet_table <- shiny::renderTable({
      shiny::req(aux$drive_tech_data)

      if (aux$drive_tech_data$type == "file") {
        aux$drive_tech_data[[1]] |> 
          dplyr::select(`Bus ID`, `Buses`, `Length...4`, `Bus Brand`) |> 
            tidyr::drop_na() |> 
            dplyr::rename(
              "Vehicle Number" = "Bus ID", 
              "Battery Capacity [kWh]" = "Buses",
              "Vehicle size [m]" = "Length...4",
              "Brand" = "Bus Brand"
            )
        # reactable::reactable(
        #   defaultPageSize = 20,
        #   columns = list(
        #     `Bus ID` = reactable::colDef(
        #       name = "#ID"
        #     ),
        #     `Buses` = reactable::colDef(
        #       name = "Battery"
        #     ),
        #     `Length...4` = reactable::colDef(
        #       name = "Length"
        #     ),
        #     `Bus Brand` = reactable::colDef(
        #       name = "Brand"
        #     )
        #   ),
        #   defaultColDef = reactable::colDef(
        #     minWidth = 175,
        #     align = "center"
        #   )
        # )
      } else {
          aux$drive_tech_data$drive_tech_manual_input_bus |> 
            tidyr::drop_na() |> 
            dplyr::rename(
              "ID" = "bus_id",
              "Name" = "bus_manufacturer",
              "Battery Capacity [kWh]" = "bus_battery_capacity"
            )
      }

    }, align = "c", digits = 0, hover = TRUE)

    shiny::observeEvent(input$route_and_scheduling_bttn, {
      golem::invoke_js(
        "bttn_active",
        list(
          active = ns("route_and_scheduling_bttn"),
          desactive = c(
            ns("fleet_information_bttn"),
            ns("charging_infrastructure_bttn"),
            ns("energy_and_tariffs_bttn")
          )
        )
      )
      shinyjs::hide("fleet_information_layout")
      shinyjs::show("route_and_scheduling_layout")
      shinyjs::hide("charging_infrastructure_layout")
      shinyjs::hide("energy_and_tariffs_layout")
    })
    output$route_map <- leaflet::renderLeaflet({
      shiny::req(aux$drive_tech_data)
      
      aux$map_tmp |> 
        leaflet::leaflet(
          options = leaflet::leafletOptions()
        ) |>
        leaflet:::setView(lng = -8.4196, lat = 40.2056, 13) |> 
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |> 
        leaflet::addPolylines(layerId = ~idtroco, color = "darkgrey")
    })
    shiny::observeEvent(input$route_map_shape_click, {
      id_selected <- aux$map_tmp[aux$map_tmp$idtroco == input$route_map_shape_click$id, ]
      
      data_filter <- aux$map_tmp[aux$map_tmp$codseroper %in% unique(id_selected$codseroper), ]

      leaflet::leafletProxy(ns("route_map")) |> 
        leaflet::clearGroup("highlighted") |> 
        leaflet::addPolylines(
          data = data_filter, 
          color = "red", 
          group = "highlighted"
        )

      print(list(
        line = unique(data_filter$codseroper),
        origem = unique(data_filter$origem),
        destination = unique(data_filter$destino)
      ))

       output$route_info <- shiny::renderUI({
          bslib::layout_column_wrap(
            bslib::value_box(
              title = "Line",
              value = unique(data_filter$codseroper)[1]
            ),
            bslib::value_box(
              title = "Origin",
              value = unique(data_filter$origem)[1]
            ),
            bslib::value_box(
              title = "Destination",
              value = unique(data_filter$destino)[1]
            )
          )
       })
    })

    # output$route_table <- shiny::renderTable({
    #   shiny::req(aux$drive_tech_data)

    #   data_filter <- aux$map_tmp[aux$map_tmp$idtroco == input$route_map_shape_click$id, ]

    #   shiny::validate(
    #     shiny::need(nrow(data_filter) > 0, "No street selected")
    #   )

    #   data_filter |> 
    #     dplyr::select(origem, destino, tipoveic, tipoalimt) |> 
    #     sf::st_drop_geometry()
    #     # reactable::reactable()
    # }, align = "c")

    shiny::observeEvent(input$charging_infrastructure_bttn, {
      golem::invoke_js(
        "bttn_active",
        list(
          active = ns("charging_infrastructure_bttn"),
          desactive = c(
            ns("fleet_information_bttn"),
            ns("route_and_scheduling_bttn"),
            ns("energy_and_tariffs_bttn")
          )
        )
      )
      shinyjs::hide("fleet_information_layout")
      shinyjs::hide("route_and_scheduling_layout")
      shinyjs::show("charging_infrastructure_layout")
      shinyjs::hide("energy_and_tariffs_layout")
    })
    output$charging_table <- shiny::renderTable({
      shiny::req(aux$drive_tech_data)

      if (aux$drive_tech_data$type == "file") {
        aux$drive_tech_data[[1]] |> 
          dplyr::select(`Charger ID`, Charger, `Status`, `Brand`) |> 
            tidyr::drop_na() |> 
            dplyr::rename(
              "Charger Number" = "Charger ID",
              "Power [kW]" = "Charger",
              "Type" = "Status",
              "Brand" = "Brand"
            )
        # reactable::reactable(
        #   defaultPageSize = 20,
        #   columns = list(
        #     `Charger ID` = reactable::colDef(
        #       name = "#ID"
        #     ),
        #     `Charger` = reactable::colDef(
        #       name = "Power"
        #     ),
        #     `Status` = reactable::colDef(
        #       name = "Status"
        #     ),
        #     `Charger Brand` = reactable::colDef(
        #       name = "Brand"
        #     )
        #   ),
        #   defaultColDef = reactable::colDef(
        #     minWidth = 175,
        #     align = "center"
        #   )
        # )
      } else {
         aux$drive_tech_data$drive_tech_manual_input_charger |> 
            tidyr::drop_na() |> 
            dplyr::rename(
              "ID" = "charger_id",
              "Name" = "charger_manufacturer",
              "Power [kW]" = "charger_power"
            )
      }

    }, align = "c", digits = 0, hover = TRUE)

    shiny::observeEvent(input$energy_and_tariffs_bttn, {
      golem::invoke_js(
        "bttn_active",
        list(
          active = ns("energy_and_tariffs_bttn"),
          desactive = c(
            ns("fleet_information_bttn"),
            ns("route_and_scheduling_bttn"),
            ns("charging_infrastructure_bttn")
          )
        )
      )
      shinyjs::hide("fleet_information_layout")
      shinyjs::hide("route_and_scheduling_layout")
      shinyjs::hide("charging_infrastructure_layout")
      shinyjs::show("energy_and_tariffs_layout")
    })
    output$energy_plot1 <- echarts4r::renderEcharts4r({
      shiny::req(aux$drive_tech_data)
      
      start_time <- as.POSIXct("2024-06-23 00:00:00")
      end_time <- as.POSIXct("2024-06-23 23:45:00")
      time_sequence <- seq(from = start_time, to = end_time, by = "15 min")

      if (aux$drive_tech_data$type == "file") {
        aux$drive_tech_data[[1]] |> 
          dplyr::select(`Energy price`) |> 
          tidyr::drop_na() |> 
          dplyr::mutate(
            index = time_sequence
          ) |> 
          echarts4r::e_chart(index) |> 
          echarts4r::e_line(`Energy price`, legend = FALSE) |> 
          echarts4r::e_x_axis(name = "Time[Hour]", nameLocation = "middle", nameTextStyle = list(fontSize = 16), nameGap = 30) |> 
          echarts4r::e_y_axis(name = "CAD$/kWh", nameLocation = "middle", nameTextStyle = list(fontSize = 16), nameGap = 50) |> 
          echarts4r::e_tooltip(trigger = "axis") |> 
          echarts4r::e_theme("dark-bold") 
      } else {
        # print(aux$drive_tech_data[["drive_tech_manual_input_price"]])
        tibble::tibble(price = rep(aux$drive_tech_data$drive_tech_manual_input_price$price, each=4)) |> 
          tidyr::drop_na() |> 
          dplyr::mutate(
            index = time_sequence
          ) |> 
          echarts4r::e_chart(index) |> 
          echarts4r::e_line(price, legend = FALSE) |> 
          echarts4r::e_x_axis(name = "Time[Hour]", nameLocation = "middle", nameTextStyle = list(fontSize = 16), nameGap = 30) |> 
          echarts4r::e_y_axis(name = "CAD$/kWh", nameLocation = "middle", nameTextStyle = list(fontSize = 16), nameGap = 50) |> 
          echarts4r::e_tooltip(trigger = "axis") |> 
          echarts4r::e_theme("dark-bold") 
      }
    })

    output$energy_plot2 <- echarts4r::renderEcharts4r({
      shiny::req(aux$drive_tech_data)

      if (aux$drive_tech_data$type == "file") {
        aux$drive_tech_data[[1]] |> 
          dplyr::select(`Peak power`, `Power price`) |> 
          tidyr::drop_na() |> 
          echarts4r::e_chart(`Peak power`) |> 
          echarts4r::e_bar(`Power price`, legend = FALSE) |> 
          echarts4r::e_x_axis(
            name = "kW", 
            nameLocation = "middle", 
            nameTextStyle = list(fontSize = 16), 
            nameGap = 30,
            min = 0, max = 500, interval = 50
          ) |> 
          echarts4r::e_y_axis(name = "CAD$/kW", nameLocation = "middle", nameTextStyle = list(fontSize = 16), nameGap = 30) |> 
          echarts4r::e_tooltip() |> 
          echarts4r::e_theme("dark-bold")
      } else {

      }
    })
  })
}

## To be copied in the UI
# mod_drive_summary_ui("describe_1")

## To be copied in the server
# mod_drive_summary_server("describe_1")
