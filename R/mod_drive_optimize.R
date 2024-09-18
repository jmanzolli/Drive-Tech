#' optimize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import ggplot2
mod_drive_optimize_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fluid(
    id = ns("optimization_layout"),
    style = "padding: 37px",
    bslib::layout_columns(
      col_widths = c(3, 9),
      bslib::layout_column_wrap(
        class = "button-container",
        width = 1,
        height = "1000px",
        fillable = FALSE,
        fill = FALSE,
        shiny::actionButton(
          inputId = ns("results_bttn"),
          label = shiny::div(class = "button-content", shiny::icon("chart-line"), "Results"),
          class = "btn-primary"
        ),
        shiny::actionButton(
          inputId = ns("analytics_bttn"),
          label = shiny::div(class = "button-content", shiny::icon("file-lines"), "Analytics"),
          class = "btn-primary"
        ),
        shiny::actionButton(
          inputId = ns("export_bttn"),
          label = shiny::div(class = "button-content", shiny::icon("download"), "Export"),
          class = "btn-primary"
        )
      ),
      bslib::page_fluid(
        shinyjs::hidden(
          shiny::div(
            id = ns("results_layout"),
            bslib::card(
              bslib::card_header(
                class = "d-flex justify-content-center",
                style = "background-color: #9A9999",
                shiny::h2(shiny::strong("RESULTS"))
              ),
              bslib::card_body(
                shiny::uiOutput(ns("results_output")),
                bslib::layout_column_wrap(
                  width = 1 / 2,
                  shiny::column(
                    12,
                    echarts4r::echarts4rOutput(ns("results_plot1")),
                    shiny::br(),
                    echarts4r::echarts4rOutput(ns("results_plot2"))
                  ),
                  echarts4r::echarts4rOutput(ns("results_plot3"))
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          shiny::div(
            id = ns("analytics_layout"),
            bslib::card(
              bslib::card_header(
                class = "d-flex justify-content-center",
                style = "background-color: #9A9999",
                shiny::h2(shiny::strong("FLEET"))
              ),
              bslib::card_body(
                bslib::layout_column_wrap(
                  width = 1 / 3,
                  bslib::card(
                    bslib::card_header("ENERGY CONSUMPTION"),
                    bslib::card_body(
                      shiny::uiOutput(ns("analytics_energy_output_totals"))
                    )
                  ),
                  bslib::card(
                    bslib::card_header("CHARGING TIME"),
                    bslib::card_body(
                      shiny::uiOutput(ns("analytics_charging_output_totals"))
                    )
                  ),
                  bslib::card(
                    bslib::card_header("FLEET STATE"),
                    bslib::card_body(
                      echarts4r::echarts4rOutput(ns("fleet_state_output_totals"))
                    )
                  )
                )
              )
            ),
            shiny::br(),
            bslib::card(
              bslib::card_header(
                class = "d-flex justify-content-center",
                style = "background-color: #9A9999",
                shiny::h2(shiny::strong("BUSES"))
              ),
              bslib::card_body(
                 shiny::column(
                  width = 12,
                  align = "center",
                  shinyWidgets::pickerInput(
                    inputId = ns("bus_selected"),
                    label = "Select a bus",
                    choices = ""
                  )
                ),
                bslib::layout_column_wrap(
                  width = 1 / 3,
                  bslib::card(
                    bslib::card_header("ENERGY CONSUMPTION"),
                    bslib::card_body(
                      shiny::uiOutput(ns("analytics_energy_output"))
                    )
                  ),
                  bslib::card(
                    bslib::card_header("CHARGING TIME"),
                    bslib::card_body(
                      shiny::uiOutput(ns("analytics_charging_output"))
                    )
                  ),
                  bslib::card(
                    bslib::card_header("FLEET STATE"),
                    bslib::card_body(
                      echarts4r::echarts4rOutput(ns("fleet_state_output_plot"))
                    )
                  )
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          shiny::div(
            id = ns("export_layout"),
            shiny::column(
              12,
              align = "center",
              shinyWidgets::pickerInput(
                inputId = ns("output_sheet"),
                label = "Select one output",
                choices = NULL
              ),
              shiny::downloadButton(ns("export_data"), "Download")
            ),
            bslib::card(
              max_height = "1000px",
              bslib::card_header("Sheet"),
              bslib::card_body(shiny::tableOutput(ns("export_table")))
            )
          )
        )
      )
    )
  )
}

#' optimize Server Functions
#'
#' @noRd
mod_drive_optimize_server <- function(id, aux, global_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      shiny::req(aux$tab_drive_tech_selected)

      shiny::isolate({
        if (aux$tab_drive_tech_selected == "Optimization" & is.null(aux$drive_tech_data)) {
          bslib::nav_select(
            id = "tab_drive_tech",
            selected = "Data Input",
            session = global_session
          )
          shinyalert::shinyalert(
            title = "No data uploaded",
            text = "Upload the data first and then optimize",
            type = "error"
          )
        } else if (aux$tab_drive_tech_selected == "Optimization") {
          if (aux$run_gurobi == 1) {
            session$userData$w$show()

            # log_file <<- tempfile(fileext = ".log")
            # result <- run_gurobi(aux$drive_tech_data, log_file)
            # if (isFALSE(result)) {
            #   result <- list(
            #     ENERGY = NULL,
            #     SOC = NULL,
            #     POWER = NULL,
            #     CHARGERS_ENABLED = NULL,
            #     CHARGERS_ASSIGNED = NULL,
            #     OPTIMAL = NULL,
            #     ANALYTICS = NULL
            #   )
            # } 
            # if (file.exists(log_file)) {
            #   result$log <- paste0(readLines(log_file), collapse = "\n")
            # } else {
            #   result$log <- NULL
            # }
            # saveRDS(result, "result.rds")
            result <- readRDS("result.rds")

            if (!is.null(result$CHARGERS_ASSIGNED)) {
              result$CHARGERS_ASSIGNED <- result$CHARGERS_ASSIGNED |>
                dplyr::mutate(
                  Time = lubridate::hm(Time),
                  Time = lubridate::hour(Time) + (lubridate::minute(Time) / 60)
                )

              aux$drive_tech_data_op <- result

              aux$run_gurobi <- 0
            }

            shiny::showModal(
              shiny::modalDialog(
                title = "Gurobi optimization",
                shiny::pre(style = "height: 75vh;", aux$drive_tech_data_op$log),
                easyClose = TRUE,
                footer = NULL,
                size = "xl"
              )
            )

            if (is.null(result$CHARGERS_ASSIGNED)) {
              shinyalert::shinyalert(type = "error", title = "Something went wrong...")
            }

            session$userData$w$hide()
          } else {
            shiny::showModal(
              shiny::modalDialog(
                title = "Gurobi optimization",
                shiny::pre(style = "height: 75vh;", aux$drive_tech_data_op$log),
                easyClose = TRUE,
                footer = NULL,
                size = "xl"
              )
            )
          }
        }
      })
    })

    shiny::observe({
      shiny::req(aux$drive_tech_data_op)

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "bus_selected",
        choices = colnames(aux$drive_tech_data_op$ENERGY)
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "output_sheet",
        choices = names(aux$drive_tech_data_op)[-length(aux$drive_tech_data_op)]
      )
    })

    shiny::observeEvent(input$results_bttn, {
      golem::invoke_js(
        "bttn_active",
        list(
          active = ns("results_bttn"),
          desactive = c(
            ns("analytics_bttn"),
            ns("export_bttn")
          )
        )
      )
      shinyjs::show("results_layout")
      shinyjs::hide("analytics_layout")
      shinyjs::hide("export_layout")
    })
    output$results_plot1 <- echarts4r::renderEcharts4r({
      shiny::req(aux$drive_tech_data_op)

      # start_time <- as.POSIXct("2024-06-23 00:00:00")
      # end_time <- as.POSIXct("2024-06-23 23:45:00")
      # time_sequence <- seq(from = start_time, to = end_time, by = "15 min")

      aux$drive_tech_data_op$SOC |>
        dplyr::mutate(
          time = seq(0.25, 24, by = .25),
        ) |>
        tidyr::gather(variable, value, -time) |>
        dplyr::mutate(variable = factor(variable, paste0("bus ", 1:ncol(aux$drive_tech_data_op$SOC)))) |>
        dplyr::group_by(variable) |>
        echarts4r::e_chart(time) |>
        echarts4r::e_line(value) |>
        echarts4r::e_theme("dark-bold") |>
        echarts4r::e_x_axis(name = "Time", min = 0, max = 24, interval = 1) |>
        echarts4r::e_y_axis(name = "State of Charge (%)") |>
        echarts4r::e_legend(
          orient = "horizontal",
          bottom = 0,
          textStyle = list(
            fontSize = 12,
            fontWeight = "bold"
          )
        ) |>
        echarts4r::e_grid(bottom = 100) |>
        echarts4r::e_title("STATE OF CHARGE OVER TIME", left = "center")
      # echarts4r::e_theme_custom()
    })
    output$results_plot2 <- echarts4r::renderEcharts4r({
      shiny::req(aux$drive_tech_data_op)

      # start_time <- as.POSIXct("2024-06-23 00:00:00")
      # end_time <- as.POSIXct("2024-06-23 23:45:00")
      # time_sequence <- seq(from = start_time, to = end_time, by = "15 min")

      aux$drive_tech_data_op$POWER |>
        dplyr::mutate(
          time = seq(0.25, 24, by = .25),
        ) |>
        tibble::tibble(
          Energy = aux$drive_tech_data$Dataset[["Energy price"]]
        ) |>
        # tidyr::gather(variable, value, -time) |>
        # dplyr::group_by(variable) |>
        echarts4r::e_chart(time) |>
        echarts4r::e_line(Power) |>
        echarts4r::e_line(Energy, y_index = 1) |>
        echarts4r::e_theme("dark-bold") |>
        echarts4r::e_x_axis(name = "Time", min = 0, max = 24, interval = 1) |>
        echarts4r::e_y_axis(name = "Power (kWh)") |>
        echarts4r::e_y_axis(name = "Energy Price (€/kWh)", index = 1) |>
        echarts4r::e_title("POWER OVER TIME AND ENERGY OVER TIME", left = "center") |>
        echarts4r::e_legend(
          orient = "horizontal",
          bottom = 0,
          textStyle = list(
            fontSize = 12,
            fontWeight = "bold"
          )
        )
    })
    output$results_plot3 <- echarts4r::renderEcharts4r({
      shiny::req(aux$drive_tech_data_op)

      aux$drive_tech_data_op$CHARGERS_ASSIGNED |>
        tidyr::drop_na() |>
        dplyr::group_by(Bus, Charger) |>
        dplyr::summarise(time_start = min(Time), time_end = max(Time)) |>
        dplyr::mutate(Charger = factor(Charger, sort(unique(Charger)))) |>
        tidyr::gather(time, value, -Bus, -Charger) |> 
        dplyr::ungroup() |> 
        dplyr::group_by(Charger, Bus) |> 
        dplyr::arrange(dplyr::desc(Charger)) |> 
        echarts4r::e_chart(Bus) |> 
        echarts4r::e_line(value, lineStyle = list(width = 20)) |> 
        echarts4r::e_theme("dark-bold") |>
        echarts4r::e_flip_coords() |> 
        echarts4r::e_y_axis(name = "Bus ID", min = 0, max = 20, interval = 1) |>
        echarts4r::e_x_axis(name = "Time [Hours]", min = 0, max = 24, interval = 1) |> 
        echarts4r::e_title("Charger assigned per Bus", left = "center") |>
        echarts4r::e_legend(
          orient = "horizontal",
          bottom = 0,
          textStyle = list(
            fontSize = 12,
            fontWeight = "bold"
          )
        )

    })

    output$results_output <- shiny::renderUI({
      shiny::req(aux$drive_tech_data_op)

      bslib::layout_column_wrap(
        width = "800px",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::value_box(
            title = "Optimal value",
            value = euro(aux$drive_tech_data_op$OPTIMAL[[1]]),
            showcase = bsicons::bs_icon("currency-euro"),
            theme = "primary",
            shiny::p("Charging costs")
          ),
          bslib::value_box(
            title = "Power Peak",
            value = "",
            showcase = bsicons::bs_icon("lightning-charge-fill"),
            theme = "primary",
            shiny::p("Not available")
          )
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::value_box(
            title = "Discharging revenues",
            value = "",
            showcase = bsicons::bs_icon("lightning-charge"),
            theme = "primary",
            shiny::p("Not available")
          ),
          bslib::value_box(
            title = "Degradation costs",
            value = "",
            showcase = bsicons::bs_icon("currency-euro"),
            theme = "primary",
            shiny::p("Not available")
          )
        )
      )
    })

    shiny::observeEvent(input$analytics_bttn, {
      golem::invoke_js(
        "bttn_active",
        list(
          active = ns("analytics_bttn"),
          desactive = c(
            ns("results_bttn"),
            ns("export_bttn")
          )
        )
      )
      shinyjs::hide("results_layout")
      shinyjs::show("analytics_layout")
      shinyjs::hide("export_layout")
    })
    output$analytics_energy_output_totals <- shiny::renderUI({
      shiny::req(aux$drive_tech_data_op, input$bus_selected)

      total <- round(aux$drive_tech_data_op$ANALYTICS$Summary[, 1], 2)
      average <- round(aux$drive_tech_data_op$ANALYTICS$Summary[, 3], 2)

      bslib::layout_column_wrap(
        width = 1,
        bslib::value_box(
          theme = "primary",
          title = "Total Energy Consumption",
          value = paste0(total, " kWh")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Average consumption",
          value = paste0(average, " kWh/bus")
        )
      )
    })
    output$analytics_energy_output <- shiny::renderUI({
      shiny::req(aux$drive_tech_data_op, input$bus_selected)

      total_energy <- aux$drive_tech_data_op$ANALYTICS[["Energy Consumption Per Bus"]] |> 
        tibble::as_tibble() |> 
        tibble::rownames_to_column(var = "Bus") |> 
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+"))) |> 
        dplyr::pull(2)

      maximum_soc <- aux$drive_tech_data_op$ANALYTICS[["Max SOC Per Bus"]] |> 
        tibble::as_tibble() |> 
        tibble::rownames_to_column(var = "Bus") |> 
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+"))) |> 
        dplyr::pull(2)

      minimum_soc <- aux$drive_tech_data_op$ANALYTICS[["Min SOC Per Bus"]] |> 
        tibble::as_tibble() |> 
        tibble::rownames_to_column(var = "Bus") |> 
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+"))) |> 
        dplyr::pull(2)
        
      avg_soc <- aux$drive_tech_data_op$ANALYTICS[["Avg SOC Per Bus"]] |> 
        tibble::as_tibble() |> 
        tibble::rownames_to_column(var = "Bus") |> 
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+"))) |> 
        dplyr::pull(2)

      bslib::layout_column_wrap(
        width = 1,
        bslib::value_box(
          theme = "primary",
          title = "Total Energy Consumption",
          value = paste0(round(total_energy, 2), " kWh")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Maximum SOC",
          value = paste0(round(maximum_soc, 2), " kWh")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Mininum Energy",
          value = paste0(round(minimum_soc, 2), " kWh")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Average SOC",
          value = scales::percent(avg_soc / 100)
        )
      )
    })
    output$analytics_charging_output_totals <- shiny::renderUI(({
      shiny::req(aux$drive_tech_data_op, input$bus_selected)
      
      total <- aux$drive_tech_data_op$ANALYTICS$Summary[, 2]
      average <- aux$drive_tech_data_op$ANALYTICS$Summary[, 4]

      bslib::layout_column_wrap(
        width = 1,
        bslib::value_box(
          theme = "primary",
          title = "Total Charging Time",
          value = paste0(round(total, 2), "h")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Average Charging Time",
          value = paste0(round(average, 2), "h")
        )
      )
    }))
    output$analytics_charging_output <- shiny::renderUI({
      shiny::req(aux$drive_tech_data_op, input$bus_selected)

      charging_window <- aux$drive_tech_data_op$ANALYTICS[["Charging Window Per Bus"]] |>
        tibble::rownames_to_column(var = "Bus") |> 
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+")))
      charging_window <- paste0(charging_window$min, " to ", charging_window$max)

      charging_number <- aux$drive_tech_data_op$ANALYTICS[["Charger Number Per Bus"]] |>
        tibble::as_tibble() |> 
        tibble::rownames_to_column(var = "Bus") |> 
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+"))) |> 
        dplyr::pull(2)

      charging_time <- aux$drive_tech_data_op$ANALYTICS[["Charging Time Per Bus"]] |> 
        tibble::as_tibble() |> 
        tibble::rownames_to_column(var = "Bus") |> 
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+"))) |> 
        dplyr::pull(2)

      bslib::layout_column_wrap(
        width = 1,
        bslib::value_box(
          theme = "primary",
          title = "Charging time",
          value = paste0(charging_time, "h")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Charging window",
          value = charging_window
        ),
        bslib::value_box(
          theme = "primary",
          title = "Charger number",
          value = charging_number
        )
      )
    })
    output$fleet_state_output_totals <- echarts4r::renderEcharts4r({
      shiny::req(aux$drive_tech_data_op)

      tibble::tribble(
        ~ value,  ~ pct,
        "Charging", aux$drive_tech_data_op$ANALYTICS$Summary[, 7] / 100,
        "IDLE", aux$drive_tech_data_op$ANALYTICS$Summary[, 5] / 100,
        "Running", aux$drive_tech_data_op$ANALYTICS$Summary[, 6] / 100
      ) |> 
        echarts4r::e_chart(value) |>
        echarts4r::e_pie(pct) |>
        echarts4r::e_theme("dark-bold") |>
        echarts4r::e_labels(
          show = TRUE,
          formatter = "{d}%",
          position = "inside"
        )
    })
    output$fleet_state_output_plot <- echarts4r::renderEcharts4r({
      shiny::req(aux$drive_tech_data_op, input$bus_selected)

      x <- aux$drive_tech_data_op$ANALYTICS[["State Percentage Per Bus (%)"]] |> 
        tibble::rownames_to_column() |> 
        tidyr::gather(variable, value, -rowname) |> 
        dplyr::filter(variable == !!input$bus_selected)
      
      x$value <- x$value / 100

      x |> 
        echarts4r::e_chart(rowname) |>
        echarts4r::e_pie(value) |>
        echarts4r::e_theme("dark-bold") |>
        echarts4r::e_labels(
          show = TRUE,
          formatter = "{d}%",
          position = "inside"
        )
    })

    shiny::observeEvent(input$export_bttn, {
      golem::invoke_js(
        "bttn_active",
        list(
          active = ns("export_bttn"),
          desactive = c(
            ns("results_bttn"),
            ns("analytics_bttn")
          )
        )
      )
      shinyjs::hide("results_layout")
      shinyjs::hide("analytics_layout")
      shinyjs::show("export_layout")
    })
    output$export_table <- shiny::renderTable(
      {
        shiny::req(aux$drive_tech_data_op, input$output_sheet)

        aux$drive_tech_data_op[[input$output_sheet]]
      },
      width = "100%",
      hover = TRUE,
      align = "c"
    )

    output$export_data <- downloadHandler(
      filename = function() {
        "export.xlsx"
      },
      content = function(file) {
        session$userData$w$show()
        OUT <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(OUT, "ENERGY")
        openxlsx::addWorksheet(OUT, "SOC")
        openxlsx::addWorksheet(OUT, "POWER")
        openxlsx::addWorksheet(OUT, "CHARGERS_ENABLED")
        openxlsx::addWorksheet(OUT, "CHARGERS_ASSIGNED")
        openxlsx::addWorksheet(OUT, "OPTIMAL")

        openxlsx::writeData(OUT, "ENERGY", aux$drive_tech_data_op[["ENERGY"]])
        openxlsx::writeData(OUT, "SOC", aux$drive_tech_data_op[["SOC"]])
        openxlsx::writeData(OUT, "POWER", aux$drive_tech_data_op[["POWER"]])
        openxlsx::writeData(OUT, "CHARGERS_ENABLED", aux$drive_tech_data_op[["CHARGERS_ENABLED"]])
        openxlsx::writeData(OUT, "CHARGERS_ASSIGNED", aux$drive_tech_data_op[["CHARGERS_ASSIGNED"]])
        openxlsx::writeData(OUT, "OPTIMAL", aux$drive_tech_data_op[["OPTIMAL"]])

        Sys.sleep(2)

        session$userData$w$hide()

        openxlsx::saveWorkbook(OUT, file)
      }
    )
  })
}
