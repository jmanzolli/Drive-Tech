#' optimize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom ggplot2
mod_optimize_ui <- function(id) {
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
          label = shiny::div(class = "button-content", shiny::icon("chart-line"), "Results")
        ),
        shiny::actionButton(
          inputId = ns("analytics_bttn"),
          label = shiny::div(class = "button-content", shiny::icon("file-lines"), "Analytics")
        ),
        shiny::actionButton(
          inputId = ns("export_bttn"),
          label = shiny::div(class = "button-content", shiny::icon("download"), "Export")
        )
      ),
      bslib::page_fluid(
        shinyjs::hidden(
          shiny::div(
            id = ns("results_layout"),
            bslib::card(
              bslib::card_header(
                class = "d-flex justify-content-center",
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
                class = "d-flex justify-content-center highlight-card-header",
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
                class = "d-flex justify-content-center highlight-card-header",
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
mod_optimize_server <- function(id, aux, global_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      shiny::req(aux$navbar_selected)

      shiny::isolate({
        if (aux$navbar_selected == "Optimization" & is.null(aux$input_data)) {
          bslib::nav_select(
            id = "tab",
            selected = "Input",
            session = global_session
          )
          shinyalert::shinyalert(
            title = "No data uploaded",
            text = "Upload the data first and then optimize",
            type = "error"
          )
        } else if (aux$navbar_selected == "Optimization") {
          if (aux$run_gurobi == 1) {
            session$userData$w$show()

            # log_file <<- tempfile(fileext = ".log")
            # result <- run_gurobi(aux$input_data, log_file)
            # if (isFALSE(result)) {
            #   result <- list(
            #     Energy = NULL,
            #     SOC = NULL,
            #     POWER = NULL,
            #     CHARGERS_ENABLED = NULL,
            #     CHARGERS_ASSIGNED = NULL,
            #     OPTIMAL = NULL
            #   )
            # } else {
            #   result <- purrr::map(result, tibble::as_tibble)
            #   names(result) <- c("ENERGY", "SOC", "POWER", "CHARGERS_ENABLED", "CHARGERS_ASSIGNED", "OPTIMAL")
            # }
            # if (file.exists(log_file)) {
            #   result$log <- paste0(readLines(log_file), collapse = "\n")
            # } else {
            #   result$log <- NULL
            # }

            result <- readRDS("result.rds")

            if (!is.null(result$CHARGERS_ASSIGNED)) {
              result$CHARGERS_ASSIGNED <- result$CHARGERS_ASSIGNED |>
                dplyr::mutate(
                  Time = lubridate::hm(Time),
                  Time = lubridate::hour(Time) + (lubridate::minute(Time) / 60)
                )

              aux$output_data <- result

              aux$run_gurobi <- 0
            }

            shiny::showModal(
              shiny::modalDialog(
                title = "Gurobi optimization",
                shiny::pre(style = "height: 75vh;", aux$output_data$log),
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
                shiny::pre(style = "height: 75vh;", aux$output_data$log),
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
      shiny::req(aux$output_data)

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "bus_selected",
        choices = colnames(aux$output_data$ENERGY)
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "output_sheet",
        choices = names(aux$output_data)[-length(aux$output_data)]
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
      shiny::req(aux$output_data)

      # start_time <- as.POSIXct("2024-06-23 00:00:00")
      # end_time <- as.POSIXct("2024-06-23 23:45:00")
      # time_sequence <- seq(from = start_time, to = end_time, by = "15 min")

      aux$output_data$SOC |>
        dplyr::mutate(
          time = seq(0.25, 24, by = .25),
        ) |>
        tidyr::gather(variable, value, -time) |>
        dplyr::mutate(variable = factor(variable, paste0("bus ", 1:ncol(aux$output_data$SOC)))) |>
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
      shiny::req(aux$output_data)

      # start_time <- as.POSIXct("2024-06-23 00:00:00")
      # end_time <- as.POSIXct("2024-06-23 23:45:00")
      # time_sequence <- seq(from = start_time, to = end_time, by = "15 min")

      aux$output_data$POWER |>
        dplyr::mutate(
          time = seq(0.25, 24, by = .25),
        ) |>
        tibble::tibble(
          Energy = aux$input_data$Dataset[["Energy price"]]
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
      shiny::req(aux$output_data)

      aux$output_data$CHARGERS_ASSIGNED |>
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
      shiny::req(aux$output_data)

      bslib::layout_column_wrap(
        width = 4,
        height = "115px",
        bslib::value_box(
          title = "Optimal value",
          value = euro(aux$output_data$OPTIMAL[[1]]),
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
        ),
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
      shiny::req(aux$output_data, input$bus_selected)

      total <- round(sum(aux$output_data$ENERGY), 2)
      average <- round(mean(unlist(aux$output_data$ENERGY)), 2)

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
      shiny::req(aux$output_data, input$bus_selected)

      bslib::layout_column_wrap(
        width = 1,
        bslib::value_box(
          theme = "primary",
          title = "Total Energy Consumption",
          value = paste0(round(sum(aux$output_data$ENERGY[, input$bus_selected]), 2), " kWh")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Maximum Energy",
          value = paste0(round(max(aux$output_data$ENERGY[, input$bus_selected]), 2), " kWh")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Mininum Energy",
          value = paste0(round(min(aux$output_data$ENERGY[, input$bus_selected]), 2), " kWh")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Average SOC",
          value = scales::percent(mean(aux$output_data$SOC[, input$bus_selected][[1]] / 100, na.rm = TRUE))
        )
      )
    })
    output$analytics_charging_output_totals <- shiny::renderUI(({
      shiny::req(aux$output_data, input$bus_selected)
      total <- aux$output_data$CHARGERS_ASSIGNED |>
        dplyr::count(Bus) |>
        dplyr::pull(n) |>
        sum()

      average <- aux$output_data$CHARGERS_ASSIGNED |>
        dplyr::count(Bus) |>
        dplyr::pull(n) |>
        mean()

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
      shiny::req(aux$output_data, input$bus_selected)

      charger_info <- aux$output_data$CHARGERS_ASSIGNED |>
        dplyr::filter(Bus == as.integer(stringr::str_extract(input$bus_selected, "[0-9]+"))) |>
        dplyr::summarise(
          charging_time = dplyr::n(),
          charging_window = paste0(decimal_to_hm(min(Time)), " to ", decimal_to_hm(max(Time))),
          charging_number = paste0("Charger ", unique(Charger), collapse = ",")
        )

      bslib::layout_column_wrap(
        width = 1,
        bslib::value_box(
          theme = "primary",
          title = "Charging time",
          value = paste0(charger_info$charging_time, "h")
        ),
        bslib::value_box(
          theme = "primary",
          title = "Charging window",
          value = charger_info$charging_window
        ),
        bslib::value_box(
          theme = "primary",
          title = "Charger number",
          value = charger_info$charging_number
        )
      )
    })
    output$fleet_state_output_totals <- echarts4r::renderEcharts4r({
      shiny::req(aux$output_data)

      tb <- apply(aux$output_data$ENERGY, 2, function(x) {
        x <- x - dplyr::lag(x)
        x[1] <- 0
        dplyr::case_when(
          x == 0 ~ "IDLE",
          x > 0 ~ "Charging",
          x < 0 ~ "Running"
        )
      }) |>
        tibble::as_tibble()

      tb |>
        tidyr::gather(variable, value) |>
        dplyr::count(value) |>
        dplyr::mutate(pct = n / sum(n)) |>
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
      shiny::req(aux$output_data, input$bus_selected)

      x <- aux$output_data$ENERGY[, input$bus_selected]
      x <- x[[1]]
      x <- x - dplyr::lag(x)
      x[1] <- 0
      x <- dplyr::case_when(
        x == 0 ~ "IDLE",
        x > 0 ~ "Charging",
        x < 0 ~ "Running"
      )

      tibble::tibble(x = x) |>
        dplyr::count(x) |>
        dplyr::mutate(pct = n / sum(n)) |>
        echarts4r::e_chart(x) |>
        echarts4r::e_pie(pct) |>
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
        shiny::req(aux$output_data, input$output_sheet)

        aux$output_data[[input$output_sheet]]
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

        openxlsx::writeData(OUT, "ENERGY", aux$output_data[["ENERGY"]])
        openxlsx::writeData(OUT, "SOC", aux$output_data[["SOC"]])
        openxlsx::writeData(OUT, "POWER", aux$output_data[["POWER"]])
        openxlsx::writeData(OUT, "CHARGERS_ENABLED", aux$output_data[["CHARGERS_ENABLED"]])
        openxlsx::writeData(OUT, "CHARGERS_ASSIGNED", aux$output_data[["CHARGERS_ASSIGNED"]])
        openxlsx::writeData(OUT, "OPTIMAL", aux$output_data[["OPTIMAL"]])

        Sys.sleep(2)

        session$userData$w$hide()

        openxlsx::saveWorkbook(OUT, file)
      }
    )
  })
}
