#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  session$userData$w <- waiter::Waiter$new(
    html = waiter::spin_loaders(id = 1, color = "darkblue", style = "height: 4em; width: 4em;"),
    color = waiter::transparent(.5)
  )

  aux <- shiny::reactiveValues(
    drive_tech_data = NULL,
    drive_tech_data_op = NULL,
    map_tmp = sf::st_read("inst/map_gis/trocos.shx") |> sf::st_transform(crs = 4326),
    drive_tech_manual_input_bus = NULL,
    drive_tech_manual_input_charger = NULL,
    drive_tech_manual_input_route = NULL
  )

  # !
  # !   DRIVE TECH
  # !
  shiny::observe({
    if (input$tab_drive_tech == "Summary" & is.null(aux$drive_tech_data)) {
        bslib::nav_select(
          id = "tab_drive_tech",
          selected = "Data Input",
          session = session
        )
        shinyalert::shinyalert(
          title = "No data uploaded",
          text = "Upload the data first",
          type = "error"
        )
    } else if (input$tab_drive_tech == "Optimization" & is.null(aux$drive_tech_data)) {
      bslib::nav_select(
        id = "tab_drive_tech",
        selected = "Data Input",
        session = session
      )
      shinyalert::shinyalert(
        title = "No data uploaded",
        text = "Upload the data first and then optimize",
        type = "error"
      )
    } else if (input$tab_drive_tech == "Optimization") {
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
  }) |> 
    shiny::bindEvent(input$tab_drive_tech, ignoreInit = TRUE)

  mod_drive_loading_server("loading", aux)
  mod_drive_summary_server("describe", aux)
  mod_drive_optimize_server("optimize", aux)

  shiny::observe({
    shiny::req(aux$drive_tech_data)

    shinyjs::show("describe_div")
  })

  # !
  # !   GEOH
  # !
  mod_geoh_metrics_server("metrics")
  mod_geoh_predicted_server("fcr", "fc")
  mod_geoh_predicted_server("co2", "co2")
}
