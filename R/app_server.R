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
    status_gurobi = 0,
    run_gurobi = 0,
    drive_tech_data = NULL,
    drive_tech_data_op = NULL,
    map_tmp = suppressMessages({sf::st_read("inst/map_gis/trocos.shx", quiet = TRUE) |> sf::st_transform(crs = 4326)}),
    drive_tech_manual_input_bus = tibble::tibble(),
    drive_tech_manual_input_charger = tibble::tibble(),
    drive_tech_manual_input_price = tibble::tibble(
      hour = 1:24,
      price = c(0.09619638,0.08876599,0.08449662,0.08228022,0.08080804,0.08383324,0.09252627,0.1027913,0.1057172,0.09572981,0.08299575,0.07722831,0.07437687,0.07192796,0.06861104,0.06739891,0.07101793,0.08115063,0.0941812,0.1104536,0.1221266,0.1205818,0.1114516,0.1015224)
    )
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
    } else if (input$tab_drive_tech == "Optimizer" & is.null(aux$drive_tech_data)) {
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
    } else if (input$tab_drive_tech == "Optimizer") {
      if (aux$run_gurobi == 1) {
        session$userData$w$show()

        manual <- aux$drive_tech_data$type
        manual <- manual == "manual"

        if (aux$status_gurobi == 1) {
          log_file <<- tempfile(fileext = ".log")
          result <- run_gurobi(aux$drive_tech_data, log_file, manual)
          if (isFALSE(result)) {
            result <- list(
              ENERGY = NULL,
              SOC = NULL,
              POWER = NULL,
              CHARGERS_ENABLED = NULL,
              CHARGERS_ASSIGNED = NULL,
              OPTIMAL = NULL,
              ANALYTICS = NULL
            )
          }
          if (file.exists(log_file)) {
            result$log <- paste0(readLines(log_file), collapse = "\n")
          } else {
            result$log <- NULL
          }
          # saveRDS(result, "inst/data/drive_tech/v2/demo.rds")
        } else {
          result <- readRDS("inst/data/drive_tech/v2/demo.rds")
        }

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
            title = "Gurobi optimizer",
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
            title = "Gurobi optimizer",
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
