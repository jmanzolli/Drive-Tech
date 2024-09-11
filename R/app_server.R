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
    tab_drive_tech_selected = NULL,
    drive_tech_data = NULL,
    drive_tech_data_op = NULL,
    map_tmp = sf::st_read("inst/map_gis/trocos.shx") |>  sf::st_transform(crs = 4326),
  )

  #!
  #!   DRIVE TECH
  #!
  shiny::observe({
    aux$tab_drive_tech_selected <- input$tab_drive_tech
  })

  mod_drive_loading_server("loading", aux)
  mod_drive_summary_server("describe", aux)
  mod_drive_optimize_server("optimize", aux, session)

  shiny::observe({
    shiny::req(aux$drive_tech_data)

    shinyjs::show("describe_div")
  })

  #!
  #!   GEOH
  #!
  mod_geoh_metrics_server("metrics")
  mod_geoh_predicted_server("fcr", "fc")
  mod_geoh_predicted_server("co2", "co2")
}
