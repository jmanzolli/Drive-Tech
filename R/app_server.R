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
    navbar_selected = NULL,
    input_data = NULL,
    output_data = NULL,
    map_tmp = sf::st_read("inst/map_gis/trocos.shx") |>  sf::st_transform(crs = 4326),
  )

  shiny::observe({
    aux$navbar_selected <- input$tab
  })

  mod_loading_server("loading", aux)
  mod_describe_server("describe", aux)
  mod_optimize_server("optimize", aux, session)

  shiny::observe({
    shiny::req(aux$input_data)

    shinyjs::show("describe_div")
  })

 
}
