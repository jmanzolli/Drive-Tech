#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "Drive TECH",
      id = "tab",
      fillable = FALSE,
      theme = bslib::bs_theme(bootswatch = "vapor") |> 
      bslib::bs_add_variables(
        "bs-btn-bg" = "#44d9e8"
      ),
      bslib::nav_panel(
        title = "Input",
        mod_loading_ui("loading"),
        shinyjs::hidden(
          shiny::div(
            id = "describe_div",
            mod_describe_ui("describe")
         )
        )
      ),
      bslib::nav_panel(
        title = "Optimization",
        mod_optimize_ui("optimize")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "driveTech"
    ),
    waiter::useWaiter(),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
