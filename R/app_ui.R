#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    
    golem_add_external_resources(),

    bslib::page_navbar(
      title = "",
      id = "tab",
      fillable = FALSE,
      bslib::nav_menu(
        title = "GEOH",
        bslib::nav_panel(
          title = "Metrics"
        ),
        bslib::nav_panel(
          title = "Predictions (FCR)"
        ),
        bslib::nav_panel(
          title = "Predictions (CO2)"
        )
      ),
      bslib::nav_panel(
        title = "DRIVE TECH",
        bslib::navset_underline(
          id = "tab_drive_tech",
          bslib::nav_panel(
            title = "Summary",
            mod_drive_loading_ui("loading"),
            shinyjs::hidden(
              shiny::div(
                id = "describe_div",
                mod_drive_summary_ui("describe")
              )
            )
          ),
          bslib::nav_panel(
            title = "Optimization",
            mod_drive_optimize_ui("optimize")
          )
        )
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
