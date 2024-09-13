#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    
    golem_add_external_resources(),

    # bslib::nav_panel(
    #   title = "Welcome",
    #   mod_welcome_ui("welcome_page")
    # ),

    bslib::page_navbar(
      title = "",
      id = "tab",
      fillable = FALSE,
      theme = bslib::bs_theme(
        bg = "#2E2E2E",
        fg = "white",
        primary = "#1e90ff",
        "bs-body-color" = "#FF5733"
      ),
      bslib::nav_panel(
        title = "GEOH",
        bslib::navset_underline(
          id = "tab_geoh",
          bslib::nav_panel(
            title = "Metrics",
            mod_geoh_metrics_ui("metrics")
          ),
          bslib::nav_panel(
            title = "Predictions (FCR)",
            mod_geoh_predicted_ui("fcr", available_models_fcr)
          ),
          bslib::nav_panel(
            title = "Predictions (CO2)",
            mod_geoh_predicted_ui("co2", available_models_co2)
          )
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
