#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  onStart <- function() {
    reticulate::use_virtualenv('drive_tech', required = TRUE)

    reticulate::source_python("./inst/python/run_gurobi.py", envir = .GlobalEnv)
    reticulate::source_python("./inst/python/models.py", envir = .GlobalEnv)

    available_models_fcr <<- available_models_list("models/fcr")
    available_models_co2 <<- available_models_list("models/co2")
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}