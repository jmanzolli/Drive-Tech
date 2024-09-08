#' loading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_drive_loading_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      id = ns("layout"),
      class = "centered-container-init",
      shiny::column(
        12,
        align = "center",
        shiny::fileInput(
          inputId = ns("file"),
          label = "Data for optimization (.xlsx file)",
          multiple = FALSE,
          accept = ".xlsx",
          width = "400px"
        )
      )
    )
  )
}

#' loading Server Functions
#'
#' @noRd
mod_drive_loading_server <- function(id, aux) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$file, {
      file <- input$file

      shiny::req(file)

      session$userData$w$show()

      # file$datapath <- pkg_resource("data/v2/input.xlsx")
      ext <- tools::file_ext(file$datapath)

      shiny::validate(shiny::need(ext == "xlsx", "Please upload a xlsx file"))

      # data <- readxl::read_xlsx(file$datapath)
      sheets <- readxl::excel_sheets(file$datapath)

      data <- purrr::map(sheets, function(.x) {
        readxl::read_xlsx(file$datapath, .x)
      })

      names(data) <- sheets

      Sys.sleep(1.75)

      session$userData$w$hide()

      shinyjs::runjs('$("#loading-layout").removeClass("centered-container-init");')
      shinyjs::runjs('$("#loading-layout").addClass("centered-container");')
      
      aux$drive_tech_data <- data
      aux$run_gurobi <- 1
    })
  })
}

## To be copied in the UI
# mod_drive_loading_ui("loading_1")

## To be copied in the server
# mod_drive_loading_server("loading_1")
