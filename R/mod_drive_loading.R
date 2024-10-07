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
      style = "padding-top: 31px",
      # class = "centered-container-init",
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
    ),
    col_12(
      style = "padding: 31px;",
      align = "center",
      shiny::h3("OR")
    ),
    bslib::layout_columns(
      width = 1/2,
      # TODO use shinymatrix to input the data here
      shiny::div(),
      bslib::layout_column_wrap(
        width = 1/3,
        shinyWidgets::autonumericInput(
          inputId = "timestamp",
          label = "Timestamp",
          value = 4,
          align = "center",
          decimalPlaces = 0,
          minimumValue = 1
        ),
        shinyWidgets::autonumericInput(
          inputId = "power",
          label = "Power",
          value = 100,
          align = "center",
          decimalPlaces = 2,
          minimumValue = 1
        ),
        shinyWidgets::autonumericInput(
          inputId = "energy_consumption",
          label = "Energy Consumption",
          value = 0.9,
          align = "center",
          decimalPlaces = 2,
          minimumValue = 0.01
        ),
        shinyWidgets::autonumericInput(
          inputId = "avg_velocity",
          label = "AVG Velocity",
          value = 12,
          align = "center",
          decimalPlaces = 2,
          minimumValue = 0.01
        ),
        shinyWidgets::autonumericInput(
          inputId = "avg_energy_price",
          label = "AVG Energy Price",
          value = 0.9034,
          align = "center",
          decimalPlaces = 4,
          minimumValue = 0.0001
        ),
        shinyWidgets::autonumericInput(
          inputId = "bus_energy",
          label = "Bus Energy",
          value = 200,
          align = "center",
          decimalPlaces = 0,
          minimumValue = 1
        ),
        shinyWidgets::autonumericInput(
          inputId = "n_charger",
          label = "Qt Chargers",
          value = 10,
          align = "center",
          decimalPlaces = 0,
          minimumValue = 1
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

      # shinyjs::runjs('$("#loading-layout").removeClass("centered-container-init");')
      # shinyjs::runjs('$("#loading-layout").addClass("centered-container");')
      
      aux$drive_tech_data <- data
      aux$run_gurobi <- 1
    })
  })
}

## To be copied in the UI
# mod_drive_loading_ui("loading_1")

## To be copied in the server
# mod_drive_loading_server("loading_1")
