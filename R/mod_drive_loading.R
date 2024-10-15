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
    col_12(
      align = "center",
      bslib::layout_column_wrap(
        width = "200px",
        fixed_width = TRUE,
        class = "justify-content-center",
        shinyWidgets::autonumericInput(
          inputId = "energy_consumption",
          label = "Energy Consumption",
          value = 0.9,
          align = "center",
          decimalPlaces = 2,
          minimumValue = 0.01,
          currencySymbolPlacement = "s",
          currencySymbol = " kWh/km"
        ),
        shinyWidgets::autonumericInput(
          inputId = "avg_velocity",
          label = "AVG Velocity",
          value = 12,
          align = "center",
          decimalPlaces = 2,
          minimumValue = 0.01,
          currencySymbol = " km/h",
          currencySymbolPlacement = "s"
        ),
        shinyWidgets::autonumericInput(
          inputId = "start_energy",
          label = "Starting Energy",
          value = 12,
          align = "center",
          decimalPlaces = 2,
          minimumValue = 1,
          currencySymbol = " %",
          currencySymbolPlacement = "s"
        )
      ),
      bslib::layout_columns(
        col_widths = c(3,3,4,2),
        mod_drive_loading_bus_ui(ns("bus_table")),
        mod_drive_loading_charge_ui(ns("charger_table")),
        mod_drive_loading_route_ui(ns("route_table")),
        mod_drive_loading_price_ui(ns("price_table"))
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

    mod_drive_loading_bus_server("bus_table", aux)
    mod_drive_loading_charge_server("charger_table", aux)
    mod_drive_loading_route_server("route_table", aux)
    mod_drive_loading_price_server("price_table", aux)

    shiny::observe({
      timestamp <- 4
      avg_velocity <- 12

    }) |> 
      shiny::bindEvent(input$submit)
  })
}

## To be copied in the UI
# mod_drive_loading_ui("loading_1")

## To be copied in the server
# mod_drive_loading_server("loading_1")
