# Create custom theme with background color #2E2E2E
custom_dark_theme <- reactable::reactableTheme(
  backgroundColor = "#2E2E2E", # Dark background
  color = "#FFFFFF",           # White text for visibility
  borderColor = "#444444",      # Dark border color
  stripedColor = "#3C3C3C",     # Slightly lighter shade for striped rows
  highlightColor = "#444444",   # Highlight color for row hover
  inputStyle = list(
    backgroundColor = "#444444", # Background color for filters and inputs
    color = "#FFFFFF"            # Text color for inputs
  ),
  style = list(
    fontFamily = "Arial, sans-serif" # General font style
  ),
  headerStyle = list(
    backgroundColor = "#3C3C3C", # Darker background for the header
    color = "#FFFFFF",           # White text for header
    fontWeight = "bold"
  )
)


#!
#!   BUS INPUT
#!

#' @export
mod_drive_loading_bus_ui <- function(id) {
  ns <- shiny::NS(id)

  col_12(
    align = "center",
    bslib::layout_column_wrap(
      width = 1/2,
      shiny::textInput(
        inputId = ns("bus_manufacturer"),
        label = "Bus Manufacturer",
        value = ""
      ),
      shinyWidgets::autonumericInput(
          inputId = ns("bus_battery_capacity"),
          label = "Battery Capacity",
          value = 0,
          align = "center",
          decimalPlaces = 2,
          currencySymbolPlacement = "s",
          currencySymbol = " kWh"
      )
    ),
    shiny::actionButton(
      inputId = ns("add"),
      label = "Add"
    ),
    shiny::br(),
    shiny::br(),
    shiny::br(),
    reactable::reactableOutput(ns("table"))
  )
}

#' @export
mod_drive_loading_bus_server <- function(id, aux) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      data <- aux$drive_tech_manual_input_bus

      new_data <- tibble::tibble(
        bus_manufacturer = input$bus_manufacturer,
        bus_battery_capacity = input$bus_battery_capacity,
      )

      data <- dplyr::bind_rows(data, new_data)

      aux$drive_tech_manual_input_bus <- data

      shiny::updateTextInput(session, "bus_manufacturer", value = "")
      shinyWidgets::updateAutonumericInput(session, "bus_battery_capacity", value = 0)
    }) |> 
      shiny::bindEvent(input$add, ignoreInit = TRUE)


    output$table <- reactable::renderReactable({
      shiny::req(aux$drive_tech_manual_input_bus)

      aux$drive_tech_manual_input_bus |> 
        reactable::reactable(theme = custom_dark_theme)
    })
  })
}

#!
#!   CHARGE INPUT
#!
mod_drive_loading_charge_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
  )
}

mod_drive_loading_charge_server <- function(id, aux) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

#!
#!   SCHEDULE INPUT
#!
mod_drive_loading_route_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
  )
}

mod_drive_loading_route_server <- function(id, aux) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
