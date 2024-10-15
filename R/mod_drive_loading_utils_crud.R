# Create custom theme with background color #2E2E2E
custom_dark_theme <- reactable::reactableTheme(
    backgroundColor = "#2E2E2E", # Dark background
    color = "#FFFFFF", # White text for visibility
    borderColor = "#444444", # Dark border color
    stripedColor = "#3C3C3C", # Slightly lighter shade for striped rows
    highlightColor = "#444444", # Highlight color for row hover
    inputStyle = list(
        backgroundColor = "#444444", # Background color for filters and inputs
        color = "#FFFFFF" # Text color for inputs
    ),
    style = list(
        fontFamily = "Arial, sans-serif" # General font style
    ),
    headerStyle = list(
        backgroundColor = "#3C3C3C", # Darker background for the header
        color = "#FFFFFF", # White text for header
        fontWeight = "bold"
    )
)
# !
# !   BUS INPUT
# !
#' @export
mod_drive_loading_bus_ui <- function(id) {
    ns <- shiny::NS(id)

    col_12(
        align = "center",
        class = "border-style",
        bslib::layout_column_wrap(
            width = 1 / 2,
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

# !
# !   CHARGE INPUT
# !
mod_drive_loading_charge_ui <- function(id) {
    ns <- shiny::NS(id)
    col_12(
        align = "center",
        class = "border-style",
        bslib::layout_column_wrap(
            width = 1 / 2,
            shiny::textInput(
                inputId = ns("charger_manufacturer"),
                label = "Charger Manufacturer",
                value = ""
            ),
            shinyWidgets::autonumericInput(
                inputId = ns("charger_power"),
                label = "Charging Power (kW)",
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

mod_drive_loading_charge_server <- function(id, aux) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observe({
            data <- aux$drive_tech_manual_input_charger

            new_data <- tibble::tibble(
                charger_manufacturer = input$charger_manufacturer,
                charger_power = input$charger_power,
            )

            data <- dplyr::bind_rows(data, new_data)

            aux$drive_tech_manual_input_charger <- data

            shiny::updateTextInput(session, "charger_manufacturer", value = "")
            shinyWidgets::updateAutonumericInput(session, "charger_power", value = 0)
        }) |>
            shiny::bindEvent(input$add, ignoreInit = TRUE)


        output$table <- reactable::renderReactable({
            shiny::req(aux$drive_tech_manual_input_charger)

            aux$drive_tech_manual_input_charger |>
                reactable::reactable(theme = custom_dark_theme)
        })
    })
}

# !
# !   SCHEDULE INPUT
# !
mod_drive_loading_route_ui <- function(id) {
    ns <- shiny::NS(id)
    col_12(
        align = "center",
        class = "border-style",
        bslib::layout_column_wrap(
            width = 1 / 3,
            shiny::textInput(
                inputId = ns("route_id"),
                label = "Route ID",
                value = ""
            ),
            shinyWidgets::timeInput(
                inputId = ns("route_start"),
                label = "Start HH:MM"
            ),
            shinyWidgets::timeInput(
                inputId = ns("route_end"),
                label = "End HH:MM"
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

mod_drive_loading_route_server <- function(id, aux) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observe({
            data <- aux$drive_tech_manual_input_route

            print(input$route_start)

            new_data <- tibble::tibble(
                route_id = input$route_id,
                route_start = input$route_start,
                route_end = input$route_end
            )

            data <- dplyr::bind_rows(data, new_data)

            aux$drive_tech_manual_input_route <- data

            shiny::updateTextInput(session, "route_id", value = "")
        }) |>
            shiny::bindEvent(input$add, ignoreInit = TRUE)


        output$table <- reactable::renderReactable({
            shiny::req(aux$drive_tech_manual_input_route)

            aux$drive_tech_manual_input_route |>
                reactable::reactable(theme = custom_dark_theme)
        })
    })
}

#!
#!   price
#!
mod_drive_loading_price_ui <- function(id) {
    ns <- shiny::NS(id)
    col_12(
        align = "center",
        class = "border-style",
        reactable::reactableOutput(ns("table"))
    )
}

mod_drive_loading_price_server <- function(id, aux) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observe({
            data <- aux$drive_tech_manual_input_price

            print(input$route_start)

            new_data <- tibble::tibble(
                route_id = input$route_id,
                route_start = input$route_start,
                route_end = input$route_end
            )

            data <- dplyr::bind_rows(data, new_data)

            aux$drive_tech_manual_input_price <- data

            shiny::updateTextInput(session, "route_id", value = "")
        }) |>
            shiny::bindEvent(input$add, ignoreInit = TRUE)

        output$table <- reactable::renderReactable({
            shiny::req(aux$drive_tech_manual_input_price)
            
            tb <- aux$drive_tech_manual_input_price |> 
                dplyr::mutate(
                    price2=price
                )
            
            tb |> 
                reactable::reactable(
                    sortable = FALSE,
                    defaultPageSize = 24,
                    theme = custom_dark_theme,
                    columns = list(
                        price = reactable::colDef(
                            format = reactable::colFormat(
                                digits = 6
                            ),
                            cell = reactable.extras::text_extra(
                               ns("price")
                            )
                        ),
                        price2 = reactable::colDef(
                            name = "",
                            cell = reactablefmtr::data_bars(
                                tb, 
                                fill_color = c("#22577A","#38A3A5","#57CC99","#80ED99","#C7F9CC"),
                                fill_opacity = 0.8,
                                text_position = "none"
                            )
                        )
                    )
                ) 
        })


        price_d <- shiny::reactive({
            input$price
        }) |> 
            shiny::debounce(2000)

        shiny::observe({
            aux$drive_tech_manual_input_price[price_d()$row, "price"] <- as.numeric(price_d()$value)
        }) |> 
            shiny::bindEvent(price_d(), ignoreInit = TRUE)
    })
}