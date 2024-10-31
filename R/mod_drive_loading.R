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
    shiny::column(
      12, align = "center",
      col_6(
        align = "right",
        shiny::span(
          "Gurobi Optimizator",
           bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "When deactivated it runs the demo version.",
            placement = "bottom"
          )
        ),
        shinyWidgets::prettyToggle(
          inputId = ns("status_gurobi"),
          label_on = "Active",
          label_off = "Deactivated"
        )
      )
    ),
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
    ),
    col_12(
      style = "padding: 31px;",
      align = "center",
      shiny::h3("OR")
    ),
    col_12(
      style = "padding: 31px;",
      align = "center",
      shiny::actionButton(
        inputId = ns("manual_input"),
        label = "Manual Input",
        class = "btn btn-primary"
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

    shiny::observe({
      aux$status_gurobi <- input$status_gurobi
    }) |> 
      shiny::bindEvent(input$status_gurobi)

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
      data$type <- "file"

      Sys.sleep(1.75)

      session$userData$w$hide()

      # shinyjs::runjs('$("#loading-layout").removeClass("centered-container-init");')
      # shinyjs::runjs('$("#loading-layout").addClass("centered-container");')

      aux$drive_tech_data <- data
      aux$run_gurobi <- 1
      shinyalert::shinyalert("File uploaded, move to the next tab > Summary", type = "success")
    })

    # !
    # !   VALIDATION
    # !
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule(
      "energy_consumption",
      shinyvalidate::compose_rules(
        shinyvalidate::sv_required(),
        shinyvalidate::sv_between(0.5, 2)
      )
    )
    iv$add_rule(
      "avg_velocity",
      shinyvalidate::compose_rules(
        shinyvalidate::sv_required(),
        shinyvalidate::sv_between(8, 20)
      )
    )
    iv$add_rule(
      "start_energy",
      shinyvalidate::compose_rules(
        shinyvalidate::sv_required(),
        shinyvalidate::sv_between(10, 30)
      )
    )
    iv$enable()

    # !
    # !   EXTRA MODULES - CRUDS
    # !
    mod_drive_loading_bus_server("bus_table", aux)
    mod_drive_loading_charge_server("charger_table", aux)
    mod_drive_loading_price_server("price_table", aux)

    shiny::observe({
      if (nrow(aux$drive_tech_manual_input_bus) < 2) {
        shinyalert::shinyalert(
          "The number of buses need to be great or equal one",
          type = "error"
        )
      } else if (nrow(aux$drive_tech_manual_input_charger) == 0) {
        shinyalert::shinyalert(
          "Chargers requires minimum one charger",
          type = "error"
        )
      } else {
        final_payload <- list(
          type = "manual",
          energy_consumption = input$energy_consumption,
          avg_velocity = input$avg_velocity,
          start_energy = input$start_energy,
          drive_tech_manual_input_bus = aux$drive_tech_manual_input_bus,
          drive_tech_manual_input_charger = aux$drive_tech_manual_input_charger,
          drive_tech_manual_input_price = aux$drive_tech_manual_input_price
        )

        message(jsonlite::toJSON(final_payload, pretty = TRUE))

        # saveRDS(final_payload, "tmp.rds")

        aux$drive_tech_data <- final_payload
        aux$run_gurobi <- 1

        shinyalert::shinyalert("Manual data inserted, move to the next tab > Summary", type = "success")

        shiny::removeModal()
      }
    }) |>
      shiny::bindEvent(input$submit)

    shiny::observe({
      shiny::showModal(shiny::modalDialog(
        title = "Manual Input",
        size = "xl",
        col_12(
          align = "center",
          bslib::layout_column_wrap(
            width = "200px",
            fixed_width = TRUE,
            class = "justify-content-center",
            shinyWidgets::autonumericInput(
              inputId = ns("energy_consumption"),
              label = "Energy Consumption",
              value = 0.9,
              align = "center",
              decimalPlaces = 2,
              # minimumValue = 0.01,
              currencySymbolPlacement = "s",
              currencySymbol = " kWh/km"
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("avg_velocity"),
              label = "AVG Velocity",
              value = 12,
              align = "center",
              decimalPlaces = 2,
              # minimumValue = 0.01,
              currencySymbol = " km/h",
              currencySymbolPlacement = "s"
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("start_energy"),
              label = "Starting Energy",
              value = 12,
              align = "center",
              decimalPlaces = 2,
              # minimumValue = 1,
              currencySymbol = " %",
              currencySymbolPlacement = "s"
            )
          ),
          mod_drive_loading_bus_ui(ns("bus_table")),
          bslib::layout_column_wrap(
            width = 1 / 2,
            heights_equal = "row",
            mod_drive_loading_charge_ui(ns("charger_table")),
            mod_drive_loading_price_ui(ns("price_table"))
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          shiny::modalButton("Close"),
          shiny::actionButton(
            inputId = ns("submit"),
            label = "Submit",
            class = "btn btn-primary"
          )
        )
      ))
    }) |>
      shiny::bindEvent(input$manual_input)
  })
}

## To be copied in the UI
# mod_drive_loading_ui("loading_1")

## To be copied in the server
# mod_drive_loading_server("loading_1")
