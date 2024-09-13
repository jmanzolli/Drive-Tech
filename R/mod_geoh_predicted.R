#' predicted_emissions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geoh_predicted_ui <- function(id, available_models) {
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          open = "open",
          width = 350,
          shiny::fileInput(
            inputId = ns("file"),
            label = "Upload trip data",
            multiple = TRUE,
            accept = ".csv",
            buttonLabel = "Browse",
            placeholder = "No file selected"
          ),
          shinyWidgets::pickerInput(
            inputId = ns("base_model"),
            label = "Base model",
            choices = available_models,
            options = list(
              `live-search` = TRUE,
              container = "body",
              size = 5
            )
          ),
          shinyWidgets::pickerInput(
            inputId = ns("comparison1"),
            label = "Alternative 1",
            choices = available_models,
            options = list(
              `live-search` = TRUE,
              container = "body",
              size = 5
            )
          ),
          shinyWidgets::pickerInput(
            inputId = ns("comparison2"),
            label = "Alternative 2",
            choices = available_models,
            options = list(
              `live-search` = TRUE,
              container = "body",
              size = 5
            )
          ),
          actionButton(
            inputId = ns("run"),
            label = "Prediction"
          )
        ),
        shiny::uiOutput(ns("indicators")),
        echarts4r::echarts4rOutput(ns("accumulated"), height = "500px")
        # reactable::reactableOutput(ns("table"))
      )
    )
  )
}

#' predicted_emissions Server Functions
#'
#' @noRd
mod_geoh_predicted_server <- function(id, model) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_emissions <- reactive({
      req(input$file)

      # NOTE assuming that the headers will be always like that .. variable [unit]
      datapath <- input$file
      datapath$id <- 1:nrow(datapath)

      data <- readr::read_csv(datapath$datapath)
      # data_emissions <- readr::read_csv("./data/test_data/020_Honda_Civic_2014_(1.8L_Auto).csv")

      return(data)
    })

    results <- eventReactive(input$run, {
      req(input$base_model, input$comparison1, input$comparison2, data_emissions())

      data_emissions <- data_emissions()

      saveRDS(
        list(
          datapath = input$base_model, 
          data_test = data_emissions
        ), "tmp.rds"
      )

      if (model == "fc") {
        r0 <- tibble::as_tibble(predict_fcr_model(input$base_model, data_emissions))
        r1 <- tibble::as_tibble(predict_fcr_model(input$comparison1, data_emissions))
        r2 <- tibble::as_tibble(predict_fcr_model(input$comparison2, data_emissions))

        data_emissions |>
          dplyr::bind_cols(
            tibble::tibble(
              baseline = r0$pred_fcr,
              alternative1 = r1$pred_fcr,
              alternative2 = r2$pred_fcr
            )
          )
      } else if (model == "co2") {
          r0 <- tibble::as_tibble(predict_co2_model(input$base_model, data_emissions))
          r1 <- tibble::as_tibble(predict_co2_model(input$comparison1, data_emissions))
          r2 <- tibble::as_tibble(predict_co2_model(input$comparison2, data_emissions))

          data_emissions |>
            dplyr::bind_cols(
              tibble::tibble(
                baseline = r0$pred_co2,
                alternative1 = r1$pred_co2,
                alternative2 = r2$pred_co2
              )
            )
      }

    })

    output$indicators <- shiny::renderUI({
      req(results())

      model_name <- toupper(model) # FC - CO2
      model_rate_name <- paste0(model_name, "R") # FCR - CO2R

      vbs <- list(
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "AVG. FCR (g/s)"),
          value = num(mean(results()$fcr), digits = 2),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "Total FC (g)"),
          value = num(sum(results()$fcr), digits = 2),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "Total CO2 (kg)"),
          value = num(sum(results()$co2), digits = 2),
          showcase = bsicons::bs_icon("speedometer2"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "AVG. CO2 (g/s)"),
          value = num(mean(results()$co2), digits = 2),
          showcase = bsicons::bs_icon("speedometer2"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", "AVG. Speed (km/h)"),
          value = num(mean(results()$gps_speed), digits = 2),
          showcase = bsicons::bs_icon("speedometer2"),
          class = "bg-primary"
        ),


        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", sprintf("AVG. %s (g/s)", model_rate_name)),
          value = num(mean(results()$baseline), digits = 2),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", sprintf("AVG. %s (g/s)", model_rate_name)),
          value = num(mean(results()$alternative1), digits = 2),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", sprintf("AVG. %s (g/s)", model_rate_name)),
          value = num(mean(results()$alternative2), digits = 2),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        ),

        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", sprintf("Total %s (g)", model_name)),
          value = num(sum(results()$baseline), digits = 2),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", sprintf("Total %s (g) (change %%)", model_name)),
          value = paste0(
            num(sum(results()$alternative1), digits = 2),
            " (", scales::percent(sum(results()$alternative1)/sum(results()$baseline) - 1), ")"
          ),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        ),
        bslib::value_box(
          title = shiny::span(style = "font-weight: bold; font-size: 25px", sprintf("Total %s (g) (change %%)", model_name)),
          value = paste0(
            num(sum(results()$alternative2), digits = 2),
            " (", scales::percent(sum(results()$alternative2)/sum(results()$baseline) - 1), ")"
          ),
          showcase = bsicons::bs_icon("fuel-pump"),
          class = "bg-primary"
        )
      )

      fluidRow(
        bslib::card(
          bslib::card_header("BASELINE MODEL OBSERVED DATA"),
          bslib::card_body(
            bslib::layout_column_wrap(
              !!!vbs[1:5]
            )
          )
        ),
        bslib::card(
          bslib::card_header("PREDICTIONS FOR ALTERNATIVES"),
          bslib::card_body(
            bslib::layout_column_wrap(
              style="text-align: center",
              h1("Baseline"),
              h1("Alternative 1"),
              h1("Alternative 2")
            ),
            bslib::layout_column_wrap(
              !!!vbs[6:8]
            ),
            bslib::layout_column_wrap(
              !!!vbs[9:11]
            )
          )
        )
      )
    })

    output$accumulated <- echarts4r::renderEcharts4r({
      req(results())

      results <- results()

      results |> 
        dplyr::select(DateTime, baseline, alternative1, alternative2) |> 
        # dplyr::mutate_at(dplyr::vars(-DateTime), cumsum) |> 
        # tidyr::gather(variable, value, -DateTime) |> 
        # dplyr::group_by(variable) |> 
        echarts4r::e_chart(DateTime) |> 
        echarts4r::e_area(baseline, color = "#1e90ff") |> 
        echarts4r::e_line(alternative1, color = "#00FF00") |> 
        echarts4r::e_line(alternative2, color = "#DC143C") |> 
        echarts4r::e_x_axis(axisLabel = list(color = "white")) |> 
        echarts4r::e_y_axis(axisLabel = list(color = "white")) |> 
        echarts4r::e_theme("dark-bold") |> 
        echarts4r::e_datazoom(x_index = 0, type = "slider")
    })

    output$table <- reactable::renderReactable({
      req(results())

      results() |> 
        dplyr::mutate(DateTime = format(DateTime, "%Y-%m-%d %H:%M:%S")) |> 
        dplyr::select(DateTime, baseline, alternative1, alternative2) |> 
        reactable::reactable(
          defaultColDef = reactable::colDef(
            align = "center"
          ),
          theme = reactable::reactableTheme(
            backgroundColor = "#f8f9fa",
            searchInputStyle = list(width = "100%")
          )
        )
    })
  })
}

## To be copied in the UI
# mod_geoh_predicted_ui("predicted_emissions_1")

## To be copied in the server
# mod_geoh_predicted_server("predicted_emissions_1")
