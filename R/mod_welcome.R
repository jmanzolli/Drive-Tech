#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textOutput renderText
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  # tagList(
  #   bslib::page_fluid(
  #     bslib::card(
  #       bslib::card_header("Welcome"),
  #       bslib::card_body(
  #         shiny::p(
  #           class = "fontize",
  #           "
  #           The GEOH app allows you to predict  the total Fuel Consumption Rate 
  #           generated during a trip. It also provides you with useful dashboards
  #           to gain insights into the quantity of emissions generated during a 
  #           trip. The platform was made by the Innovation in Mobility and 
  #           Transportation Safety Lab (IMaTS) at McGill University, under the 
  #           supervision of Luis F. Miranda-Moreno.
  #           "
  #         )
  #       )
  #     ),
  #     bslib::layout_column_wrap(
  #       width = 1 / 2,
  #       bslib::card(
  #         bslib::card_header("Metrics"),
  #         bslib::card_body(
  #           bslib::layout_column_wrap(
  #             width = 1 / 2,
  #             shiny::div(
  #               shiny::h1("Input"),
  #               shiny::p(
  #                 class = "fontize",
  #                 "
  #                 The app will require trip data with speed, acceleration, slope,
  #                 stored as a coma-separated values (.csv) files with the same 
  #                 format and names as the example files located in the app folder
  #                 'Actual Data' (you can use them to input the data).
  #                 "
  #               ),
  #               shiny::p(class = "fontize", "Steps for inputting data into the app:"),
  #               shiny::tags$ol(
  #                 shiny::tags$li(class = "fontize", "Go to the Metrics tab"),
  #                 shiny::tags$li(class = "fontize", "⁠Browse the folder where the data is located"),
  #                 shiny::tags$li(class = "fontize", "⁠Select all datasets you would like to upload into the application and upload them at once")
  #               )
  #             ),
  #             shiny::div(
  #               shiny::h1("Output"),
  #               shiny::p(class = "fontize", "You will see 2 outputs:"),
  #               shiny::tags$ol(
  #                 shiny::tags$li(class = "fontize", "⁠Descriptive Indicators: The
  #                 app will provide you with indicators that account for all 
  #                 datasets you have uploaded"),
  #                 shiny::tags$li(class = "fontize", "⁠⁠Interactive Map: a map made
  #                 using the plotly package, which means they are completely 
  #                 interactive. To activate or deactivate any line, boxplot or 
  #                 graph, simply click on its name listed inside the legend. 
  #                 Double-click it to isolate it.")
  #               )
  #             )
  #           )
  #         )
  #       ),
  #       bslib::card(
  #         bslib::card_header("Predictions"),
  #         bslib::card_body(
  #          bslib::layout_column_wrap(
  #             width = 1 / 2,
  #             shiny::div(
  #               shiny::h1("Input"),
  #               shiny::p(class = "fontize", "The app will require trip data with 
  #                 speed, acceleartion, slope, vehilce weight and engine_size 
  #                 stored as a coma-separated values (.csv) files with the same 
  #                 format and names as the example files located in the app 
  #                 folder (you can use them to put in the data)."),
  #               shiny::p(class = "fontize", "Steps for inputting data into the app:"),
  #               shiny::tags$ol(
  #                 shiny::tags$li(class = "fontize", "⁠⁠Go to the Predictions tab"),
  #                 shiny::tags$li(class = "fontize", "Browse and upload one dataset (the one that corresponds to the baseline model)"),
  #                 shiny::tags$li(class = "fontize", "⁠⁠Select the same baseline model in the “Baseline Model” droplist"),
  #                 shiny::tags$li(class = "fontize", "⁠⁠Select the Alternative 1 model of the car you would like to compare in the droplist"),
  #                 shiny::tags$li(class = "fontize", "⁠⁠Select the Alternative 2 model of the car you would like to compare in the droplist"),
  #                 shiny::tags$li(class = "fontize", "⁠Click on Predict")
  #               )
  #             ),
  #             shiny::div(
  #               shiny::h1("Output"),
  #               shiny::p(class = "fontize", "You will see 3 outputs:"),
  #               shiny::tags$ol(
  #                 shiny::tags$li(class = "fontize", "⁠⁠Descriptive Indicators for the Baseline Model: The app will provide you with emission-related indicators for the baseline model describing the dataset you have uploaded."),
  #                 shiny::tags$li(class = "fontize", "Predicted Fuel Consumtion Rates: The app will provide you with the predicted Fuel Consumption Rate for the baseline model as well as the predicted Fuel Consumption Rate for each of the 2 alternatives you have selected."),
  #                 shiny::tags$li(class = "fontize", "An interactive map with the Fuel Consumption Rate for all cars selected as a function of time.")
  #               )
  #             )
  #           )
  #         )
  #       )
  #     )
  #   )
  # )

  bslib::page_fillable(
    shiny::column(
      12, align = "center", style = "padding: 31px",
      shiny::tags$image(src = "www/bbb.png", width = "50%")
    )
  )
}

#' start Server Functions
#'
#' @noRd
mod_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
