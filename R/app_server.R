#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # data <- shiny::eventReactive(input$file, {
  #   file <- input$file

  #   shiny::req(file)

  #   ext <- tools::file_ext(file$datapath)

  #   shiny::validate(shiny::need(ext == "xlsx", "Please upload a xlsx file"))

  #   readxl::read_xlsx(file$datapath)
  # })

  # output$table <- shiny::renderTable({
  #   shiny::req(data())

  #   data()
  # })

  output$plot1 <- shiny::renderPlot({
    energy <- readxl::read_xlsx("output.xlsx", sheet = 1)[, -1]

    tibble::tibble(
      index = 1:ncol(energy),
      value = as.numeric(tail(energy, 1))
    ) |>
      ggplot(aes(x = index, y = value)) +
      geom_point() +
      geom_line()
  })

  output$plot2 <- shiny::renderPlot({
    soc <- readxl::read_xlsx("output.xlsx", sheet = 2)[, -1]

   tibble::tibble(
      index = 1:ncol(soc),
      value = as.numeric(tail(soc, 1))
    ) |>
      ggplot(aes(x = index, y = value)) +
      geom_point() +
      geom_line()
  })

  output$optimal_value <- shiny::renderText({
    ov <- readxl::read_xlsx("output.xlsx", sheet = 4)[, -1]
    ov[[1]]
  })

  output$power_value <- shiny::renderText({
    power <- readxl::read_xlsx("output.xlsx", sheet = 3)[, -1]
    tail(power, 1)[[1]]
  })
}
