#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  data <- shiny::eventReactive(input$file, {
    file <- input$file
    
    shiny::req(file)

    ext <- tools::file_ext(file$datapath)

    shiny::validate(shiny::need(ext == "xlsx", "Please upload a xlsx file"))

    readxl::read_xlsx(file$datapath)
  })

  output$table <- shiny::renderTable({
    shiny::req(data())

    data()
  })
} 
