devtools::load_all()

reticulate::source_python("./inst/python/run_gurobi.py", envir = .GlobalEnv)

file <- aux <- list()

file$datapath <- pkg_resource("./data/v2/input.xlsx")
ext <- tools::file_ext(file$datapath)
sheets <- readxl::excel_sheets(file$datapath)
data <- purrr::map(sheets, function(.x) {
    readxl::read_xlsx(file$datapath, .x)
})
names(data) <- sheets
aux$input_data <- data


log_file <- tempfile(fileext = ".log")
result <- run_gurobi(aux$input_data, log_file)

result <- purrr::map(result, tibble::as_tibble)
names(result) <- c("ENERGY", "SOC", "POWER", "CHARGERS_ENABLED", "CHARGERS_ASSIGNED", "OPTIMAL")

result$log <- paste0(readLines(log_file), collapse = "\n")

aux$output_data <- result

saveRDS(result, "result.rds")
