devtools::load_all()

reticulate::source_python("./inst/python/run_gurobi.py", envir = .GlobalEnv)

file <- aux <- list()

file$datapath <- pkg_resource("./data/drive_tech/v2/input.xlsx")
ext <- tools::file_ext(file$datapath)
sheets <- readxl::excel_sheets(file$datapath)
data <- purrr::map(sheets, function(.x) {
    readxl::read_xlsx(file$datapath, .x)
})
names(data) <- sheets
aux$drive_tech_data <- data
aux$drive_tech_data$type <- "file"
manual <- aux$drive_tech_data$type
manual <- manual == "manual"

log_file <- tempfile(fileext = ".log")
result <- run_gurobi(aux$drive_tech_data, log_file, manual)
result$CHARGERS_ASSIGNED <- result$CHARGERS_ASSIGNED |>
    dplyr::mutate(
        Time = lubridate::hm(Time),
        Time = lubridate::hour(Time) + (lubridate::minute(Time) / 60)
    )

result <- purrr::map(result, tibble::as_tibble)
names(result) <- c("ENERGY", "SOC", "POWER", "CHARGERS_ENABLED", "CHARGERS_ASSIGNED", "OPTIMAL")

result$log <- paste0(readLines(log_file), collapse = "\n")

aux$drive_tech_data_op <- result

saveRDS(result, "result.rds")
