pkg_resource <- function(...) {
    system.file(..., package = "driveTech")
}

cad <- function(x, digits = 2) {
    if (!is.null(x)) {
        as.character(formattable::currency(x, "CAD ", big.mark = ".", dec = ",", digits = digits))
    } else {
        return(NA)
    }
}

euro <- function(x, digits = 2) {
    if (!is.null(x)) {
        as.character(formattable::currency(x, "€ ", big.mark = ".", dec = ",", digits = digits))
    } else {
        return(NA)
    }
}

decimal_to_hm <- function(decimal_hour) {
  hours <- floor(decimal_hour)
  minutes <- round((decimal_hour - hours) * 60)
  sprintf("%02d:%02d", hours, minutes)
}

num <- function(x, big = ",", dec = ".", digits = 2) {
    as.character(format(as.numeric(x), big.mark = big, decimal.mark = dec, digits = 2, scientific = FALSE))
}

available_models_list <- function(src) {
    available_models_name <- list.files(pkg_resource(src))
    available_models_name <- stringr::str_replace_all(available_models_name, "\\.pickle", "")
    available_models_name <- stringr::str_replace_all(available_models_name, "_", " ")
    available_models_name <- trimws(stringr::str_replace_all(available_models_name, "^[0-9]+", ""))
    available_models <- list.files(pkg_resource(src), full.names = TRUE)
    names(available_models) <- available_models_name
    return(available_models)
}

custom_dark_theme <- reactable::reactableTheme(
    backgroundColor = "#2E2E2E",
    color = "#FFFFFF",
    borderColor = "#444444",
    stripedColor = "#3C3C3C",
    highlightColor = "#444444",
    inputStyle = list(
        backgroundColor = "#444444",
        color = "#FFFFFF" 
    ),
    style = list(
        fontFamily = "Arial, sans-serif"
    ),
    headerStyle = list(
        backgroundColor = "#3C3C3C",
        color = "#FFFFFF",
        fontWeight = "bold"
    )
)