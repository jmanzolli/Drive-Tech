#' @export
pkg_resource <- function(...) {
    system.file(..., package = "driveTech")
}

#' @export 
cad <- function(x, digits = 2) {
    if (!is.null(x)) {
        as.character(formattable::currency(x, "CAD ", big.mark = ".", dec = ",", digits = digits))
    } else {
        return(NA)
    }
}

#' @export 
euro <- function(x, digits = 2) {
    if (!is.null(x)) {
        as.character(formattable::currency(x, "€ ", big.mark = ".", dec = ",", digits = digits))
    } else {
        return(NA)
    }
}

#' @export
decimal_to_hm <- function(decimal_hour) {
  hours <- floor(decimal_hour)
  minutes <- round((decimal_hour - hours) * 60)
  sprintf("%02d:%02d", hours, minutes)
}

#' @export
num <- function(x, big = ",", dec = ".", digits = 2) {
    as.character(format(as.numeric(x), big.mark = big, decimal.mark = dec, digits = 2, scientific = FALSE))
}

#' @export 
available_models_list <- function(src) {
    available_models_name <- list.files(pkg_resource(src))
    available_models_name <- stringr::str_replace_all(available_models_name, "\\.pickle", "")
    available_models_name <- stringr::str_replace_all(available_models_name, "_", " ")
    available_models_name <- trimws(stringr::str_replace_all(available_models_name, "^[0-9]+", ""))
    available_models <- list.files(pkg_resource(src), full.names = TRUE)
    names(available_models) <- available_models_name
    return(available_models)
}
