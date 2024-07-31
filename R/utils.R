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
