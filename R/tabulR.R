#' tabulR/qtable: Create "quick" tables.
#'
#' This is a tiny packaged function (named \code{qtable}) that uses
#' \code{\link[data.table]{data.table}} to generate a quick margin table for one
#' or more variables, by one or more groups. See \code{\link{qtable}} for more details.
#'
#' @author Kristian D. Olsen
#' @docType package
#' @name tabulR

NULL

#' @export
.datatable.aware <- TRUE

#' @importFrom knitr knit_print
#' @export
knit_print.qtable <- function(x, format = "html", align = NULL, digits = 1L, ...) {
  # Default alignment
  def <- rep("l", ncol(x)); def[vapply(x, is.numeric, logical(1L))] <- "c"

  # Format numerics between 0 and 1 as percentages.
  is_pct <- vapply(x, is_percent, logical(1))
  if (any(is_pct)) {
    fmt <-  paste0("%.", digits, "f%%")
    x[is_pct] <- lapply(x[is_pct], function(p) sprintf(fmt = fmt, p*100L))
  }

  knitr::kable(x, format, align = align %||% def, digits, ...)
}