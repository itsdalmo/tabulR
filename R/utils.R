# Extract "simplified" classes. Where, e.g., all date formats are "date" -------
simple_classes <- function(x) {
  stopifnot(is.data.frame(x))
  out <- vapply(x, function(x) class(x)[1], character(1))
  out <- ifelse(out == "integer", "numeric", out)
  out <- ifelse(out %in% c("POSIXct", "POSIXt", "Date"), "date", out)
  out
}

# Hadley's %||% ----------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b


# complete_df <- function(df, vars) {
#   cj <- df[, vars, with = FALSE]
#   cj <- lapply(cj, function(x) { if (is.factor(x)) levels(x) else unique(x) })
#   cj <- expand.grid(cj)
#
#   data.table::setkeyv(df, vars)
#   df[cj]
# }
