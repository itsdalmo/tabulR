# Complete a data.frame with implicit missing values ---------------------------
complete_df <- function(df, vars) {
  cj <- df[, vars, with = FALSE]
  cj <- lapply(cj, function(x) { if (is.factor(x)) levels(x) else unique(x) })
  cj <- expand.grid(cj)

  data.table::setkeyv(df, vars)
  df[cj]
}

# Check if a vector represents a percentage ------------------------------------
is_percent <- function(x) {
  !all(is.na(x)) && is.numeric(x) && !is.integer(x) && all(x <= 1L & x >= 0L)
}

# Hadley's %||% ----------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b


