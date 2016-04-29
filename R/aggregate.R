table_mean <- function(df, vars, groups, wide) {
  df <- df[, .(n = .N, value = mean(value, na.rm = TRUE)), by = c(groups, "variable")]
  if (wide) {
    if (length(groups) > 1L && length(vars) == 1L) {
      spread_by <- tail(groups, 1L)
      group_by <- setdiff(groups, spread_by)
      df[, n := as.character(n)][, n := paste0(n, collapse = "/"), by = group_by]
      fm <- paste0(c(group_by, "n"), collapse = "+")
      fm <- paste0(fm, "~", spread_by)
    } else {
      df[, n := as.character(n)][, n := paste0(n, collapse = "/"), by = groups]
      fm <- paste0(c(groups, "n"), collapse = "+")
      fm <- paste0(fm, "~ variable", collapse = " ")
    }

    df <- data.table::dcast(df, formula = fm, value.var = "value")
  }
  df
}

table_freq <- function(df, var, groups, wide) {
  # Sum "wt" to get a weighted count by variable and value.
  # Use this sum to generate proportions. For "n" we use natural weights.
  df <- df[, .(n = .N, wt = sum(wt)), by = c(groups, "variable", "value")]
  df[, proportion := prop.table(wt), by = c(groups, "variable")][, wt := NULL]
  if (wide) {
    df[, n := sum(n), by = c(groups, "variable")]
    fm <- paste0(c(groups, if (length(unique(df$variable)) > 1L) "variable", "n"), collapse = "+")
    fm <- paste0(fm, "~ value", collapse = " ")
    df <- data.table::dcast(df, formula = fm, value.var = "proportion")
  }
  df
}

table_date <- function(df, var, groups, wide) {
    df <- df[, .(n = .N, min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)), by = c(groups, "variable")]
    if (!wide) {
      df <- data.table::melt(df, groups, c("min", "max"), variable.name = "type")
    }
}

