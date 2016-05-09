qtable_impl <- function(df, vars, groups, weight, margin, wide) {
  if (!length(vars)) {
    stop("No variables specified.")
  } else if (any(vars %in% groups)) {
    stop("Cannot group by and aggregate the same variable.")
  }

  # Subset and add weight (if specified, 1L if not.)
  df <- df[, c(groups, vars, weight), with = FALSE]
  if (!is.null(weight) && weight %in% names(df)) {
    data.table::setnames(df, weight, "wt")
  } else {
    df[, wt := 1L]
  }

  # Coerce integers to numeric. We are doing means anyhow.
  is_int <- vapply(df[, vars, with = FALSE], function(x) is.numeric(x) && is.integer(x), logical(1))
  if (any(is_int)) {
    df[, `:=`(vars[is_int], lapply(.SD, as.numeric)), .SDcols = vars[is_int]]
  }

  # Check variable types (POSIX and dates are treated the same)
  classes <- vapply(df[, vars, with = FALSE], function(x) class(x)[1], character(1))
  classes[classes %in% c("POSIXct", "POSIXt", "Date")] <- "date"

  type <- unique(classes)
  if (length(type) != 1L) {
    stop("qtable does not support mixed variable types.")
  } else if (length(vars) > 1L && !type %in% c("numeric", "factor")) {
    stop("qtable does not support multiple variables if their class is not numeric or factor.")
  } else if (length(vars) > 1L && type == "factor") {
    levels <- lapply(df[, vars, with = FALSE], levels)
    levels <- unlist(lapply(levels[-1L], identical, levels[[1L]]))
    if (!all(levels)) {
      stop("qtable only supports multiple factors with identical levels.")
    }
  }

  # Use rbind to include a margin (2x size. Always set the none-margin weights to 1L.)
  # Melt to get long format before aggregation.
  if (length(groups) && margin) {
    mg <- data.table::copy(df)[, wt := 1L]
    mg[, groups[1] := lapply(.SD, as.factor), .SDcols = groups[1], with = FALSE]
    df <- rbind(mg, df[, groups[1] := "Total", with = FALSE])
  }

  df <- data.table::melt(
    data = df, id.vars = c(groups, "wt"), measure.vars = vars,
    variable.factor = TRUE, value.factor = identical(type, "factor")
  )

  if (type == "factor" || type == "character") {
    df <- qtable_freq(df, vars, groups, wide)
  } else if (type == "numeric") {
    df <- qtable_mean(df, vars, groups, wide)
  } else if (type == "date") {
    df <- qtable_date(df, vars, groups, wide)
  } else {
    stop("qtable does not support variables of class ", paste0("'", type, "'"))
  }

  df[]

}

# Underlying aggregate functions for each variable type ------------------------
qtable_mean <- function(df, vars, groups, wide) {
  # Get the unweighted count and weighted mean (wt = 1L when weight = NULL).
  df <- df[, .(n = .N, value = weighted.mean(value, w = wt, na.rm = TRUE)),
           keyby = c(groups, "variable")]

  # Always complete implicit missing in groups.
  if (length(groups)) df <- complete_df(df, vars = c(groups, "variable"))[is.na(n), n := 0]

  # Return early if not casting
  if (!wide) return(df)

  # Paste together counts for each "grouping" when spreading a numeric
  # by multiple groups. TODO: Include 0's for empty groups as well.
  if (length(groups) > 1L || length(groups) && length(vars) > 1L) {
    # Paste together counts for each "grouping" when spreading a numeric
    # by multiple groups. TODO: Include 0's for empty groups as well.
    grp <- if (length(groups) > 1L) head(groups, -1L) else groups
    df[, n := as.character(n)][, n := paste0(n, collapse = "/"), by = grp]
    fm <- paste0(c(grp, "n"), collapse = "+")
    fm <- paste0(fm, "~", if (length(groups) > 1L) tail(groups, 1L) else "variable")
  } else {
    fm <- paste0(c(groups, "n"), collapse = "+")
    fm <- paste0(fm, "~ variable", collapse = " ")
  }

  df <- data.table::dcast(df, formula = fm, value.var = "value", drop = TRUE, fill = NA)
  df
}

qtable_freq <- function(df, vars, groups, wide) {
  # Drop NA values before aggregating.
  df <- df[!is.na(value),]

  # Sum "wt" to get a weighted count by variable and value.
  # Use this sum to generate proportions. For "n" we use natural weights.
  df <- df[, .(n = .N, wt = sum(wt)), keyby = c(groups, "variable", "value")]
  df[, proportion := prop.table(wt), by = c(groups, "variable")][, wt := NULL]

  # Always complete implicit missing in groups. This includes values for factors.
  df <- complete_df(df, vars = c(groups, "variable", "value"))
  df[is.na(n), n := 0][is.na(proportion), proportion := 0]
  data.table::setkeyv(df, c(groups, "variable", "value"))

  # Return early if not casting
  # (or if it is a single variable with no groups)
  if (!wide || is.null(groups) && length(vars) == 1L)
    return(df)

  # Use total n for groups when casting values
  df[, n := sum(n), by = c(groups, "variable")]
  fm <- paste0(c(groups, if (length(unique(df$variable)) > 1L) "variable", "n"), collapse = "+")
  fm <- paste0(fm, "~ value", collapse = " ")

  # Don't drop levels in LHS (req v1.9.7) when casting. Fill with 0's.
  df <- data.table::dcast(df, formula = fm, value.var = "proportion", drop = TRUE, fill = 0L)
  df
}

qtable_date <- function(df, vars, groups, wide) {
  # Unweighted count and min/max dates.
  df <- df[, .(n = .N, min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)),
           keyby = c(groups, "variable")]

  # Always complete implicit missing in groups.
  if (length(groups)) {
    df <- complete_df(df, vars = c(groups, "variable"))[is.na(n), n := 0]
    data.table::setkeyv(df, c(groups, "variable"))
  }

  # Date tables are "wide" by default. Melt if a long table is desired.
  # If wide is desired, and the length of vars is just 1L - remove var "variable".
  if (!wide) {
    df <- data.table::melt(df, groups, c("min", "max"), variable.name = "type")
  } else if (length(vars) == 1L) {
    df[, variable := NULL]
  }

  df

}
