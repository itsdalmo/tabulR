# qtable -----------------------------------------------------------------------
qtable_impl <- function(df, vars, groups, weight, margin, wide) {
  if (!length(vars)) stop("No variables specified.")
  if (any(vars %in% groups)) stop("Cannot group by and aggregate the same variable.")

  # Subset data and make sure variables are specified correctly
  df <- df[, c(groups, vars, weight), with = FALSE][, "wt" := weight %||% 1L, with = FALSE]
  type <- unique(simple_classes(df[, vars, with = FALSE]))

  # Multiple types or multiple variables when type is not character
  # or factor, are not supported. Mult. factors must have identical levels.
  if (length(type) != 1L) {
    stop("qtable does not support mixed classes.")
  } else if (length(vars) > 1L && !type %in% c("numeric", "factor")) {
    stop("qtable does not support multiple variables if all classes are not factor, or numeric.")
  } else if (length(vars) > 1L && type == "factor" && wide) {
    levels <- lapply(df[, vars, with = FALSE], levels)
    levels <- unlist(lapply(levels[-1L], identical, levels[[1L]]))
    if (!all(levels)) stop("Multiple factors must have identical levels to spread.")
  } else if (length(vars) > 1L && type == "numeric") {
    # Make sure integers are converted to numeric before melting to stop warnings.
    is_integer <- vapply(df[, vars, with = FALSE], is.integer, logical(1L))
    if (any(is_integer) && !all(is_integer))
      df[, `:=`(vars[is_integer], lapply(.SD, as.numeric)), .SDcols = vars[is_integer]]
  }

  # Use rbind to include a margin (2x size). Always set the none-margin weights
  # to 1L. If no weight is specified, it is 1L already.
  if (length(groups) && margin)
    df <- rbind(data.table::copy(df)[, wt := 1L], df[, groups[1] := "Total", with = FALSE])

  df <- data.table::melt(
    df, c(groups, "wt"), vars, variable.factor = TRUE, value.factor = identical(type, "factor"))

  # Filter NA values in groups and/or response.
  is_na <- vapply(df[, groups, with = FALSE], is.na, logical(nrow(df)))
  is_na <- rowSums(data.frame(is_na)) > 0L
  df <- df[!is.na(value) & !is_na]

  if (type == "factor" || type == "character") {
    df <- qtable_freq(df, vars, groups, wide)
  } else if (type == "numeric") {
    df <- qtable_mean(df, vars, groups, wide)
  } else if (type == "date") {
    df <- qtable_date(df, vars, groups, wide)
  } else {
    stop("qtable does not support variables of class ", paste0("'", type, "'"))
  }

  structure(df[], class = c("qtable", class(df)))

}

# Underlying aggregate functions for each variable type ------------------------
# Numeric/integer
qtable_mean <- function(df, vars, groups, wide) {
  df <- df[, .(n = .N, value = weighted.mean(value, w = wt, na.rm = TRUE)), by = c(groups, "variable")]
  if (wide) {
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

    df <- data.table::dcast(df, formula = fm, value.var = "value", drop = c(TRUE, FALSE))
  }
  df
}

# Factor/character
qtable_freq <- function(df, vars, groups, wide) {
  # Sum "wt" to get a weighted count by variable and value.
  # Use this sum to generate proportions. For "n" we use natural weights.
  df <- df[, .(n = .N, wt = sum(wt)), by = c(groups, "variable", "value")]
  df[, proportion := prop.table(wt), by = c(groups, "variable")][, wt := NULL]

  # Return early if not casting
  if (!wide) return(df)

  # Use total n for groups when casting values
  df[, n := sum(n), by = c(groups, "variable")]
  fm <- paste0(c(groups, if (length(unique(df$variable)) > 1L) "variable", "n"), collapse = "+")
  fm <- paste0(fm, "~ value", collapse = " ")

  # Don't drop levels in LHS (req v1.9.7) when casting. Fill with 0's.
  df <- data.table::dcast(df, formula = fm, value.var = "proportion", drop = c(TRUE, FALSE), fill = 0L)
  df
}

# Date
qtable_date <- function(df, vars, groups, wide) {
  df <- df[, .(n = .N, min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)), by = c(groups, "variable")]
  if (!wide) {
    df <- data.table::melt(df, groups, c("min", "max"), variable.name = "type")
  } else if (length(vars) == 1L) {
    df[, variable := NULL]
  }
  df
}

