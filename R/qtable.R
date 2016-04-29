#' qtable
#'
#' A function for generating "quick" tables for one or more variables (does not support mixed types).
#' By default it generates proportions for \code{factor} and \code{character} vectors,
#' means for \code{numeric} and \code{integer}, and min/max for \code{Date} (including POSIX)
#' vectors. It always includes the number of observations for each variable.
#' When producing wide tables, uneven counts are separate with \code{/}.
#'
#' @param df A \code{data.frame}, \code{data.table} or \code{Survey}.
#' @param vars The variables to aggregate.
#' @param groups Variables to group by.
#' @param margin Set to TRUE to generate a margin for the first variable in groups.
#' @param wide Should a long or a wide table be returned? Wide tables spread levels for
#' \code{factor} and unique values for \code{character}. For a single \code{numeric},
#' the last group is used, while multiple \code{numeric} will be spread by variable names.
#' @param weight A variable to weight results by.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # TODO

qtable <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  UseMethod("qtable")
}

#' @export
qtable.data.frame <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  df <- data.table::as.data.table(df)
  as.data.frame(qtable_impl(df, vars, groups, weight, margin, wide))
}

#' @export
qtable.data.table <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  df <- data.table::copy(df)
  qtable_impl(df, vars, groups, weight, margin, wide)
}

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
  }

  # Use rbind to include a margin (2x size). Always set the none-margin weights
  # to 1L. If no weight is specified, it is 1L already.
  if (!is.null(groups) && margin)
    df <- rbind(data.table::copy(df)[, wt := 1L], df[, groups[1] := "Total", with = FALSE])
  df <- data.table::melt(df, c(groups, "wt"), vars, value.factor = isTRUE(type == "factor"))

  if (type == "factor" || type == "character") {
    df <- table_freq(df, vars, groups, wide)
  } else if (type == "numeric") {
    df <- table_mean(df, vars, groups, wide)
  } else if (type == "date") {
    df <- table_date(df, vars, groups, wide)
  } else {
    stop("qtable does not support variables of class ", paste0("'", type, "'"))
  }

  df[]

}

#' @export
.datatable.aware <- TRUE
