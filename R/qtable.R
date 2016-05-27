#' qtable
#'
#' Same as \code{\link{qtable_}}, except designed for use with \code{dplyr}. It takes
#' unquoted variable names and supports all dplyr select statements, except renaming in
#' the select statement itself. \code{qtable} also looks for \code{dplyr} groups if
#' they are not specified.
#'
#' Note that \code{groups} and \code{weight} still have to be character vectors.
#'
#' @inheritParams qtable_
#' @param ... Unquoted variable names passed to \code{\link[dplyr]{select}}.
#' @return Same as \code{\link{qtable}}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # TODO

qtable <- function(df, ..., groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("The NSE version of qtable requires dplyr.")

  # Use dplyr to select vars. Also, look for dplyr groups if not specified.
  vars <- dplyr::select_vars_(names(df), lazyeval::lazy_dots(...))

  # Rename vars before creating table
  if (any(names(vars) != vars)) {
    if (data.table::is.data.table(df)) {
      data.table::setnames(df, unname(vars), names(vars))
    } else {
      names(df)[match(vars, names(df))] <- names(vars)
    }
  }

  qtable_(df, vars = names(vars), groups = groups, weight = weight, margin = margin, wide = wide)

}

#' qtable
#'
#' Generating "quick" tables for one or more variables (does not support mixed types).
#' By default \code{qtable} generates proportions for \code{factor} and \code{character} vectors,
#' means for \code{numeric} and \code{integer}, and min/max for \code{Date} (including POSIX)
#' vectors. It always includes the number of observations for each variable.
#' When producing wide tables, group counts are separate with \code{/}.
#'
#' @param df A \code{data.frame} or \code{data.table}.
#' @param vars The variables to aggregate, as a character vector.
#' @param groups Variables to group by during aggregation.
#' @param weight Name of a variable to weight by. Only used when \code{margin = TRUE}.
#' @param margin If \code{TRUE} (the default), the first group will include a "Total".
#' @param margin_name Optional: Give the margin a different name from "Total".
#' @param wide Should a long or a wide table be returned? Wide tables spread levels for
#' \code{factor} and unique values for \code{character}. For a single \code{numeric},
#' the last group is used, while multiple \code{numeric} will be spread by variable names.
#' @return A \code{data.frame} or \code{data.table} with the additional class \code{qtable}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # TODO

qtable_ <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wide = TRUE) {
  UseMethod("qtable_")
}

#' @export
qtable_.data.frame <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wide = TRUE) {
  if (is.null(groups) && requireNamespace("dplyr", quietly = TRUE)) {
    groups <- dplyr::groups(df)
    if (!is.null(groups))
      groups <- as.character(groups)
  }
  df <- data.table::as.data.table(df)
  df <- as.data.frame(qtable_impl(df, vars, groups, weight, margin, margin_name, wide))
  structure(df, class = c("qtable", class(df)))
}

#' @export
qtable_.data.table <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wide = TRUE) {
  if (is.null(groups) && requireNamespace("dplyr", quietly = TRUE)) {
    groups <- dplyr::groups(df)
    if (!is.null(groups))
      groups <- as.character(groups)
  }
  df <- data.table::copy(df)
  df <- qtable_impl(df, vars, groups, weight, margin, margin_name, wide)
  structure(df, class = c("qtable", class(df)))
}

#' @importFrom knitr knit_print
#' @export
knit_print.qtable <- function(x, format = "html", align = NULL, digits = 1L, ...) {
  x <- as.data.frame(x)

  # Default alignment
  def <- rep("l", ncol(x)); def[vapply(x, is.numeric, logical(1L))] <- "c"

  # Format numerics between 0 and 1 as percentages.
  is_pct <- vapply(x, is_percent, logical(1))
  if (any(is_pct)) {
    x[is_pct] <- lapply(x[is_pct], function(p) sprintf(fmt = paste0("%.", digits, "f%%"), p*100L))
  }

  knitr::knit_print(knitr::kable(x, format, align = align %||% def, digits, ...))
}