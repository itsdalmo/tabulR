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

#' qtable
#'
#' Generating "quick" tables for one or more variables (does not support mixed types).
#' By default \code{qtable} generates proportions for \code{factor} and \code{character} vectors,
#' means for \code{numeric} and \code{integer}, and min/max for \code{Date} (including POSIX)
#' vectors. It always includes the number of observations for each variable.
#' When producing wide tables, group counts are separate with \code{/}.
#'
#' @param df A \code{data.frame} or \code{data.table}.
#' @param vars The variables to aggregate.
#' @param groups Variables to group by.
#' @param weight A variable to weight results by. This is only applied to the margin.
#' @param margin Set to TRUE to generate a margin for the first group.
#' @param wide Should a long or a wide table be returned? Wide tables spread levels for
#' \code{factor} and unique values for \code{character}. For a single \code{numeric},
#' the last group is used, while multiple \code{numeric} will be spread by variable names.
#' @return A \code{data.frame} or \code{data.table} with the additional class \code{qtable}.
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
