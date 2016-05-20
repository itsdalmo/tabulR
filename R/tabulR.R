#' tabulR/qtable: Create "quick" tables.
#'
#' This is a tiny package that uses \code{\link[data.table]{data.table}} to generate
#' a quick margin table or plot for one or more variables, by one or more groups.
#' See \code{\link{qtable}}, \code{\link{bar_chart}} and \code{\link{line_chart}} for more details.
#'
#' @importFrom stats weighted.mean
#' @importFrom utils head tail
#' @author Kristian D. Olsen
#' @docType package
#' @name tabulR

NULL

#' @export
.datatable.aware <- TRUE
