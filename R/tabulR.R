#' tabulR/qtable: Create "quick" tables.
#'
#' This is a tiny package with two functions (named \code{qtable} and \code{qtable_})
#' that uses \code{\link[data.table]{data.table}} to generate a quick margin table for one
#' or more variables, by one or more groups. See \code{\link{qtable}} for more details.
#'
#' @importFrom stats weighted.mean
#' @importFrom utils head tail
#' @author Kristian D. Olsen
#' @docType package
#' @name tabulR

NULL

#' @export
.datatable.aware <- TRUE
