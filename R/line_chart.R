#' Line chart
#'
#' Create a line chart to easily visualize numeric data. This function
#' is meant to be used with \code{\link[tabulR]{qtable}}, but only supports one
#' grouping variable.
#'
#' @inheritParams bar_chart
#' @param wrap Ignored.
#' @param ... Unquoted variable names passed to \code{\link[dplyr]{select}}.
#' @author Kristian D. Olsen
#' @importFrom ggplot2 theme_gray theme element_rect element_text element_blank element_line
#' @seealso bar_chart
#' @export
#' @examples
#' NULL

line_chart <- function(df, ..., groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("The NSE version of bar_chart requires dplyr.")

  # Use dplyr to select vars. Also, look for dplyr groups if not specified.
  vars <- dplyr::select_vars_(names(df), lazyeval::lazy_dots(...))
  groups <- groups %||% as.character(dplyr::groups(df))
  if (!length(groups)) groups <- NULL

  # Rename vars before creating table
  if (any(names(vars) != vars)) {
    if (data.table::is.data.table(df)) {
      data.table::setnames(df, unname(vars), names(vars))
    } else {
      names(df)[match(vars, names(df))] <- names(vars)
    }
  }

  line_chart_(df, vars = names(vars), groups = groups, weight = weight, margin = margin, margin_name = margin_name, wrap = wrap)

}

#' @rdname line_chart
#' @export
line_chart_ <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  if (length(groups) > 1L)
    stop("line_chart can only handle 1 grouping variable.")
  UseMethod("line_chart_")
}

#' @export
line_chart_.data.frame <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, margin_name = margin_name, wide = FALSE)
  line_chart_impl(out, vars, groups, weight, margin, margin_name, wrap)
}

#' @export
line_chart_.qtable <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  line_chart_impl(df, vars, groups, weight, margin, margin_name, wrap)
}

line_chart_impl <- function(df, vars, groups, weight, margin, margin_name, wrap) {
  if ("proportion" %in% names(df)) stop("Use bar_chart() to plot proportions.")

  # Create the plot
  out <- ggplot2::ggplot(
    data = df,
    ggplot2::aes_string(
      x     = "variable",
      y     = "value",
      ymin  = min(df$value) * 0.8,
      ymax  = max(df$value) * 1.1,
      group = groups,
      colour  = groups)
  )

  # Add geom line and point (dots)
  out <- out + ggplot2::geom_line(size = 1L) + ggplot2::geom_point(size = 3L)

  # Add labels to each line
  out <- out + ggplot2::geom_text(
    ggplot2::aes(
    label    = sprintf("%.1f", value)),
    size     = 3,
    colour   = "#23373b"
  )

  out

}
