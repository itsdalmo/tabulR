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
  if (length(groups) > 2L)
    stop("line_chart can only handle <= 2 grouping variables.")
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

  mult_var <- length(levels(df$variable)) > 1L
  mult_grp <- length(groups) > 1L

  if (!mult_var && !mult_grp) {
    stop("Cannot create line_chart with a single variable and <= 1 groups.", call. = FALSE)
  } else if (mult_var && mult_grp && !wrap) {
    warning("Multiple variables with more than one group have to be wrapped. Ignoring wrap = FALSE.", call. = FALSE)
    wrap <- TRUE
  } else if ((!mult_var || !mult_grp) && wrap) {
    warning("Ignoring wrap = TRUE when not plotting multiple variables and/or groups.", call. = FALSE)
    wrap <- FALSE
  }

  # Create the plot
  if (mult_grp) {
    xvar  <- if (mult_var) "variable" else tail(groups, 1L)
    yvar  <- "value"
    ymin  <- min(df$value) * 0.75
    ymax  <- max(df$value) * 1.2
    group <- head(groups, 1L)
    colour  <- head(groups, 1L)
  } else {
    xvar  <- "variable"
    yvar  <- "value"
    ymin  <- min(df$value) * 0.75
    ymax  <- max(df$value) * 1.2
    group <- groups %||% NA
    colour  <- groups
  }

  out <- ggplot2::ggplot(
    data = df, ggplot2::aes_string(
      x = xvar,
      y = yvar,
      ymin = ymin,
      ymax = ymax,
      group = group,
      colour  = colour))

  # Add geom line and point (dots)
  out <- out + ggplot2::geom_line(size = 1L) + ggplot2::geom_point(size = 3L)

  # Add label to the margin/last level in first group.
  if (is.null(margin_name) && !is.null(groups)) {
    margin_name <- levels(df[[head(groups, 1L)]])
    margin_name <- tail(margin_name, 1L)
  }

  # TODO: vjust geom_text.

  out <- out + ggplot2::geom_text(
    data = if (!is.null(groups)) df[df[[head(groups, 1L)]] == margin_name, ] else df,
    ggplot2::aes(
    label    = sprintf("%.1f", value)),
    size     = 3,
    colour   = "#23373b"
  )

  # Optionally wrap the results by variable.
  if (wrap) {
    wby <- if (mult_grp) tail(groups, 1L) else "variable"
    out <- out + ggplot2::facet_wrap(wby, ncol = 2L, scales="free_x")
  }

  out

}
