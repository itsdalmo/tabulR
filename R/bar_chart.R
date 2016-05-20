#' Bar chart
#'
#' Create a bar chart to easily visualize numeric and/or categorical data. This function
#' is meant to be used with \code{\link[tabulR]{qtable}}, but only supports one
#' grouping variable.
#'
#' @inheritParams qtable_
#' @param wrap Optional: Call \code{\link[ggplot2]{facet_wrap}} on variables in \code{bar_chart}.
#' @param ... Unquoted variable names passed to \code{\link[dplyr]{select}}.
#' @author Kristian D. Olsen
#' @importFrom ggplot2 theme_gray theme element_rect element_text element_blank element_line
#' @seealso line_chart
#' @export
#' @examples
#' NULL

bar_chart <- function(df, ..., groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
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

  bar_chart_(df, vars = names(vars), groups = groups, weight = weight, margin = margin, wrap = wrap)

}

#' @rdname bar_chart
#' @export
bar_chart_ <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  if (length(groups) > 1L)
    stop("bar_chart can only handle 1 grouping variable.")
  UseMethod("bar_chart_")
}

#' @export
bar_chart_.data.frame <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, wide = FALSE)
  bar_chart_impl(out, vars, groups, weight, margin, wrap)
}

#' @export
bar_chart_.qtable <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  bar_chart_impl(df, vars, groups, weight, margin, wrap)
}

bar_chart_impl <- function(df, vars, groups, weight, margin, wrap) {
  is_grouped <- !is.null(groups)
  is_proportion <- "proportion" %in% names(df)
  multiple_vars <- length(vars) > 1L

  # Cannot visualize multiple proportions with groups, without
  # wrapping. Override with a warning.
  if (is_proportion && multiple_vars && is_grouped && !wrap) {
    warning("Multiple proportions with groups have to be wrapped. Ignoring wrap = FALSE.", call. = FALSE)
    wrap <- TRUE
  } else if (!multiple_vars && wrap) {
    warning("Ignoring wrap = TRUE when plotting a single variable.", call. = FALSE)
    wrap <- FALSE
  }

  # Use groups as the x-axis if it contains more unique values than the
  # proportions being plotted, or the number of variables being plotted for numeric plots.
  if (is_grouped) {
    group_length <- length(levels(df[[groups]]) %||% unique(df[[groups]]))
    xvar <- if (is_proportion) "value" else "variable"
    xvar_length <- length(levels(df[[xvar]]) %||% unique(df[[xvar]]))
  }

  # Figure out the best aesthetics for visualizing the plot.
  # (Depends on: proportion/numeric, grouped or not, single/mult. vars, wrap etc.)
  if (is_proportion) {
    xvar  <- "value"
    yvar  <- "proportion"
    ymin  <- 0L
    ymax  <- 1.05
    group <- "value"
    fill  <- if (is_grouped) groups else NULL

    # Swap xvar and fill if necessary.
    if (is_grouped && group_length > xvar_length) {
      xvar <- groups
      fill <- "value"
    }
  } else {
    xvar  <- "variable"
    yvar  <- "value"
    ymin  <- min(df$value, na.rm = TRUE) * 0.8
    ymax  <- max(df$value, na.rm = TRUE) * 1.2
    group <- groups
    fill  <- if (is_grouped) groups else NULL

    # Swap xvar and fill if necessary.
    if (is_grouped && group_length > xvar_length) {
      xvar <- groups
      group <- "variable"
      fill <- if (multiple_vars && !wrap) "variable" else NULL
    }
  }

  # Create the bar plot using predefined aes
  out <- ggplot2::ggplot(
    data = df, ggplot2::aes_string(
      x = xvar,
      y = yvar,
      ymin = ymin,
      ymax = ymax,
      group = group,
      fill  = fill)) +

    ggplot2::geom_bar(
      stat = "identity",
      width = .5,
      position = ggplot2::position_dodge(width = .6))

  # Add labels to each bar.
  # Round percentages to a whole number, keep 1 decimal for numeric.
  out <- out + ggplot2::geom_text(
    ggplot2::aes(
      label = if (is_proportion) sprintf("%.0f %%", proportion * 100L) else sprintf("%.1f", value)),
    position = ggplot2::position_dodge(width = 0.6),
    vjust = -1.1,
    hjust = .35,
    size = 3,
    colour = "#23373b"
  )

  # For proportions, we want yaxis ticks to be percentages as well.
  # In case of numeric, we only want to show a subset of the plot area.
  if (is_proportion) {
    out <- out + ggplot2::scale_y_continuous(labels = scales::percent)
  } else {
    out <- out + ggplot2::coord_cartesian(ylim=c(ymin, ymax))
  }

  # Optionally wrap the results by variable.
  if (wrap) {
    out <- out + ggplot2::facet_wrap(~ variable, ncol = 2L, scales="free_x")
  }

  out

}