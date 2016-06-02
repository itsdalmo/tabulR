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

bar_chart <- function(df, ..., groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
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

  bar_chart_(df, vars = names(vars), groups = groups, weight = weight, margin = margin, margin_name = margin_name, wrap = wrap)

}

#' @rdname bar_chart
#' @export
bar_chart_ <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  if (length(groups) > 2L)
    stop("bar_chart can only handle <= 2 grouping variables.")
  UseMethod("bar_chart_")
}

#' @export
bar_chart_.data.frame <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, margin_name = margin_name, wide = FALSE)
  bar_chart_impl(out, vars, groups, weight, margin, margin_name, wrap)
}

#' @export
bar_chart_.qtable <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  bar_chart_impl(df, vars, groups, weight, margin, margin_name, wrap)
}

bar_chart_impl <- function(df, vars, groups, weight, margin, margin_name, wrap) {
  is_proportion <- "proportion" %in% names(df)
  grouped <- !is.null(groups)

  # Decide on aesthetics based on type and number of variables, and groups
  xaxis <- if (is_proportion) "value" else "variable"     # value == factor levels
  yaxis <- if (is_proportion) "proportion" else "value"   # proportion/numeric value
  group <- if (grouped) tail(groups, 1L) else NULL        # group/fill.
  if (is_proportion) group <- head(groups, 1L)            # Special for proportions.
  wrapv <- if (wrap) "variable" else NULL                 # Wrap variable by default.

  # Give informative warnings when input cannot be visualized. Differs between
  # proportions and numerics, since proportions also need to indicate "value".
  if (is_proportion && length(groups) > 1L && length(vars) > 1L) {
    stop("Cannot plot multiple proportions with multiple groups.", call. = FALSE)
  } else if (is_proportion && length(vars) > 1L && length(group) && !wrap) {
    warning("Plotting multiple proportions with groups requires wrapping. Ignoring wrap = FALSE.", call. = FALSE)
    wrap <- TRUE
  } else if (is_proportion && length(groups) > 1L && !wrap) {
    warning("Plotting a proportion with multiple groups requires wrapping. Ignoring wrap = FALSE.", call. = FALSE)
    wrap <- TRUE
  } else if (!is_proportion && length(vars) > 1L && length(groups) > 1L && !wrap) {
    warning("Ignoring wrap = FALSE when plotting multiple variables and groups.", call. = FALSE)
    wrap <- TRUE
  }

  # Always set wrap to FALSE when dealing with a single variable and <= 1 group.
  if (!length(vars) > 1L && !length(groups) > 1L && wrap) {
    warning("Ignoring wrap = TRUE when plotting a single variable and <= 1 group.", call. = FALSE)
    wrap <- FALSE
  }

  # Use groups as the x-axis if it contains more unique values than the proportions
  # being plotted, or the number of variables being plotted for numeric plots.
  if (grouped) {
    n_group <- df[[group]]
    n_group <- length(levels(n_group) %||% unique(n_group))
    n_xaxis <- df[[xaxis]]
    n_xaxis <- length(levels(n_xaxis) %||% unique(n_xaxis))
    if (n_group > n_xaxis) {
      # Make xaxis the primary group (variables/values) and place group on xaxis instead.
      group <- if (is_proportion || length(vars) > 1L) xaxis else group
      xaxis <- head(groups, 1L)
    }
  } else if (is_proportion && length(vars) > 1L) {
    # Group by variable if we have multiple proportions and no groups
    group <- "variable"
  }


  # Wrap will always be FALSE if there is not multiple groups or variables.
  # If there are not multiple variables, we want to wrap by the second group.
  if (wrap) {
    if (length(vars) > 1L) {
      wrapv <- "variable"
      group <- if (is_proportion) "value" else tail(groups, 1L)
    } else {
      wrapv <- tail(groups, 1L)
      if (is_proportion) group <- "value"
    }
  }

  # Make sure we are not grouping by the the same variable as we have on the
  # xaxis, or that we are wrapping by.
  group <- setdiff(group, c(xaxis, wrapv))
  if (!length(group)) group <- NULL

  # Determine yaxis limits for proportions/scores
  ymin <- if (is_proportion) 0L else min(df$value, na.rm = TRUE) * 0.75
  ymax <- if (is_proportion) 1.05 else max(df$value, na.rm = TRUE) * 1.2

  # Create the plot and add geom_bar -------------------------------------------
  out <- ggplot2::ggplot(
    data = df, ggplot2::aes_string(
      x = xaxis,
      y = yaxis,
      ymin = ymin,
      ymax = ymax,
      group = group,
      fill  = group)) +

    ggplot2::geom_bar(
      stat = "identity",
      width = .5,
      position = ggplot2::position_dodge(width = .6))

  # Add labels -----------------------------------------------------------------
  # (Round percentages to a whole number, keep 1 decimal for numeric)
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

  # When wrapping, use the last group or xaxis
  if (wrap) {
    out <- out + ggplot2::facet_wrap(wrapv, ncol = 2L, scales = "free_x")
  }

  out

}