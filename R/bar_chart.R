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
  is_grouped <- !is.null(groups)
  is_proportion <- "proportion" %in% names(df)

  mult_var <- length(vars) > 1L
  mult_grp <- length(groups) > 1L

  # Give informative warnings when input cannot be plotted. Differs between
  # proportions and numerics, since proportions also need to indicate "value".
  if (is_proportion && mult_grp && mult_var) {
    stop("Cannot plot multiple proportions with multiple groups.", call. = FALSE)
  } else if (is_proportion && mult_var && is_grouped && !wrap) {
    warning("Plotting multiple proportions with groups requires wrapping. Ignoring wrap = FALSE.", call. = FALSE)
    wrap <- TRUE
  } else if (is_proportion && mult_grp && !wrap) {
    warning("Plotting a proportion with multiple groups requires wrapping. Ignoring wrap = FALSE.", call. = FALSE)
    wrap <- TRUE
  } else if (!is_proportion && mult_var && mult_grp && !wrap) {
    warning("Ignoring wrap = FALSE when plotting multiple variables and groups.", call. = FALSE)
    wrap <- TRUE
  }
  # Always set wrap to FALSE when dealing with a single variable and <= 1 group.
  if (!mult_var && !mult_grp && wrap) {
    warning("Ignoring wrap = TRUE when plotting a single variable and <= 1 group.", call. = FALSE)
    wrap <- FALSE
  }

  # Use groups as the x-axis if it contains more unique values than the
  # proportions being plotted, or the number of variables being plotted for numeric plots.
  var <- if (is_proportion) "value" else "variable"
  if (is_grouped) {
    gvar <- df[[head(groups, 1L)]]
    glength <- length(levels(gvar) %||% unique(gvar))
    xvar <- df[[var]]
    xlength <- length(levels(xvar) %||% unique(xvar))
    if (glength > xlength) {
      org <- var; var <- head(groups, 1L);
      groups <- c(org, tail(groups, -1L))
    }
  }

  # Figure out the best aesthetics for visualizing the plot.
  # (Depends on: proportion/numeric, grouped or not, single/mult. vars, wrap etc.)
  if (is_proportion) {
    xvar  <- var
    yvar  <- "proportion"
    ymin  <- 0L
    ymax  <- 1.05
    group <- if (is_grouped) head(groups, 1L) else if (!is_grouped && mult_var && !wrap) "variable" else NULL
    fill  <- if (is_grouped) head(groups, 1L) else if (!is_grouped && mult_var && !wrap) "variable" else NULL
  } else {
    xvar  <- var
    yvar  <- "value"
    ymin  <- min(df$value, na.rm = TRUE) * 0.75
    ymax  <- max(df$value, na.rm = TRUE) * 1.2
    group <- if ((mult_var || mult_grp) && is_grouped && (!wrap || mult_var && mult_grp)) tail(groups, 1L) else NULL
    fill  <- if ((mult_var || mult_grp) && is_grouped && (!wrap || mult_var && mult_grp)) tail(groups, 1L) else NULL
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
    if (is_proportion) {
      wby <- if (mult_var) "variable" else tail(groups, 1L)
    } else {
      wby <- if (mult_var) "variable" else tail(groups, 1L)
    }
    out <- out + ggplot2::facet_wrap(wby, ncol = 2L, scales="free_x")
  }

  out

}