% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qtable.R
\name{qtable}
\alias{qtable}
\title{qtable}
\usage{
qtable(df, ..., groups = NULL, weight = NULL, margin = TRUE,
  margin_name = NULL, wide = TRUE)
}
\arguments{
\item{df}{A \code{data.frame} or \code{data.table}.}

\item{...}{Unquoted variable names passed to \code{\link[dplyr]{select}}.}

\item{groups}{Variables to group by during aggregation.}

\item{weight}{Name of a variable to weight by. Only used when \code{margin = TRUE}.}

\item{margin}{If \code{TRUE} (the default), the first group will include a "Total".}

\item{margin_name}{Optional: Give the margin a different name from "Total".}

\item{wide}{Should a long or a wide table be returned? Wide tables spread levels for
\code{factor} and unique values for \code{character}. For a single \code{numeric},
the last group is used, while multiple \code{numeric} will be spread by variable names.}
}
\value{
Same as \code{\link{qtable}}.
}
\description{
Same as \code{\link{qtable_}}, except designed for use with \code{dplyr}. It takes
unquoted variable names and supports all dplyr select statements, except renaming in
the select statement itself. \code{qtable} also looks for \code{dplyr} groups if
they are not specified.
}
\details{
Note that \code{groups} and \code{weight} still have to be character vectors.
}
\examples{
# TODO
}
\author{
Kristian D. Olsen
}

