context("bar_chart")

set.seed(100L)
df <- data.frame(
  group = factor(rbinom(100, 4, .2), labels = paste("Group", LETTERS[1:4])),
  fct = factor(rbinom(100, 2, .3), labels = c("Yes", "No", "Don't know")),
  fct2 = factor(rbinom(100, 2, .3), labels = c("Yes", "No", "Don't know")),
  dum = factor(rbinom(100, 1, .5), labels = c("Answer A", "Answer B")),
  num1 = runif(100, 0, 100),
  num2 = runif(100, 0, 100),
  stringsAsFactors = FALSE
)

test_that("bar_chart visual tests for numeric work", {
  p <- bar_chart_(df, vars = "num1")
  p <- bar_chart_(df, vars = c("num1", "num2"))
  p <- bar_chart_(df, vars = c("num1", "num2"), wrap = TRUE)

  p <- bar_chart_(df, vars = "num1", groups = "group")
  p <- bar_chart_(df, vars = c("num1", "num2"), groups = "group")
  p <- bar_chart_(df, vars = c("num1", "num2"), groups = "group", wrap = TRUE)

  p <- bar_chart_(df, vars = "num1", groups = c("group", "fct"))
  p <- bar_chart_(df, vars = "num1", groups = c("group", "fct"), wrap = TRUE)
  p <- bar_chart_(df, vars = c("num1", "num2"), groups = c("group", "fct"), wrap = TRUE)
})

test_that("bar_chart visual tests for proportions work", {
  p <- bar_chart_(df, vars = "fct")
  p <- bar_chart_(df, vars = c("fct", "fct2"))
  p <- bar_chart_(df, vars = c("fct", "fct2"), wrap = TRUE)

  p <- bar_chart_(df, vars = "fct", groups = "group")
  p <- bar_chart_(df, vars = c("fct", "fct2"), groups = "group", wrap = TRUE)
  p <- bar_chart_(df, vars = "fct", groups = c("group", "dum"), wrap = TRUE)
})

test_that("bar_chart and bar_chart_ gives same output", {
  skip_if_not_installed("dplyr")
  dt <- df %>% dplyr::group_by(group) %>% bar_chart(dplyr::one_of("num1"))
  qt <- bar_chart_(df, vars = "num1", groups = "group")
  expect_equal(dt, qt)
})

test_that("bar_chart errors when call cannot be plotted", {
  expect_error(
    qt <- bar_chart_(df, vars = c("fct", "fct2"), groups = c("group", "dum")),
    "Cannot plot multiple proportions with multiple groups."
  )
})

test_that("bar_chart ignores wrap = TRUE when necessary", {
  expect_warning(
    qt <- bar_chart_(df, vars = "num1", wrap = TRUE),
    "Ignoring wrap = TRUE"
  )
  expect_warning(
    qt <- bar_chart_(df, vars = "num1", groups = "group", wrap = TRUE),
    "Ignoring wrap = TRUE"
  )
})

test_that("bar_chart ignores wrap = FALSE when necessary", {
  expect_warning(
    qt <- bar_chart_(df, vars = c("num1", "num2"), groups = c("group", "fct"), wrap = FALSE),
    "Ignoring wrap = FALSE"
  )
  expect_warning(
    qt <- bar_chart_(df, vars = "fct", groups = c("group", "dum"), wrap = FALSE),
    "Ignoring wrap = FALSE"
  )
})
