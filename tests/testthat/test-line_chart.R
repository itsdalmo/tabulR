context("line_chart")

set.seed(100L)
df <- data.frame(
  group = factor(rbinom(100, 4, .2), labels = paste("Group", LETTERS[1:4])),
  fct = factor(rbinom(100, 2, .3), labels = c("Yes", "No", "Don't know")),
  dum = factor(rbinom(100, 1, .5), labels = c("Answer A", "Answer B")),
  num1 = runif(100, 0, 100),
  num2 = runif(100, 0, 100),
  stringsAsFactors = FALSE
)

test_that("line_chart visual tests work", {
  p <- line_chart_(df, vars = "num1", groups = c("group", "fct"))
  p <- line_chart_(df, vars = c("num1", "num2"))
  p <- line_chart_(df, vars = c("num1", "num2"), groups = "group")
  p <- line_chart_(df, vars = c("num1", "num2"), groups = c("group", "fct"), wrap = TRUE)
})

test_that("line_chart and line_chart_ gives same output", {
  skip_if_not_installed("dplyr")
  dt <- df %>% dplyr::group_by(group) %>% line_chart(dplyr::one_of("num1", "num2"))
  qt <- line_chart_(df, vars = c("num1", "num2"), groups = "group")
  expect_equal(dt, qt)
})

test_that("line_chart errors when call results in no lines being drawn", {
  expect_error(
    qt <- line_chart_(df, vars = "num1"),
    "Cannot create line_chart with a single variable and <= 1 groups"
  )
  expect_error(
    qt <- line_chart_(df, vars = "num1", groups = "group"),
    "Cannot create line_chart with a single variable and <= 1 groups"
  )
})

test_that("line_chart ignores wrap = TRUE when necessary", {
  expect_warning(
    qt <- line_chart_(df, vars = c("num1", "num2"), groups = NULL, wrap = TRUE),
    "Ignoring wrap = TRUE"
  )
  expect_warning(
    qt <- line_chart_(df, vars = "num1", groups = c("group", "fct"), wrap = TRUE),
    "Ignoring wrap = TRUE"
  )
})

test_that("line_chart ignores wrap = FALSE when necessary", {
  expect_warning(
    qt <- line_chart_(df, vars = c("num1", "num2"), groups = c("group", "fct"), wrap = FALSE),
    "Ignoring wrap = FALSE"
  )
})
