context("bar_chart and line_chart")

set.seed(100L)
df <- data.frame(
  group = factor(paste("Group", LETTERS[1:3]), levels = paste("Group", LETTERS[1:4])),
  fct = factor(c("No", "Yes", NA), levels = c("Yes", "No", "Don't know")),
  num = runif(3, 0, 100),
  stringsAsFactors = FALSE
)

test_that("bar_chart and bar_chart_ gives same output", {
  skip_if_not_installed("dplyr")
  dt <- df %>% dplyr::group_by(group) %>% bar_chart(one_of("num"))
  qt <- bar_chart_(df, vars = "num", groups = "group")
  expect_equal(dt, qt)
})

test_that("line_chart and line_chart_ gives same output", {
  skip_if_not_installed("dplyr")
  dt <- df %>% dplyr::group_by(group) %>% line_chart(one_of("num"))
  qt <- line_chart_(df, vars = "num", groups = "group")
  expect_equal(dt, qt)
})

# TODO - GENERAL TESTS