context("date columns")

df <- data.frame(
  group = factor(paste("Group", LETTERS[1:3]), levels = paste("Group", LETTERS[1:4])),
  fct = factor(c("No", "Yes", NA), levels = c("Yes", "No", "Don't know")),
  date = seq(as.Date("2016/1/1"), by = "month", length.out = 3),
  date2 = seq(as.Date("2016/1/1"), by = "day", length.out = 3),
  weight = c(1, 2, 2),
  stringsAsFactors = FALSE
)

test_that("Error for mixed columns", {
  expect_error(x <- qtable(df, vars = c("date", "fct")), "mixed variable types.")
})

test_that("min/max for a single date", {
  x <- qtable(df, vars = "date")
  expect_identical(x$n, 3L)
  expect_identical(x$min, as.Date("2016/01/01"))
  expect_identical(x$max, as.Date("2016/03/01"))
})

test_that("min/max for a multiple dates (error)", {
  expect_error(x <- qtable(df, vars = c("date", "date2")), "multiple variables")
})

test_that("min/max for a single grouped date", {
  x <- qtable(df, vars = "date", groups = "group")
  expect_identical(x$n, c(1L, 1L, 1L, 0L, 3L))
  expect_identical(as.character(x$group), c(paste("Group", LETTERS[1:4]), "Total"))
  expect_identical(x$min, c(df$date, NA, as.Date("2016/01/01")))
  expect_identical(x$max, c(df$date, NA, as.Date("2016/03/01")))
})

test_that("min/max for a single date with multiple groups", {
  x <- qtable(df, vars = "date", groups = c("group", "fct"))
  expect_identical(dim(x), c(15L, 5L))
})

test_that("min/max for a single date with multiple groups", {
  x <- qtable(df, vars = "date", groups = c("group", "fct"), wide = FALSE)
  expect_identical(names(x), c("group", "fct", "type", "value"))
})
