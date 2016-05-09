context("numeric columns")

set.seed(1000L)
df <- data.frame(
  group = factor(paste("Group", LETTERS[1:3]), levels = paste("Group", LETTERS[1:4])),
  fct = factor(c("No", "Yes", NA), levels = c("Yes", "No", "Don't know")),
  int = as.integer(runif(3, 1, 10)),
  num = runif(3, 0, 100),
  weight = c(1, 2, 2),
  stringsAsFactors = FALSE
)

test_that("Error for mixed columns", {
  expect_error(x <- qtable_(df, vars = c("int", "fct")), "mixed variable types.")
})

test_that("mean for a single numeric", {
  x <- qtable_(df, vars = "num")
  expect_identical(x$n, 3L)
  expect_identical(round(x$num, digits = 3), 42.497)
})

test_that("means for multiple numerics", {
  x <- qtable_(df, vars = c("num", "int"))
  expect_identical(x$n, 3L)
  expect_identical(round(x$num, digits = 3), 42.497)
  expect_identical(x$int, 4)
})

test_that("means for numeric by group", {
  x <- qtable_(df, vars = "num", groups = "group")
  expect_identical(as.character(x$group), c(paste("Group", LETTERS[1:4]), "Total"))
  expect_identical(x$n, c(1L, 1L, 1L, 0L, 3L))
  expect_identical(round(x$num, 1), c(69.1, 51.6, 6.8, NA, 42.5))
})

test_that("means for multiple numerics by group", {
  x <- qtable_(df, vars = c("int", "num"), groups = "group")
  expect_identical(as.character(x$group), c(paste("Group", LETTERS[1:4]), "Total"))
  expect_identical(x$n, c("1/1", "1/1", "1/1", "0/0", "3/3"))
  expect_identical(x$int, c(3, 7, 2, NA, 4))
  expect_identical(round(x$num, 1), c(69.1, 51.6, 6.8, NA, 42.5))
})

test_that("means for single numeric by multiple groups", {
  x <- qtable_(df, vars = "num", groups = c("group", "fct"))
  expect_identical(as.character(x$group), c(paste("Group", LETTERS[1:4]), "Total"))
  expect_identical(x$n, c("0/1/0", "1/0/0", "0/0/0", "0/0/0", "1/1/0"))
  expect_identical(round(x$Yes, 1), c(NA, 51.6, NA, NA, 51.6))
  expect_identical(round(x$No, 1), c(69.1, NA, NA, NA, 69.1))
  expect_identical(x$`Don't know`, as.numeric(c(NA, NA, NA, NA, NA)))
})

test_that("means for weighted numerics", {
  x <- qtable_(df, vars = c("int", "num"), groups = "group", weight = "weight")
  expect_identical(as.character(x$group), c(paste("Group", LETTERS[1:4]), "Total"))
  expect_identical(x$n, c("1/1", "1/1", "1/1", "0/0", "3/3"))
  expect_identical(x$int, c(3, 7, 2, NA, 4.2))
  expect_identical(round(x$num, 1), c(69.1, 51.6, 6.8, NA, 37.2))
})