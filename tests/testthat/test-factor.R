context("factor/character columns")

df <- data.frame(
  group = factor(paste("Group", LETTERS[1:3]), levels = paste("Group", LETTERS[1:4])),
  fct = factor(c("No", "Yes", NA), levels = c("Yes", "No", "Don't know")),
  chr = paste("Option", c(1, 2, 2)),
  chr2 = paste("Test", c(1, 2, 3)),
  weight = c(1, 2, 2),
  stringsAsFactors = FALSE
)

test_that("Error for mixed columns", {
  expect_error(x <- qtable(df, vars = c("chr", "fct")), "mixed variable types.")
})

test_that("proportions for a single factor (single variable ignores wide = TRUE)", {
  x <- qtable(df, vars = "fct")
  expect_identical(x$n, c(1L, 1L, 0L))
  expect_identical(as.character(x$value), c("Yes", "No", "Don't know"))
  expect_identical(round(x$proportion, digits = 3), c(.5, .5, 0.0))
})

test_that("proportions for a single character", {
  x <- qtable(df, vars = "chr")
  expect_identical(names(x), c("variable", "value", "n", "proportion"))
  expect_identical(x$n, c(1L, 2L))
  expect_identical(round(x$proportion[1L], digits = 3), .333)
  expect_identical(round(x$proportion[2L], digits = 3), .667)
})

test_that("multiple factors require identical levels", {
  x <- df; x$chr <- as.factor(x$chr)
  expect_error(x <- qtable(x, vars = c("fct", "chr")), "identical levels")
  expect_error(x <- qtable(x, vars = c("fct", "chr"), wide = FALSE), "identical levels")
})

test_that("Error for multiple character columns", {
  expect_error(x <- qtable(df, vars = c("chr", "chr2")), "multiple variables")
})

test_that("proportions for a grouped factor", {
  x <- qtable(df, vars = "fct", groups = "group")
  expect_identical(x$n, c(1L, 1L, 0L, 0L, 2L))
  expect_identical(round(x$Yes, digits = 2), c(0, 1, 0, 0, .5))
  expect_identical(round(x$No, digits = 2), c(1, 0, 0, 0, .5))
  expect_identical(round(x$`Don't know`, digits = 2), c(0, 0, 0, 0, 0))

})

test_that("Proportion without margin", {
  x <- qtable(df, vars = "fct", groups = "group", margin = FALSE)
  expect_identical(x$n, c(1L, 1L, 0L, 0L))
  expect_identical(round(x$Yes, digits = 2), c(0, 1, 0, 0))
  expect_identical(round(x$No, digits = 2), c(1, 0, 0, 0))
  expect_identical(round(x$`Don't know`, digits = 2), c(0, 0, 0, 0))
})

test_that("weighted proportions", {
  x <- qtable(df, vars = "chr", weight = "weight")
  expect_identical(x$n, c(1L, 2L))
  expect_identical(names(x), c("variable", "value", "n", "proportion"))
  expect_identical(x$value, c("Option 1", "Option 2"))
  expect_identical(x$proportion, c(0.2, 0.8))
})

test_that("weighted proportions in groups", {
  x <- qtable(df, vars = "chr", groups = "fct", weight = "weight")
  expect_identical(x$n, c(1L, 1L, 0L, 3L))
  expect_identical(as.character(x$fct), c("Yes", "No", "Don't know", "Total"))
  expect_identical(x$`Option 1`, c(0, 1, 0, .2))
  expect_identical(x$`Option 2`, c(1, 0, 0, 0.8))
})