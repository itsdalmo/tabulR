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
  expect_error(x <- qtable(df, vars = c("chr", "fct")), "mixed classes")
})

test_that("proportions for a single factor", {
  x <- qtable(df, vars = "fct")
  expect_identical(x$n, 2L)
  expect_identical(round(x$Yes, digits = 3), .5)
  expect_identical(round(x$No, digits = 3), .5)
  expect_identical(round(x$`Don't know`, digits = 3), 0)
})

test_that("proportions for a single character", {
  x <- qtable(df, vars = "chr")
  expect_identical(names(x), c("n", "Option 1", "Option 2"))
  expect_identical(x$n, 3L)
  expect_identical(round(x$`Option 1`, digits = 3), .333)
  expect_identical(round(x$`Option 2`, digits = 3), .667)
})

test_that("proportions for multiple factors (only long table)", {
  x <- df; x$chr <- as.factor(x$chr)
  expect_error(x <- qtable(x, vars = c("fct", "chr")), "identical levels")
  x <- qtable(x, vars = c("fct", "chr"), wide = FALSE)
  expect_identical(as.character(x$variable), c("fct", "fct", "chr", "chr"))
  expect_identical(as.character(x$value), c("No", "Yes", "Option 1", "Option 2"))
  expect_identical(x$n, c(1L, 1L, 1L, 2L))
  expect_identical(round(x$proportion, digits = 2), c(.5, .5, .33, .67))
})

test_that("Error for multiple character columns", {
  expect_error(x <- qtable(df, vars = c("chr", "chr2")), "multiple variables")
})

test_that("proportions for a grouped factor", {
  x <- qtable(df, vars = "fct", groups = "group")
  expect_identical(x$n, c(1L, 1L, 2L))
  # TODO
  expect_identical(round(x$Yes, digits = 2), c(0, 1, .5))
  expect_identical(round(x$No, digits = 2), c(1, 0, .5))
  expect_identical(round(x$`Don't know`, digits = 2), c(0, 0, 0))

})

test_that("Proportion without margin", {
  x <- qtable(df, vars = "fct", groups = "group", margin = FALSE)
  expect_identical(x$n, c(1L, 1L))
  # TODO
  expect_identical(round(x$Yes, digits = 2), c(0, 1))
  expect_identical(round(x$No, digits = 2), c(1, 0))
  expect_identical(round(x$`Don't know`, digits = 2), c(0, 0))
})

test_that("weighted proportions", {
  x <- qtable(df, vars = "chr", weight = "weight")
  expect_identical(x$n, 3L)
  expect_identical(names(x), c("n", "Option 1", "Option 2"))
  expect_identical(x$`Option 1`, 0.2)
  expect_identical(x$`Option 2`, 0.8)
})

test_that("weighted proportions in groups", {
  x <- qtable(df, vars = "chr", groups = "fct", weight = "weight")
  # TODO
  expect_identical(x$n, c(1L, 1L, 3L))
  expect_identical(as.character(x$fct), c("Yes", "No", "Total"))
  expect_identical(x$`Option 1`, c(0, 1, .2))
  expect_identical(x$`Option 2`, c(1, 0, 0.8))
})