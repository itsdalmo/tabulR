context("numeric columns")

set.seed(100L)
df <- data.frame(
  grp = factor(c(rep(c("gA", "gB"), 5), rep("gC", 10))),
  grp2 = rep(c("g2a", "g2b"), 10),
  int = runif(20, 1, 10),
  num = runif(20, 0, 100),
  fct = factor(rep(c("fA", "fB", "fC"), 10)[1:20L]),
  chr = c(rep(c("cA", "cB"), 5), rep("cC", 10)),
  w = c(rep(1L, 10), rep(2L, 10)),
  stringsAsFactors = FALSE
)

# TODO
test_that("single numeric", {
  x <- qtable(df, vars = "int")
  expect_equal(dim(x), c(1, 2))
  expect_equal(names(x), c("n", "int"))
})

test_that("single numeric with groups", {
  x <- qtable(df, vars = "int", groups = "grp")
  expect_equal(dim(x), c(4, 3))
  expect_equal(names(x), c("grp", "n", "int"))

  x <- qtable(df, vars = "int", groups = c("grp", "grp2"))
  expect_equal(dim(x), c(4, 4)) # Will fail when complete_df is done.
  expect_equal(names(x), c("grp", "n", "g2a", "g2b"))
})

test_that("multiple numerics", {
  x <- qtable(df, vars = c("int", "num"))
  expect_equal(dim(x), c(1,3))
  expect_equal(names(x), c("n", "int", "num"))
})

test_that("multiple numerics with groups", {
  x <- qtable(df, vars = c("int", "num"), groups = "grp")
  expect_equal(dim(x), c(4,4))
  expect_equal(names(x), c("grp", "n", "int", "num"))
})