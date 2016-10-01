context("qtable (quick table)")

# TODO: Any general tests?
set.seed(100L)
df <- data.frame(
  group = factor(paste("Group", LETTERS[1:3]), levels = paste("Group", LETTERS[1:4])),
  fct = factor(c("No", "Yes", NA), levels = c("Yes", "No", "Don't know")),
  num = runif(3, 0, 100),
  stringsAsFactors = FALSE
)

test_that("qtable and qtable_ gives same output", {
  skip_if_not_installed("dplyr")
  dt <- df %>% dplyr::group_by(group) %>% qtable(dplyr::one_of("num"))
  qt <- qtable_(df, vars = "num", groups = "group")
  expect_identical(dt, qt)
})

test_that("qtable supports renaming variables in call", {
  skip_if_not_installed("dplyr")
  dt <- df %>% dplyr::group_by(group) %>% qtable(score = num)
  qt <- df %>% dplyr::group_by(group) %>% qtable(num)
  names(qt)[3] <- "score"
  expect_identical(dt, qt)
})
