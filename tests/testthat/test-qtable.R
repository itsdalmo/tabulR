context("qtable (quick table)")

# TODO: Any general tests?
set.seed(100L)
df <- data.frame(
  group = factor(paste("Group", LETTERS[1:3]), levels = paste("Group", LETTERS[1:4])),
  fct = factor(c("No", "Yes", NA), levels = c("Yes", "No", "Don't know")),
  num = runif(3, 0, 100),
  stringsAsFactors = FALSE
)

test_that("qtable and dtable gives same output", {
  skip_if_not_installed("dplyr")
  dt <- df %>% dplyr::group_by(group) %>% dtable(one_of("num"))
  qt <- qtable(df, vars = "num", groups = "group")
  expect_identical(dt, qt)
})