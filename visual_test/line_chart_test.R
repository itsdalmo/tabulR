library(dplyr)
library(devtools)
load_all()

rm(list = ls(all = TRUE))
set.seed(100L)
df <- data.frame(
  group = factor(rbinom(100, 4, .2), labels = paste("Group", LETTERS[1:4])),
  fct = factor(rbinom(100, 2, .3), labels = c("Yes", "No", "Don't know")),
  dum = factor(rbinom(100, 1, .5), labels = c("Answer A", "Answer B")),
  num1 = runif(100, 0, 100),
  num2 = runif(100, 0, 100),
  stringsAsFactors = FALSE
)

# Plotting 1 variable with 2 groups
# x: fct (Yes/No/Don't know)
# legend: group

df %>% group_by(group, fct) %>% line_chart(num1)

# Plotting > 1 variable with no groups
# x: variable (num1, num2)
# legend: none

df %>% line_chart(num1:num2)

# Plotting > 1 variable with 1 group
# x: variable (num1, num2)
# legend: group

df %>% group_by(group) %>% line_chart(num1:num2)

# Plotting > 1 variable with 2 groups
# x: variable
# legend: group
# wrap: fct (overrides wrap = FALSE)

df %>% group_by(group, fct) %>% line_chart(num1:num2)
