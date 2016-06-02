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

# NUMERIC ----------------------------------------------------------------------

# Plotting >=1 numeric variable with 0 groups
# x: variable
# legend: none
# fill: none

df %>% bar_chart(num1)
df %>% bar_chart(num1:num2)

# Plotting >1 numeric variable with 0 groups (wrap = TRUE)
# x: variable
# legend: none
# fill: none
# wrap: variable

df %>% bar_chart(num1:num2, wrap = TRUE)

# Plotting 1 numeric variable with 1 group
# x: group
# legend: none

df %>% group_by(group) %>% bar_chart(num1)

# Plotting > 1 numeric variable with 1 group
# x: group
# legend: variable (num1, num2)
# fill: variable (num1, num2)

df %>% group_by(group) %>% bar_chart(num1:num2)

# Plotting > 1 numeric variable with 1 group (wrap = TRUE)
# x: group
# legend: none
# fill: none
# Wrap: variable

df %>% group_by(group) %>% bar_chart(num1:num2, wrap = TRUE)

# Plotting 1 numeric variable with >1 group
# x: group
# legend: fct
# fill: fct
# Wrap: none

df %>% group_by(group, fct) %>% bar_chart(num1)

# Plotting 1 numeric variable with >1 group (wrap = TRUE)
# x: group
# legend: none
# fill: none
# Wrap: fct

df %>% group_by(group, fct) %>% bar_chart(num1, wrap = TRUE)

# Plotting > 1 numeric variable with > 1 group
# x: group
# legend: fct (Yes, no, don't know)
# fill: fct (Yes, no, don't know)
# Wrap: variable (num1, num2)

df %>% group_by(group, fct) %>% bar_chart(num1:num2)

# FACTOR -----------------------------------------------------------------------

# Add another factor variable with identical levels to the data (multiple vars)
df$fct2 <- factor(rbinom(100, 2, .3), labels = c("Yes", "No", "Don't know"))

# Plotting 1 factor variable with 0 groups
# x: value (Yes/No/Don't know)
# legend: none
# fill: none

df %>% bar_chart(fct)

# Plotting > 1 factor variable with 0 groups
# x: value (Yes/No/Don't know)
# legend: variable (fct1, fct2)
# fill: variable (fct1, fct2)

df %>% bar_chart(fct, fct2)

# Plotting > 1 factor variable with 0 groups (wrap = TRUE)
# x: value (Yes/No/Don't know)
# legend: none
# fill: none
# wrap: variable (fct1, fct2)

df %>% bar_chart(fct, fct2, wrap = TRUE)

# Plotting 1 factor variable with 1 group
# x: group (Group A to D)
# legend: value (Yes/No/Don't know)
# fill: value (Yes/No/Don't know)

df %>% group_by(group) %>% bar_chart(fct)

# Plotting > 1 factor variable with 1 group
# x: group (Group A to D)
# legend: value (Yes/No/Don't know)
# fill: value (Yes/No/Don't know)
# wrap: variable (fct1, fct2)

df %>% group_by(group) %>% bar_chart(fct, fct2)

# Plotting 1 factor variable with >1 group
# x: group (Group A to D)
# legend: value (Yes/No/Don't know)
# fill: value (Yes/No/Don't know)
# wrap: variable (fct1, fct2)

df %>% group_by(group, dum) %>% bar_chart(fct)

