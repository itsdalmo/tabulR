
<!-- README.md is generated from README.Rmd. Please edit that file -->
tabulR
------

[![Linux/OSX](https://travis-ci.org/itsdalmo/tabulR.svg?branch=master)](https://travis-ci.org/itsdalmo/tabulR) [![Windows](https://ci.appveyor.com/api/projects/status/github/itsdalmo/tabulR?branch=master&svg=true)](https://ci.appveyor.com/project/itsdalmo/tabulR) [![Coverage](http://codecov.io/github/itsdalmo/tabulR/coverage.svg?branch=master)](http://codecov.io/github/itsdalmo/tabulR?branch=master)

tabulR serves two purposes, to generate "quick" tables using `qtable()` and plots using `line_chart()` and `bar_chart()`.

Note: This is a work in progress.

Installation
------------

Development version:

``` r
devtools::install_github("itsdalmo/tabulR")
```

CRAN:

``` r
# Not on CRAN yet.
```

Usage
-----

`qtable` takes one or more variables and returns a table (made with [data.table](https://github.com/Rdatatable/data.table)) depending on type:

-   `numeric`: The count and weighted mean.
-   `factor`/`character`: Count and weighted proportions.
-   `date` (including POSIX): Count and Min/Max dates.

`qtable` (and charts by extension) always does the following:

-   Returns the count for each variable/group (excluding NA's). Multiple counts are separated with **/**.
-   Only proportions and means are weighted, the counts are always neutral.
-   It completes the data to make implicit missing values *explicit*.
-   When completing the data, missing counts, proportions and means are set to `0`, `0` and `NA` respectively.
-   By default, tables will be cast to a wide format when deemed appropriate (usually using the last group in `groups`).
-   Includes a margin (called *Total*) for each column by default, if the data is grouped.

`bar_chart` and `line_chart` also:

-   Maps and aggregates data to generate a plot (made with [ggplot2](https://github.com/hadley/ggplot2)).
-   Minimizes the number of legends when mapping groups and variables to aestetics.
-   Adds the appropriate geom's, including `geom_text()`.
-   Attempts to adjust the axis labels and range to improve readability.

``` r
require(tabulR)
set.seed(100L)
df <- data.frame(
  group = factor(paste("Group", LETTERS[1:3]), levels = paste("Group", LETTERS[1:4])),
  fct = factor(c("No", "Yes", NA), levels = c("Yes", "No", "Don't know")),
  num = runif(3, 0, 100),
  stringsAsFactors = FALSE
)
```

#### Numeric

``` r
out <- qtable_(df, vars = "num", groups = c("group", "fct"))
knitr::knit_print(out, digits = 1L)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
group
</th>
<th style="text-align:left;">
n
</th>
<th style="text-align:center;">
Yes
</th>
<th style="text-align:center;">
No
</th>
<th style="text-align:center;">
Don't know
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Group A
</td>
<td style="text-align:left;">
0/1/0
</td>
<td style="text-align:center;">
NA
</td>
<td style="text-align:center;">
30.8
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Group B
</td>
<td style="text-align:left;">
1/0/0
</td>
<td style="text-align:center;">
25.8
</td>
<td style="text-align:center;">
NA
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Group C
</td>
<td style="text-align:left;">
0/0/0
</td>
<td style="text-align:center;">
NA
</td>
<td style="text-align:center;">
NA
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Group D
</td>
<td style="text-align:left;">
0/0/0
</td>
<td style="text-align:center;">
NA
</td>
<td style="text-align:center;">
NA
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:left;">
1/1/0
</td>
<td style="text-align:center;">
25.8
</td>
<td style="text-align:center;">
30.8
</td>
<td style="text-align:center;">
NA
</td>
</tr>
</tbody>
</table>
#### Factor

``` r
out <- qtable_(df, vars = "fct", groups = "group")
knitr::knit_print(out, digits = 1L)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
group
</th>
<th style="text-align:center;">
n
</th>
<th style="text-align:center;">
Yes
</th>
<th style="text-align:center;">
No
</th>
<th style="text-align:center;">
Don't know
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Group A
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
0.0%
</td>
<td style="text-align:center;">
100.0%
</td>
<td style="text-align:center;">
0.0%
</td>
</tr>
<tr>
<td style="text-align:left;">
Group B
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
100.0%
</td>
<td style="text-align:center;">
0.0%
</td>
<td style="text-align:center;">
0.0%
</td>
</tr>
<tr>
<td style="text-align:left;">
Group C
</td>
<td style="text-align:center;">
0
</td>
<td style="text-align:center;">
0.0%
</td>
<td style="text-align:center;">
0.0%
</td>
<td style="text-align:center;">
0.0%
</td>
</tr>
<tr>
<td style="text-align:left;">
Group D
</td>
<td style="text-align:center;">
0
</td>
<td style="text-align:center;">
0.0%
</td>
<td style="text-align:center;">
0.0%
</td>
<td style="text-align:center;">
0.0%
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
50.0%
</td>
<td style="text-align:center;">
50.0%
</td>
<td style="text-align:center;">
0.0%
</td>
</tr>
</tbody>
</table>
#### Chart

``` r
out <- bar_chart_(df, vars = "num", groups = "group")
# knitr::knit_print(out)
```

NSE
---

`qtable` is not exactly a NSE version (non standard evaluation) of `qtable_`, it also requires dplyr to use. It expects unquoted arguments for `vars`, and supports `one_of()`, `starts_with()` etc. from dplyr. The same is true for the charts, such as `bar_chart` and `bar_chart_`.

#### Table

``` r
require(dplyr)
out <- df %>% group_by(group) %>% qtable(one_of("num"))
knitr::knit_print(out, digits = 1L)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
group
</th>
<th style="text-align:center;">
n
</th>
<th style="text-align:center;">
num
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Group A
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
30.8
</td>
</tr>
<tr>
<td style="text-align:left;">
Group B
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
25.8
</td>
</tr>
<tr>
<td style="text-align:left;">
Group C
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
55.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Group D
</td>
<td style="text-align:center;">
0
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
37.3
</td>
</tr>
</tbody>
</table>
#### Chart

``` r
require(dplyr)
out <- df %>% group_by(fct) %>% bar_chart(one_of("num"), margin = FALSE)
# knitr::knit_print(out)
```
