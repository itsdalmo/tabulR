
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
  group = factor(rbinom(100, 4, .2), labels = paste("Group", LETTERS[1:4])),
  fct = factor(rbinom(100, 2, .3), labels = c("Yes", "No", "Don't know")),
  dum = factor(rbinom(100, 1, .5), labels = c("Answer A", "Answer B")),
  num1 = runif(100, 0, 100),
  num2 = runif(100, 0, 100),
  stringsAsFactors = FALSE
)
```

#### Numeric

``` r
qtable_(df, vars = "num1", groups = c("group", "fct"))
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
22/14/2
</td>
<td style="text-align:center;">
65.0
</td>
<td style="text-align:center;">
51.5
</td>
<td style="text-align:center;">
38.8
</td>
</tr>
<tr>
<td style="text-align:left;">
Group B
</td>
<td style="text-align:left;">
21/19/5
</td>
<td style="text-align:center;">
46.9
</td>
<td style="text-align:center;">
47.5
</td>
<td style="text-align:center;">
54.7
</td>
</tr>
<tr>
<td style="text-align:left;">
Group C
</td>
<td style="text-align:left;">
5/6/4
</td>
<td style="text-align:center;">
55.2
</td>
<td style="text-align:center;">
46.1
</td>
<td style="text-align:center;">
34.7
</td>
</tr>
<tr>
<td style="text-align:left;">
Group D
</td>
<td style="text-align:left;">
1/1/0
</td>
<td style="text-align:center;">
80.1
</td>
<td style="text-align:center;">
19.7
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
49/40/11
</td>
<td style="text-align:center;">
56.6
</td>
<td style="text-align:center;">
48.0
</td>
<td style="text-align:center;">
44.5
</td>
</tr>
</tbody>
</table>
#### Factor

``` r
qtable_(df, vars = "fct", groups = "group")
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
38
</td>
<td style="text-align:center;">
57.9%
</td>
<td style="text-align:center;">
36.8%
</td>
<td style="text-align:center;">
5.3%
</td>
</tr>
<tr>
<td style="text-align:left;">
Group B
</td>
<td style="text-align:center;">
45
</td>
<td style="text-align:center;">
46.7%
</td>
<td style="text-align:center;">
42.2%
</td>
<td style="text-align:center;">
11.1%
</td>
</tr>
<tr>
<td style="text-align:left;">
Group C
</td>
<td style="text-align:center;">
15
</td>
<td style="text-align:center;">
33.3%
</td>
<td style="text-align:center;">
40.0%
</td>
<td style="text-align:center;">
26.7%
</td>
</tr>
<tr>
<td style="text-align:left;">
Group D
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
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:center;">
100
</td>
<td style="text-align:center;">
49.0%
</td>
<td style="text-align:center;">
40.0%
</td>
<td style="text-align:center;">
11.0%
</td>
</tr>
</tbody>
</table>
#### Chart

``` r
out <- bar_chart_(df, vars = "num1", groups = "group")
```

NSE
---

`qtable` is not exactly a NSE version (non standard evaluation) of `qtable_`, it also requires dplyr to use. It expects unquoted arguments for `vars`, and supports `one_of()`, `starts_with()` etc. from dplyr. The same is true for the charts, such as `bar_chart` and `bar_chart_`.

#### Table

``` r
require(dplyr)
df %>% group_by(group) %>% qtable(starts_with("num"))
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
num1
</th>
<th style="text-align:center;">
num2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Group A
</td>
<td style="text-align:center;">
38
</td>
<td style="text-align:center;">
58.6
</td>
<td style="text-align:center;">
51.8
</td>
</tr>
<tr>
<td style="text-align:left;">
Group B
</td>
<td style="text-align:center;">
45
</td>
<td style="text-align:center;">
48.0
</td>
<td style="text-align:center;">
50.1
</td>
</tr>
<tr>
<td style="text-align:left;">
Group C
</td>
<td style="text-align:center;">
15
</td>
<td style="text-align:center;">
46.1
</td>
<td style="text-align:center;">
57.6
</td>
</tr>
<tr>
<td style="text-align:left;">
Group D
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
49.9
</td>
<td style="text-align:center;">
24.1
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:center;">
100
</td>
<td style="text-align:center;">
51.8
</td>
<td style="text-align:center;">
51.4
</td>
</tr>
</tbody>
</table>
#### Chart

``` r
require(dplyr)
out <- df %>% group_by(group) %>% bar_chart(starts_with("num"), margin = FALSE)
```
