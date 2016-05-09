
<!-- README.md is generated from README.Rmd. Please edit that file -->
tabulR
------

[![Linux/OSX](https://travis-ci.org/itsdalmo/tabulR.svg?branch=master)](https://travis-ci.org/itsdalmo/tabulR) [![Windows](https://ci.appveyor.com/api/projects/status/github/itsdalmo/tabulR?branch=master&svg=true)](https://ci.appveyor.com/project/itsdalmo/tabulR) [![Coverage](http://codecov.io/github/itsdalmo/tabulR/coverage.svg?branch=master)](http://codecov.io/github/itsdalmo/tabulR?branch=master)

tabulR includes just two functions `qtable()` and `qtable_()`, which are meant to generate a "quick" table using [data.table](https://github.com/Rdatatable/data.table), and aims to balance information and readability in the output. `qtable()` is meant to be used with dplyr and it's NSE select statements (but uses the same underlying code as `qtable_()`).

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

`qtable` takes one or more variables and returns the following, dep. on type:

-   `numeric`: The count and weighted mean.
-   `factor`/\``character`: Count and weighted proportions.
-   `date` (including POSIX): Count and Min/Max dates.

`qtable` always does the following:

-   Returns the count for each variable/group (excluding NA's). Multiple counts are separated with **/**.
-   Only proportions and means are weighted, the counts are always netural.
-   It completes the data to make implicit missing values *explicit*.
-   When completing the data, counts are set to `0`, proportions to `0` and means to `NA`.
-   By default, it casts the data to a wide format when deemed appropriate (usually using the last group in `groups`).
-   It also includes a margin (named *Total*) for each column by default.

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
#### dtable

This is not really a NSE version of `qtable`. It also uses dplyr's `select_vars` to subset the data, so that we can use e.g. `one_of()`.

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
