
<!-- README.md is generated from README.Rmd. Please edit that file -->
tabulR
------

[![Build Status](https://travis-ci.org/itsdalmo/tabulR.svg?branch=master)](https://travis-ci.org/itsdalmo/tabulR) [![codecov.io](http://codecov.io/github/itsdalmo/tabulR/coverage.svg?branch=master)](http://codecov.io/github/itsdalmo/tabulR?branch=master)

tabulR includes just one function `qtable()`, which is meant to generate a "quick" table using [data.table](https://github.com/Rdatatable/data.table), and aims to balance information and readability in the output.

Note: This is a work in progress.

Installation
------------

#### Dependencies ahead of CRAN

-   [data.table](https://github.com/Rdatatable/data.table) 1.9.7: Lets us use `drop = FALSE` for only RHS of formula in `dcast`.

``` r
# Install development version of data.table
install.packages("data.table", type = "source", repos = "https://Rdatatable.github.io/data.table")
```

#### Install tabulR

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

TODO...
