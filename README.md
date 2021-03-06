
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lisprr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/igjit/lisprr.svg?branch=master)](https://travis-ci.org/igjit/lisprr)
[![Codecov test
coverage](https://codecov.io/gh/igjit/lisprr/branch/master/graph/badge.svg)](https://codecov.io/gh/igjit/lisprr?branch=master)
<!-- badges: end -->

A toy Lisp interpreter in R

## Installation

You can install lisprr from github with:

``` r
# install.packages("devtools")
devtools::install_github("igjit/lisprr")
```

## How to play

### evaluate

``` r
lisprr::evaluate("(+ 1 2)")
#> [1] 3
```

``` r
lisprr::evaluate("(plot (: 1 10))")
```

### translate

``` r
lisprr::translate("(+ 1 2)")
#> 1 + 2
```

``` r
lisprr::translate("(define (add2 x) (+ x 2))")
#> add2 <- function(x = NULL) {
#>     x + 2
#> }
```

### repl

``` lisp
> lisprr::repl()
lisprr> (: 1 10)
1 2 3 4 5 6 7 8 9 10
lisprr> (define (add2 x) (+ x 2))
#<closure>
lisprr> (add2 40)
42
lisprr> (plot iris)

lisprr> q
bye.
>
```
