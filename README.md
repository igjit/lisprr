# lisprr

A toy Lisp interpreter in R

## Installation

You can install lisprr from github with:


``` r
# install.packages("devtools")
devtools::install_github("igjit/lisprr")
```

## How to play

### evaluate

```r
lisprr::evaluate("(+ 1 2)")
#> [1] 3
```

```r
lisprr::evaluate("(plot (: 1 10))")
```

### translate

```r
lisprr::translate("(+ 1 2)")
#> 1 + 2
```

```r
lisprr::translate("(define add2 (lambda (x) (+ x 2)))")
#> .Primitive("<-")(add2, function(x = NULL) {
#>     x + 2
#> })
```

### repl

```lisp
> lisprr::repl()
lisprr> (: 1 10)
1 2 3 4 5 6 7 8 9 10
lisprr> (define add2 (lambda (x) (+ x 2)))
#<closure>
lisprr> (add2 40)
42
lisprr> (plot iris)

lisprr> q
bye.
>
```
