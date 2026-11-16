# Standard errors

This function computes the standard error (i.e. `sd / sqrt(n)`) of the
values in `x`. If `na.rm` is `TRUE` then missing values are removed
before computation proceeds.

## Usage

``` r
se(x, na.rm = FALSE)
```

## Source

Original URL: <https://cran.r-project.org/doc/manuals/R-intro.html>

## Arguments

- x:

  A numeric vector or an R object which is coercible to one by
  `as.vector`.

- na.rm:

  Logical. Should missing values be removed?

## Value

A numeric.

## See also

[`var`](https://rdrr.io/r/stats/cor.html) and
[`sd`](https://rdrr.io/r/stats/sd.html) for the variance and standard
deviation.

## Author

From the manual "An Introduction to R", modified by Mathieu Basille
\<\email{mathieu@basille.org}\>

## Examples

``` r
bla <- rnorm(1000, sd = 100)
sd(bla)
#> [1] 99.32545
sqrt(var(bla)/length(bla))
#> [1] 3.140947
se(bla)
#> [1] 3.140947

is.na(bla) <- 200:300
sd(bla, na.rm = TRUE)
#> [1] 99.53578
se(bla, na.rm = TRUE)
#> [1] 3.319704
```
