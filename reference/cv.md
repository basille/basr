# Coefficient of variation

Computes the coefficient of variation (i.e. `sd / mean`) of the values
in `x`. If `ci` is `TRUE` then confidence intervals are also computed.

## Usage

``` r
cv(
  x,
  na.rm = FALSE,
  ci = FALSE,
  conf.level = 0.95,
  method = c("mckaymod", "mckay", "naive")
)
```

## Arguments

- x:

  A numeric vector

- na.rm:

  Logical. Should missing values be removed?

- ci:

  Logical. Should confidence intervals be computed?

- conf.level:

  Confidence level of the interval.

- method:

  The method to compute the confidence intervale. Either the naive
  (`naive`), the McKay (`mckay`) or the modified McKay (`mckaymod`,
  default) approximation.

## Value

If `ci`, returns a list with the coefficient of variation. in the first
element and the confidence interval in the second.

## Original URL (not available anymore)

`http://tolstoy.newcastle.edu.au/R/e2/help/07/06/19043.html`

## References

Vangel, M. G. (1996) Confidence intervals for a normal coefficient of
variation. The American Statistician, 50: 21-26

## Author

From Kevin Wright, modified by Mathieu Basille <mathieu@basille.org>

## Examples

``` r
xx <- 1:10
cv(xx)
#> [1] 0.5504819
sd(xx)/mean(xx)
#> [1] 0.5504819
cv(xx, ci = TRUE)
#> Warning: Confidence interval may be approximate.
#> $k
#> [1] 0.5504819
#> 
#> $ci
#> [1] 0.3518577 1.4741283
#> 
#> attr(,"alpha")
#> [1] 0.05
#> attr(,"method")
#> [1] "mckaymod"
```
