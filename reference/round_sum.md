# Rounding that preserve sum

Regular rounding can break the sum of values. This can be particularly
annoying when working on proportions that sum up to 1. `round_sum` works
around the problem by adjusting values to preserve the sum.

## Usage

``` r
round_sum(x, digits = 0)
```

## Arguments

- x:

  A numeric vector.

- digits:

  Integer indicating the number of decimal places. Negative values round
  to a power of ten, so for example `digits = -2` will round to the
  nearest hundred.

## Value

A numeric vector of the same length than `x` with rounded values.

## Author

Initial function from josliber (<https://stackoverflow.com/a/32544987>),
improved by Mikhail (<https://stackoverflow.com/a/35930285>), and
further improved by Mathieu Basille <mathieu@basille.org>

## Examples

``` r
(toto <- c(1/3, 1/3, 1/3))
#> [1] 0.3333333 0.3333333 0.3333333
sum(toto)
#> [1] 1
#'
## Regular rounding:
round(toto, 2)
#> [1] 0.33 0.33 0.33
sum(round(toto, 2))
#> [1] 0.99
#'
## Rounding preserving sum:
round_sum(toto, 2)
#> [1] 0.33 0.33 0.34
sum(round_sum(toto, 2))
#> [1] 1
#'
## Be careful when pushed to the extremes:
round(toto, 0)
#> [1] 0 0 0
round_sum(toto, 0)
#> [1] 0 0 1
#'
## Works also with negative 'digits' to round to powers of ten:
(toto <- c(1/3, 1/3, 1/3)*1e6)
#> [1] 333333.3 333333.3 333333.3
round(toto, -2)
#> [1] 333300 333300 333300
round_sum(toto, -2)
#> [1] 333300 333300 333400
```
