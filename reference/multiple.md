# Extract elements that appear multiple times

Similarly to `unique`, `multiple` returns a vector with only the values
that appear several times, i.e. that are actually duplicated.

## Usage

``` r
multiple(x, single = FALSE, na.rm = FALSE)
```

## Arguments

- x:

  A vector.

- single:

  Logical, whether to actually return only values that appear a single
  time, i.e. the opposite of `multiple` (defaults to `FALSE`).

- na.rm:

  Logical, whether to remove `NA`s. Defaults to `FALSE`, i.e. `NA` is
  considered a value of its own.

## Value

An object of the same type of `x`, but with only the values that appear
several times.

## Author

Mathieu Basille <mathieu@basille.org>

## Examples

``` r
(toto <- rep(c(1:4, NA), times = 1:5))
#>  [1]  1  2  2  3  3  3  4  4  4  4 NA NA NA NA NA
unique(toto)
#> [1]  1  2  3  4 NA
multiple(toto)
#> [1]  2  3  4 NA
unique(nsubset(toto, 2)) # `nsubset(x, 2)` keeps only values that occur at least twice
#> [1] 2 3 4

## Argument 'single'
multiple(toto, single = TRUE)
#> [1] 1
unique(nsubset(toto, 2, comp = "<"))
#> [1] 1

## Argument 'na.rm'
unique(toto, na.rm = TRUE)
#> [1]  1  2  3  4 NA
```
