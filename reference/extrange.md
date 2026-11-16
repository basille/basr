# Extended range

Returns the range extended by a given proportion.

## Usage

``` r
extrange(x, percent = 0.1, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- percent:

  The proportion to be added to the range.

- na.rm:

  Logical, indicating if `NA`'s should be omitted.

## Value

A vector containing the minimum and maximum values of the extended
range.

## Details

If the regular range returns a single value, the proportion is computed
on this value itself (and not on the range).

## Author

Mathieu Basille \<<mathieu@basille.org>\>

## Examples

``` r
extrange(0:10)
#> [1] -1 11
extrange(0:10, percent = .5)
#> [1] -5 15
extrange(-10:10)
#> [1] -12  12
extrange(rep(10, 3))
#> [1]  9 11
```
