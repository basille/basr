# Reclassify the values of a vector

Reclassify given values of a vector by new values. Note that all values
need not to be documented, only the ones that need to be modified.

## Usage

``` r
reclass(x, from, to = NULL, na.fill = FALSE, factor = FALSE, ...)
```

## Arguments

- x:

  A character or numeric vector.

- from:

  A vector describing the values to change from, or a matrix of
  reclassification with two columns (from, to).

- to:

  A vector describing the values to change to, or nothing if `from` is a
  matrix.

- na.fill:

  Logical, whether to replace all values of `x` not specified in `from`
  by `NA`s (defaults to `FALSE`).

- factor:

  Logical, whether to return a factor (defaults to `FALSE`).

- ...:

  Additional arguments passed to `factor`.

## Value

A vector with the same length as `x`.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
(bla <- rep(1:5, 3))
#>  [1] 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
reclass(bla, c(3, 4), c(7, 3))
#>  [1] 1 2 7 3 5 1 2 7 3 5 1 2 7 3 5
reclass(bla, c(3, 4), c("a", "b"))
#>  [1] "1" "2" "a" "b" "5" "1" "2" "a" "b" "5" "1" "2" "a" "b" "5"
reclass(bla, c(3, 4), c("a", "b"), na.fill = TRUE)
#>  [1] NA  NA  "a" "b" NA  NA  NA  "a" "b" NA  NA  NA  "a" "b" NA 

## Conversion as a factor:
reclass(bla, c(3, 4), c("a", "b"), factor = TRUE)
#>  [1] 1 2 a b 5 1 2 a b 5 1 2 a b 5
#> Levels: 1 2 5 a b
(bli <- rep(letters[1:5], 3))
#>  [1] "a" "b" "c" "d" "e" "a" "b" "c" "d" "e" "a" "b" "c" "d" "e"
reclass(bli, c("b", "d"), c(1, 2))
#>  [1] "a" "1" "c" "2" "e" "a" "1" "c" "2" "e" "a" "1" "c" "2" "e"

## With a matrix of reclassification:
(mat <- matrix(c("b", "d", 1, 2), ncol = 2))
#>      [,1] [,2]
#> [1,] "b"  "1" 
#> [2,] "d"  "2" 
reclass(bli, mat)
#>  [1] "a" "1" "c" "2" "e" "a" "1" "c" "2" "e" "a" "1" "c" "2" "e"

## Fast computation time on large vectors:
blu <- rpois(1e6, 10)
system.time(reclass(blu, c(3, 4), c(7, 3)))
#>    user  system elapsed 
#>   1.739   0.051   1.790 

## Cases with NAs in x, from or to:
bla[c(2, 4)] <- NA
bla
#>  [1]  1 NA  3 NA  5  1  2  3  4  5  1  2  3  4  5
reclass(bla, c(3, 4), c("a", NA))
#>  [1] "1" NA  "a" NA  "5" "1" "2" "a" NA  "5" "1" "2" "a" NA  "5"
reclass(bla, c(NA, 4), c("a", "b"))
#>  [1] "1" "a" "3" "a" "5" "1" "2" "3" "b" "5" "1" "2" "3" "b" "5"
reclass(bla, c(NA, 4), c("a", NA))
#>  [1] "1" "a" "3" "a" "5" "1" "2" "3" NA  "5" "1" "2" "3" NA  "5"
```
