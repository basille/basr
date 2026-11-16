# Capitalizing - every first letter of a word is changed to upper case

A simple function to capitalize a character string: the first letter of
every word is changed to upper case. Optionally, all other letters are
changed to lower case.

## Usage

``` r
capwords(s, strict = FALSE)
```

## Arguments

- s:

  A character vector, or an object that can be coerced to character by
  `as.character`.

- strict:

  Logical: other letters than the first are converted to lower case

## Value

A character vector of the same length and with the same attributes as
`x` (after possible coercion).

## Author

From the help page of [`chartr`](https://rdrr.io/r/base/chartr.html)

## Examples

``` r
capwords(c("using AIC for model selection"))
#> [1] "Using AIC For Model Selection"
## ->  [1] "Using AIC For Model Selection"
capwords(c("using AIC", "for MODEL selection"), strict = TRUE)
#> [1] "Using Aic"           "For Model Selection"
## ->  [1] "Using Aic"  "For Model Selection"
##                ^^^        ^^^^^
##               'bad'       'good'
```
