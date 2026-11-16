# Rename an R object

Rename an R object.

## Usage

``` r
mv(from, to)
```

## Arguments

- from:

  The name of an R object, with or without quotes.

- to:

  The new name, with or without quotes.

## Author

Jean Lobry

## Examples

``` r
bla <- 2
ls()
#> [1] "bla"
mv(bla, bli)
bli
#> [1] 2
ls()
#> [1] "bli"
```
