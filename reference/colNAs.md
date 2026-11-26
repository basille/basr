# Number of NAs by column

Returns the number of NAs for each column of a data frame.

## Usage

``` r
colNAs(x)
```

## Arguments

- x:

  A data frame.

## Value

A numeric vector.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
set.seed(123)
(df <- data.frame(matrix(sample(c(NA, 1:10), 100, TRUE), ncol = 4)))
#>    X1 X2 X3 X4
#> 1   2  3  8  8
#> 2   2 NA  5  6
#> 3   9 10  4  2
#> 4   1  6  8  7
#> 5   5  4  9  8
#> 6  10  9  3  2
#> 7   4  6  5  6
#> 8   3  8 10  2
#> 9   5  8  7  6
#> 10  8  9  5  5
#> 11  9  6  5  9
#> 12 10 10  6  4
#> 13  4  4 NA  4
#> 14  2  6  5  7
#> 15 10  4  1  2
#> 16  8 10 NA  9
#> 17  8  5  1  1
#> 18  8  8  3  9
#> 19  2  1  4  1
#> 20  7  4  5  9
#> 21  9  7  2  5
#> 22  6  1  8  3
#> 23  9 NA  3 NA
#> 24  8  8  5  5
#> 25  2 10  8  2
colNAs(df)
#> X1 X2 X3 X4 
#>  0  2  2  1 
```
