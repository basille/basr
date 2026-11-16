# Cut into classes of equal size

Cut a numeric vector into k classes of (roughly) equal size.

## Usage

``` r
kcut(x, k = 10, labels = FALSE)
```

## Arguments

- x:

  Numeric vector to cut.

- k:

  The number of classes.

- labels:

  Labels for the levels of the resulting category. By default (`FALSE`),
  labels are constructed using simple integer codes; if `NULL`, labels
  are build using `"(a,b]"` interval notation. Alternatively, a custom
  vector of length `k` can be used.

## Value

A vector of k classes.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\> and Emiel van Loon

## Examples

``` r
bla <- rnorm(100)
summary(bla)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -2.053247 -0.631106 -0.063435 -0.001962  0.574233  3.241040 
bli <- kcut(bla)
table(bli)
#> bli
#>  1  2  3  4  5  6  7  8  9 10 
#> 10 10 10 10 10 10 10 10 10 10 
head(kcut(bla, labels = NULL))
#> [1] (-0.798,-0.486]  (0.709,1.13]     (-0.486,-0.282]  (-1.26,-0.798]  
#> [5] (-0.0634,0.203]  (-0.282,-0.0634]
#> 10 Levels: [-2.05,-1.26] (-1.26,-0.798] (-0.798,-0.486] ... (1.13,3.24]
head(kcut(bla, labels = LETTERS[1:10]))
#> [1] C I D B F E
#> Levels: A B C D E F G H I J
```
