# Subset vectors, matrices and data frames based on value frequency

Subset vectors and tables (`data.frames` and `matrix`) based on the
frequency of the values (of the vector or a column of the table), and a
type of inequality (less than and greater than).

## Usage

``` r
nsubset(x, ...)

# Default S3 method
nsubset(x, n, comp = c(">=", ">", "<", "<="), na.rm = TRUE, ...)

# S3 method for class 'data.frame'
nsubset(
  x,
  col,
  n,
  comp = c(">=", "<=", ">", "<", "=="),
  na.rm = TRUE,
  select,
  drop = FALSE,
  ...
)

# S3 method for class 'matrix'
nsubset(
  x,
  col,
  n,
  comp = c(">=", "<=", ">", "<", "=="),
  na.rm = TRUE,
  select,
  drop = FALSE,
  ...
)
```

## Arguments

- x:

  An object to be subsetted, i.e. a vector of atomic type ('logical',
  'integer', 'numeric', 'complex', 'character' and 'raw'), a factor, a
  `data.frame` or a `matrix`.

- ...:

  Further arguments passed to or from other methods.

- n:

  An integer; either a single value or the same number of values than
  returned by `table(x)` / `table(x[, "col"])`.

- comp:

  The sign of the inequality, one of `">="` (default), `">"`, `"<"` or
  `"<="`.

- na.rm:

  Logical; whether NAs should be removed (default) or not (in which
  case, they will be considered as a value of their own).

- col:

  Name of the column which is used to compute frequencies (with or
  without quotes).

- select:

  Expression, indicating columns to select from a data frame.

- drop:

  Passed on to `[` indexing operator.

## Value

A subset of the same class as `x`.

## Author

Mathieu Basille <mathieu@basille.org>

## Examples

``` r

## Simples examples and NAs:
set.seed(42)
bla <- rpois(100, 4)
bla[(1:9)*10] <- NA
bla
#>   [1]  7  7  3  6  5  4  5  2  5 NA  4  5  7  3  4  7  8  2  4 NA  7  2  9  7  1
#>  [26]  4  3  7  4 NA  5  6  3  5  0  6  0  2  7 NA  3  4  1  8  3  8  6  5  8 NA
#>  [51]  3  3  3  5  1  5  5  2  3 NA  5  9  5  4  6  2  3  6  5 NA  1  2  2  4  2
#>  [76]  5  0  3  4 NA  4  2  3  5  5  4  2  1  1 NA  5  0  2  7  7  5  3  4  5  4
table(bla, useNA = "ifany")
#> bla
#>    0    1    2    3    4    5    6    7    8    9 <NA> 
#>    4    6   12   14   14   19    6   10    4    2    9 
nsubset(bla, 5)
#>  [1] 7 7 3 6 5 4 5 2 5 4 5 7 3 4 7 2 4 7 2 7 1 4 3 7 4 5 6 3 5 6 2 7 3 4 1 3 6 5
#> [39] 3 3 3 5 1 5 5 2 3 5 5 4 6 2 3 6 5 1 2 2 4 2 5 3 4 4 2 3 5 5 4 2 1 1 5 2 7 7
#> [77] 5 3 4 5 4
nsubset(bla, 14)
#>  [1] 3 5 4 5 5 4 5 3 4 4 4 3 4 5 3 5 3 4 3 5 3 3 3 5 5 5 3 5 5 4 3 5 4 5 3 4 4 3
#> [39] 5 5 4 5 5 3 4 5 4
nsubset(bla, 5, comp = "<")
#>  [1] 8 9 0 0 8 8 8 9 0 0
nsubset(bla, 5, na.rm = FALSE)
#>  [1]  7  7  3  6  5  4  5  2  5 NA  4  5  7  3  4  7  2  4 NA  7  2  7  1  4  3
#> [26]  7  4 NA  5  6  3  5  6  2  7 NA  3  4  1  3  6  5 NA  3  3  3  5  1  5  5
#> [51]  2  3 NA  5  5  4  6  2  3  6  5 NA  1  2  2  4  2  5  3  4 NA  4  2  3  5
#> [76]  5  4  2  1  1 NA  5  2  7  7  5  3  4  5  4

## Factors and n provided for each value:
set.seed(42)
(bli <- factor(LETTERS[sample(1:10, size = 100, replace = TRUE)]))
#>   [1] A E A I J D B J A H G D I E D J B C I I D E E D B H C J A J H F J H D D F
#>  [38] B E D B H B C H G A E J B F F B D C F E B G F J J H E A A G D J I D I C E
#>  [75] H E F B B H A B E H G H E D I D G J B E F C F A I I
#> Levels: A B C D E F G H I J
table(bli)
#> bli
#>  A  B  C  D  E  F  G  H  I  J 
#>  9 13  6 13 13  9  6 11  9 11 
factor(nsubset(bli, n = 7:16))
#>  [1] A E A D B A D E D B D E E D B A D D B E D B B A E B B D E B E A A D D E E B
#> [39] B A B E E D D B E A
#> Levels: A B D E

## Data frames:
set.seed(42)
bla <- data.frame(value = rnorm(100), group = sample(letters[1:4],
    size = 100, replace = TRUE, prob = (1:4) * 10))
table(bla$group)
#> 
#>  a  b  c  d 
#>  7 16 28 49 
bli <- nsubset(bla, group, 25)
head(bli)
#>         value group
#> 2 -0.56469817     c
#> 4  0.63286260     c
#> 5  0.40426832     d
#> 6 -0.10612452     c
#> 8 -0.09465904     c
#> 9  2.01842371     d
table(bli$group)
#> 
#>  c  d 
#> 28 49 
nsubset(bla, group, 25, comp = ">", select = 1, drop = TRUE)
#>  [1] -0.56469817  0.63286260  0.40426832 -0.10612452 -0.09465904  2.01842371
#>  [7] -0.06271410  1.30486965  2.28664539 -1.38886070 -0.27878877 -0.13332134
#> [13] -0.28425292 -2.65645542 -2.44046693 -0.30663859 -1.78130843 -0.17191736
#> [19]  1.89519346 -0.25726938 -1.76316309  0.46009735 -0.63999488  0.45545012
#> [25]  0.70483734  1.03510352 -0.60892638 -0.78445901 -2.41420765  0.03612261
#> [31]  0.20599860 -0.36105730  0.75816324 -0.72670483 -1.36828104  0.43281803
#> [37] -0.81139318  1.44410126 -0.43144620  0.32192527 -0.78383894  1.57572752
#> [43]  0.64289931  0.08976065  0.67928882  0.08983289 -2.99309008  0.28488295
#> [49]  0.18523056  0.58182373 -0.72729206  1.30254263  1.03850610 -1.04311894
#> [55]  0.62351816 -0.95352336  0.58099650  0.76817874  0.46376759 -0.88577630
#> [61] -1.09978090  1.51270701  0.25792144  0.08844023 -1.19432890  0.61199690
#> [67] -0.21713985 -0.18275671  0.93334633  0.82177311  0.65034856  1.39111046
#> [73] -1.11078888 -0.86079259 -1.45921400  0.07998255  0.65320434
nsubset(bla, group, 25, comp = ">", select = group, drop = TRUE)
#>  [1] "c" "c" "d" "c" "c" "d" "d" "c" "c" "d" "d" "c" "c" "d" "c" "c" "d" "c" "d"
#> [20] "d" "d" "d" "d" "d" "d" "d" "d" "d" "c" "c" "d" "c" "d" "d" "c" "c" "d" "d"
#> [39] "c" "d" "d" "d" "c" "d" "d" "c" "c" "c" "d" "d" "c" "d" "d" "d" "c" "d" "d"
#> [58] "d" "c" "c" "d" "c" "d" "d" "d" "d" "c" "d" "d" "d" "d" "d" "d" "d" "c" "d"
#> [77] "d"

## Fast computation:
set.seed(42)
bla <- rpois(1e7, 4)
table(bla)
#> bla
#>       0       1       2       3       4       5       6       7       8       9 
#>  183869  732303 1463918 1953614 1954018 1562489 1041924  596057  298141  132588 
#>      10      11      12      13      14      15      16      17      18 
#>   52921   19180    6343    1909     516     154      45       9       2 
system.time(table(nsubset(bla, 1e7)))
#>    user  system elapsed 
#>   0.398   0.261   0.660 

bla <- data.frame(value = rnorm(1e7), group = sample(letters[1:4],
    size = 1e7, replace = TRUE, prob = (1:4) * 10))
table(bla$group)
#> 
#>       a       b       c       d 
#>  999413 2000466 2999466 4000655 
system.time(bli <- nsubset(bla, group, 25))
#>    user  system elapsed 
#>   1.400   0.572   1.971 
```
