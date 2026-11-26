# Modified table function to handle NAs

A slight modification of the
[`table`](https://rdrr.io/r/base/table.html) function, to include `NA`
values in the table by default. See
[`table`](https://rdrr.io/r/base/table.html) for details of the
function.

## Usage

``` r
table(
  ...,
  exclude = if (useNA == "no") c(NA, NaN),
  useNA = c("ifany", "no", "always"),
  dnn = list.names(...),
  deparse.level = 1
)
```

## Arguments

- ...:

  One or more objects which can be interpreted as factors (including
  numbers or character strings), or a list (such as a data frame) whose
  components can be so interpreted. (For ‘as.table’, arguments passed to
  specific methods; for ‘as.data.frame’, unused.)

- exclude:

  Levels to remove for all factors in ‘...’. If it does not contain NA
  and ‘useNA’ is not specified, it implies ‘useNA = "ifany"’. See
  ‘Details’ for its interpretation for non-factor arguments.

- useNA:

  Whether to include `NA` values in the table. Default is now `ifany`.

- dnn:

  The names to be given to the dimensions in the result (the *dimnames
  names*).

- deparse.level:

  Controls how the default ‘dnn’ is constructed. See
  [`table`](https://rdrr.io/r/base/table.html) for details.

## Value

A *contingency table*, an object of class ‘"table"’, an array of integer
values.

## Author

R Core Team, modified by Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
d <- factor(rep(c("A", "B", "C"), 10), levels = c("A", "B", "C",
    "D", "E"))
is.na(d) <- 3:4
d
#>  [1] A    B    <NA> <NA> B    C    A    B    C    A    B    C    A    B    C   
#> [16] A    B    C    A    B    C    A    B    C    A    B    C    A    B    C   
#> Levels: A B C D E
table(d)
#> d
#>    A    B    C    D    E <NA> 
#>    9   10    9    0    0    2 
```
