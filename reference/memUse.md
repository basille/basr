# Memory usage of saved objects

Show memory usage of saved objects.

## Usage

``` r
memUse(pos = 1, pattern)

# S3 method for class 'memUse'
print(
  x,
  sort = c("size", "alphabetical"),
  decreasing = ifelse(sort == "size", TRUE, FALSE),
  n = 10,
  bytes = FALSE,
  ...
)
```

## Arguments

- pos:

  Which environment to use to list the saved objects (as a position in
  the search list). See [`ls`](https://rdrr.io/r/base/ls.html).

- pattern:

  An optional regular expression. See
  [`ls`](https://rdrr.io/r/base/ls.html).

- x:

  An object of class `memUse`.

- sort:

  Whether to sort the object list by `size` or `alphabetical` order.

- decreasing:

  Logical. Should the sort order be increasing or decreasing?

- n:

  A single integer, giving the number of objects to display.

- bytes:

  Logical. Whether to display the raw size in bytes.

- ...:

  Further arguments passed to or from other methods.

## Value

A data frame of classe `memUse` providing object names, class and memory
usage; or `0` if no object is present in the specified environment.

## Original URL

<https://stackoverflow.com/a/9839949>. See also the complete Stack
Overflow thread:
<https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session>

## Author

Michael Hallquist, modified by Mathieu Basille \<<mathieu@basille.org>\>

## Examples

``` r
## Create some data
rand <- rnorm(100)
data(iris)
foo <- function(x) print(x)
##'
## Check memory usage (and raw numbers in bytes)
memUse()
#>           class   size
#> iris data.frame 7.1 Kb
memUse()$bytes
#> [1] 7256
```
