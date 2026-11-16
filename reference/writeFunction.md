# Prints a function to a file

Prints a function to a file.

## Usage

``` r
writeFunction(fun, file = NULL)
```

## Arguments

- fun:

  A function.

- file:

  A character string naming a file. By default, write the function in
  `<fun>.R` in the working directory.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
if (FALSE) { # \dontrun{
f1 <- function(x) {
    ## Comment
    print(x)
}
writeFunction(f1)
rm(f1)
source("f1.R")
file.remove("f1.R")
f1(3)} # }
```
