# Generate package reference manual

Generate package reference manual.

## Usage

``` r
manual(pkg = ".", path = NULL, preview = TRUE, overwrite = FALSE)
```

## Arguments

- pkg:

  Package description, can be path or package name. See
  [`as.package`](https://devtools.r-lib.org/reference/as.package.html)
  for more information.

- path:

  Path in which to produce package. If `NULL`, defaults to the root
  directory of the package.

- preview:

  Preview generated PDF file.

- overwrite:

  Overwrite output file if it exists.

## Value

The output of the system command used to generate the manual.

## Note

Requires package devtools.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
if (FALSE) { # \dontrun{
## Generate reference manual for the package in the current folder:
manual()} # }
```
