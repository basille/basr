# Terminate an R Session (with packages list saved)

A modified version of
[`q`](https://rdrr.io/r/base/quit.html)/[`quit`](https://rdrr.io/r/base/quit.html)
that allows to save the list of attached packages. See
[`quit`](https://rdrr.io/r/base/quit.html) for the function details.

## Usage

``` r
q(save = "yes", savePackageList = TRUE, status = 0, runLast = TRUE)

quit(save = "yes", savePackageList = TRUE, status = 0, runLast = TRUE)
```

## Arguments

- save:

  A character string indicating whether the environment (workspace)
  should be saved, one of `"no"` or `"yes"` (which is the default).

- savePackageList:

  Logical, whether to save the list of loaded packages (defaults to
  `TRUE`). Switched to `FALSE` if `save = "no"`.

- status:

  The (numerical) error status to be returned to the operating system,
  where relevant. Conventionally ‘0’ indicates successful completion.

- runLast:

  Should ‘.Last()’ be executed?

## Details

If `savePackageList`, the list of attached packages is automatically
saved in a file `.Rpackages`. See
[`savepkglist`](http://basille.github.io/basr/reference/savepkglist.md)
for more details.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
if (FALSE) { # \dontrun{
## Quit with package list saved:
q()} # }
```
