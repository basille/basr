# Update user packages (CRAN and GitHub)

Checks the list of user-installed packages (on CRAN, BioConductor or
GitHub) and updates them where required.

## Usage

``` r
update_packages()
```

## Value

(Invisibly) A data frame with information about the updated package(s).

## Note

Requires package pak.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
if (FALSE) { # \dontrun{
## Simply update all user packages:
update_packages()} # }
```
