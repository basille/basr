# Load or save the list of attached packages

Display, save or load the list of attached packages.

## Usage

``` r
savepkglist(file = ".Rpackages")

attpkglist()

loadpkglist(file = ".Rpackages")
```

## Arguments

- file:

  The name of the file in which to save the list of attached packages,
  or from which to load it. The path is relative to the current working
  directory.

## Details

`attpkglist` simply lists all attached packages (i.e. not base
packages).

`savepkglist` saves the list of all attached packages in a file, with
one package per line.

`loadpkglist` loads a list of packages from a file. The file should
contain one package name per line, without quotes, and no empty line. If
the packages are not installed, the function sends a warning.

`.loadpkglist` automatically loads the `.Rpackages` file at startup (see
the Note below).

## Note

To automatically load a `.Rpackages` list at startup, add this in your
`.Rprofile`:

    ### Load packages at the start of R if the package list exists
    basr:::.loadpkglist()

Essentially, the function appends the list of packages at the end of the
`defaultPackages` option (see
[`options`](https://rdrr.io/r/base/options.html) for this option; see
also [`Startup`](https://rdrr.io/r/base/Startup.html) for more details
about the initialization at start of an R session).

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
if (FALSE) { # \dontrun{
## Save the list of packages in file 'list.Rpackages':
savepkglist(file = "list.Rpackages")} # }
if (FALSE) attpkglist() # \dontrun{}
if (FALSE) loadpkglist() # \dontrun{}
```
