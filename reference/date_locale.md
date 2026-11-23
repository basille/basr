# Prints a date in any locale

Convenience function to print a date using user format and locale.

## Usage

``` r
date_locale(x = Sys.Date(), format = "%A %d %B %Y", locale = "fr_FR.utf8")
```

## Arguments

- x:

  A Date object; defaults to current date.

- format:

  A character string providing the format, as recognized by
  [`format`](https://rdrr.io/r/base/format.html) (see
  [`as.Date`](https://rdrr.io/r/base/as.Date.html) for details);
  defaults to `"%A %d %B %Y"`, i.e. `"vendredi 06 novembre 2026"` in
  French.

- locale:

  A character string providing the locale, as recognized by
  [`Sys.setlocale`](https://rdrr.io/r/base/locales.html) (see
  [`locales`](https://rdrr.io/r/base/locales.html) for details);
  defaults to `"fr_FR.utf8"`.

## Value

A character string with the date in user format and locale.

## Author

Mathieu Basille \<<mathieu@basille.org>\>

## Examples

``` r
## Today written in French
date_locale()
#> Warning: OS reports request to set locale to "fr_FR.utf8" cannot be honored
#> [1] "Monday 23 November 2026"

## Christmas 2000 written in French
date_locale(as.Date("2000-12-25"))
#> Warning: OS reports request to set locale to "fr_FR.utf8" cannot be honored
#> [1] "Monday 25 December 2000"
```
