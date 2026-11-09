# Submission

* This is a new submission.

## Test environments

* Local: Debian GNU/Linux 13 (trixie), R version 4.5.3 (2026-03-11)
* winbuilder: x86_64-w64-mingw32, R version 4.6.2 (2026-10-29 ucrt)
* winbuilder: x86_64-w64-mingw32, R Under development (unstable) (2026-11-07
  r90850 ucrt)

## R CMD check results

* Local, with devtools::check(remote = TRUE, manual = TRUE):

0 errors | 0 warnings | 1 note

The note is about StackOverflow URLs, which are valid but return a 403 error:

  Found the following (possibly) invalid URLs:
    URL: https://stackoverflow.com/a/9839949
      From: man/memUse.Rd
      Status: 403
      Message: Forbidden
    URL: https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
      From: man/memUse.Rd
      Status: 403
      Message: Forbidden

* winbuilder:

The note is about CRAN incoming feasibility:

  Maintainer: 'Mathieu Basille <mathieu@basille.org>'

  New submission

## Downstream dependencies

There are currently no downstream dependencies for this package.
