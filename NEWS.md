# basr 1.1.0

## New features

- `nselect`: Select values based on their frequency.
- `multiple`: Extract elements that appear multiple times.

## Minor changes

- `reclass` has a `na.fill` argument that replaces all non-specified values by
  NA, which is `FALSE` by default.
- `togray` is synonymous of `togrey`.
- `quit` is synonymous of `q`.
- `q` has an explicit `savePackageList` argument, which is `TRUE` by default.

## Bug fixes

- `reclass` now properly handles NAs in from/to.

# basr 1.0.0

Major cleanup for v1.0, with new functions and documentation. 

## New features

- `date_locale`: Prints a date in any locale.
- `init_httpgd`: Init a web graphic device from httpgd.
- `install_selected`: Install a list of selected packages by theme.
- `savepkglist`: Load or save the list of attached packages.
- `session_info_md`: `sessionInfo()` adapted for Markdown output.
- `update_packages`: Update user packages (CRAN and GitHub).

## Minor changes

- `colNAs` replaces `colNA`, for consistency with `colMeans` and `colSums`.
- `kcut` replaces `ncut`.

# basr 0.12

## New feature

- `ncut`: Cut into classes of equal size

# basr 0.11

## New feature

- `summaryIC`: Now provide *IC weights in an additional column.

# basr 0.10.1

## Bug fix

- `reclass`: Now vectorized, fixes the problem of colliding values of 'x' and
  'from'.

# basr 0.10

## New feature

- `nsubset`: Subset according to frequency of a column (WARNING: replaces
  `nselect`!).

# basr 0.9

## New feature

- `colNA`: Number of NAs by column.

- `summaryIC`: Summary of AIC/BIC.

# basr 0.8.1

## Enhancement

- `memUse`: Return 0 if no object is present in the specified environment.

# basr 0.8

## New feature

- `memUse`: Show memory usage of saved objects.

# basr 0.7.6

## New feature

- `reclass`: `from` can be a matrix of reclassification.

# basr 0.7.5

## New feature

- `plot.confint`: Now colorize CIs non-overlapping with zero + new argument
  `add.signif` to add an asterisk for these CIs.

# basr 0.7.4

## New feature

- `reclass`: New arguments `factor` and `...`.

## Enhancement

- `values` renamed to `reclass` to avoid confusion with `raster`.

# basr 0.7.3

## New feature

- `attpkglist`: List attached packages. Used by `savepkglist`
- `.loadpkglist`: Load the `.Rpackages` file at startup

## Bug fixes

- `loadpkglist`: Fixed search order problem (see new function
  `.loadpkglist`)

# basr 0.7.2

## Enhancement

- `togray`/`togrey`: Argument `inverse` implemented

# basr 0.7.1

## Bug fixes

- `extrange`: Argument `na.rm` implemented
- Various corrections in the documentation

# basr 0.7

## New feature

- `togray`/`togrey`: Convert continuous variable to grey levels

# basr 0.6.1

## Enhancement

- `dynamitePlot`: more sensible defaults, new arguments `sym` and
  `head`, argument `arr.lwd` renamed to `lwd`

# basr 0.6

## New feature

- `confint`: Confidence Intervals for Model Parameters

# basr 0.5.3

## Feature removed

- `pandoc`: Use now `knitr:::pandoc`

# basr 0.5.2

## Bug fixes

- `pandoc`: The default css is now http://ase-research.org/R/rmd.css

# basr 0.5.1

## New features

- `pandoc`: New argument `format` for the output format

# basr 0.5

## New function

- `pandoc`: Convert using `Pandoc`

# basr 0.4

## New function

- `extrange`: Extended range

# basr 0.3

## New function

- `cv`: coefficient of variation

# basr 0.2

## Bug fixes

- `savepkglist`: throws an error if there is no attached package
- `save.image`: handles errors in `savehistory` and `savepkglist`

# basr 0.1

## New functions

- `capwords`: capitalizing
- `dynamitePlot`: creates dynamite plots
- `getcolors`: choosing colors visually
- `loadpkglist`: load or save the list of attached packages
- `manual`: generate package reference manual
- `mv`: rename an R object
- `nselect`: subsetting tables given occurrences
- `q`: terminate an R session
- `quit`: terminate an R session
- `save.image`: save the current workspace
- `savepkglist`: load or save the list of attached packages
- `se`: standard errors
- `table`: modified table function to handle NAs
- `values`: change the values of a vector
- `writeFunction`: function output
