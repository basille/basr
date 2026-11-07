basr
====

The package `basr` is a collection of basic functions that I find mostly useful
for myself.

## Installation

Install `basr` from CRAN:

```{r}
install.packages("basr")
```

To install the package from GitHub (which may be ahead of CRAN from time to
time), use the package [`remotes`](https://cran.r-project.org/package=remotes)
or better [`pak`](https://cran.r-project.org/package=pak) to install `basr`:


```{r}
remotes::install_github("basille/basr")
pak::pkg_install("basille/basr")
```

## Getting started

Here is a complete list of documented functions:

### Data

- `colNA()`: Number of NAs for each column of a data frame.
- `extrange()`: Range extended by a given proportion.
- `kcut()`: Cut into classes of equal size.
- `nsubset()`: Subset according to frequency of a column.
- `reclass()`: Reclassify the values of a vector.
- `table()`: Modified to handle NAs.

### Stats

- `confint()`: Modified to display coefficients and plot them together with CIs. 
- `cv()`: Computes the coefficient of variation (i.e. sd/mean).
- `se()`: Standard errors.
- `summaryIC()`: Summary of AIC/BIC.

### Graphs

- `dynamitePlot()`: Dynamite Plots.
- `getcolors()`: Choosing colors visually.
- `togray()`: Convert continuous variable to grey levels.

### Package management

- `install_selected()`: Install a list of selected packages by theme.
- `q()`: Terminate an R Session (with packages saved).
- `save.image()`: Save the current workspace (including attached packages).
- `loadpkglist()`/`savepkglist()`: Load or save the list of attached packages.
- `update_packages()`: Update user packages (CRAN and GitHub).

### Utils

- `capwords()`: Capitalizing - every first letter of a word is changed to upper
  case.
- `cowsay_fortune()`: Nice print of a fortune cookie.
- `date_locale()`: Prints a date in any locale.
- `init_httpgd()`: Init a web graphic device from `httpgd`.
- `memUse()`: Show memory usage of saved objects.
- `mv()`: Rename an R object.
- `manual()`: Generate package reference manual.
- `session_info_md()`: sessionInfo() adapted for Markdown output.
- `writeFunction()`: Prints a function to a file.
