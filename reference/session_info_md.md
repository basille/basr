# sessionInfo() formatted for Markdown

Displays session information formatted for Markdown.

## Usage

``` r
session_info_md(
  language = c("EN", "FR"),
  header_level = 2,
  unnumbered = FALSE,
  sort_attached = TRUE
)
```

## Arguments

- language:

  The output language; currently supports English ("EN", default) and
  French ("FR").

- header_level:

  The header level to use for (sub)sections of the output.

- unnumbered:

  Logical; should the (sub)sections be numbered or not? Defaults to
  `"FALSE"`.

- sort_attached:

  Logical; should attached packages be listed by alphabetical order?
  Defaults to `"TRUE"`.

## Value

Session information formatted for Markdown.

## Details

The function can be used with something like this in a RMarkdown file:

    ```{r r-session, echo = FALSE, results = "asis", cache = FALSE}
    session_info_md(language = "FR", unnumbered = TRUE)
    ```

## Note

Requires packages sessioninfo and knitr.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
## Show session information formatted for Markdown in French:
session_info_md(language = "FR")
#> ## Informations de session R
#> 
#> ### Plateforme
#> 
#> |Paramètre |Valeur                                                         |
#> |:---------|:--------------------------------------------------------------|
#> |version   |R version 4.6.2 (2026-10-29)                                   |
#> |os        |Ubuntu 26.04.1 LTS                                             |
#> |system    |x86_64, linux-gnu                                              |
#> |ui        |X11                                                            |
#> |language  |en                                                             |
#> |collate   |C                                                              |
#> |ctype     |C.UTF-8                                                        |
#> |tz        |UTC                                                            |
#> |date      |2026-11-16                                                     |
#> |pandoc    |3.8.3 @ /opt/hostedtoolcache/pandoc/3.8.3/x64/ (via rmarkdown) |
#> |quarto    |NA                                                             |
#> 
#> ### Packages
#> 
#> |Package     | Attaché | Version|       Date| lib| Source|
#> |:-----------|:-------:|-------:|----------:|---:|------:|
#> |basr        |    *    |   1.1.0| 2026-11-16| [1]|  local|
#> |R6          |         |   2.6.1| 2025-02-15| [1]|   RSPM|
#> |askpass     |         |   1.2.1| 2024-10-04| [1]|   RSPM|
#> |bslib       |         |  0.12.0| 2026-08-04| [1]|   RSPM|
#> |cachem      |         |   1.1.0| 2024-05-16| [1]|   RSPM|
#> |cli         |         |   3.6.6| 2026-04-09| [1]|   RSPM|
#> |curl        |         |   8.1.0| 2026-10-14| [1]|   RSPM|
#> |desc        |         |   1.4.3| 2023-12-10| [1]|   RSPM|
#> |devtools    |         |   2.5.2| 2026-04-30| [1]|   RSPM|
#> |digest      |         |  0.6.39| 2025-11-19| [1]|   RSPM|
#> |downlit     |         |   0.4.5| 2025-11-14| [1]|   RSPM|
#> |ellipsis    |         |   0.3.3| 2026-04-04| [1]|   RSPM|
#> |evaluate    |         |   1.0.5| 2025-08-27| [1]|   RSPM|
#> |fansi       |         |   1.0.7| 2025-11-19| [1]|   RSPM|
#> |fastmap     |         |   1.2.0| 2024-05-15| [1]|   RSPM|
#> |fontawesome |         |   0.5.3| 2024-11-16| [1]|   RSPM|
#> |fs          |         |   2.1.0| 2026-04-18| [1]|   RSPM|
#> |glue        |         |   1.8.1| 2026-04-17| [1]|   RSPM|
#> |htmltools   |         |   0.5.9| 2025-12-04| [1]|   RSPM|
#> |htmlwidgets |         |   1.6.4| 2023-12-06| [1]|   RSPM|
#> |httr2       |         |   1.3.0| 2026-07-13| [1]|   RSPM|
#> |jquerylib   |         |   0.1.4| 2021-04-26| [1]|   RSPM|
#> |jsonlite    |         |   2.0.0| 2025-03-27| [1]|   RSPM|
#> |knitr       |         |    1.52| 2026-09-06| [1]|   RSPM|
#> |lifecycle   |         |   1.0.5| 2026-01-08| [1]|   RSPM|
#> |magrittr    |         |   2.0.5| 2026-04-04| [1]|   RSPM|
#> |memoise     |         |   2.0.1| 2021-11-26| [1]|   RSPM|
#> |openssl     |         |   2.5.0| 2026-11-02| [1]|   RSPM|
#> |otel        |         |   0.3.0| 2026-11-07| [1]|   RSPM|
#> |pak         |         |  0.11.2| 2026-11-07| [1]|   RSPM|
#> |pillar      |         |  1.11.1| 2025-09-17| [1]|   RSPM|
#> |pkgbuild    |         |   1.4.8| 2025-05-26| [1]|   RSPM|
#> |pkgconfig   |         |   2.0.3| 2019-09-22| [1]|   RSPM|
#> |pkgdown     |         |   2.2.1| 2026-07-07| [1]|   RSPM|
#> |pkgload     |         |   1.5.3| 2026-06-15| [1]|   RSPM|
#> |purrr       |         |   1.2.2| 2026-04-10| [1]|   RSPM|
#> |ragg        |         |   1.5.2| 2026-03-23| [1]|   RSPM|
#> |rlang       |         |   1.3.0| 2026-07-05| [1]|   RSPM|
#> |rmarkdown   |         |    2.32| 2026-09-01| [1]|   RSPM|
#> |rstudioapi  |         |  0.19.0| 2026-06-11| [1]|   RSPM|
#> |sass        |         |  0.4.10| 2025-04-11| [1]|   RSPM|
#> |sessioninfo |         |   1.2.4| 2026-06-04| [1]|   RSPM|
#> |systemfonts |         |   1.3.2| 2026-03-05| [1]|   RSPM|
#> |textshaping |         |   1.0.5| 2026-03-06| [1]|   RSPM|
#> |tibble      |         |   3.3.1| 2026-01-11| [1]|   RSPM|
#> |usethis     |         |   3.2.2| 2026-09-10| [1]|   RSPM|
#> |vctrs       |         |   0.7.3| 2026-04-11| [1]|   RSPM|
#> |whisker     |         |   0.4.1| 2022-12-05| [1]|   RSPM|
#> |withr       |         |   3.0.3| 2026-06-19| [1]|   RSPM|
#> |xfun        |         |    0.62| 2026-10-16| [1]|   RSPM|
#> |xml2        |         |   1.6.0| 2026-06-22| [1]|   RSPM|
#> |yaml        |         |  2.3.12| 2025-12-10| [1]|   RSPM|
#> 
#> **Chemin des librairies**
#> 
#> * [1] `/home/runner/work/_temp/Library`
#> * [2] `/opt/R/4.6.2/lib/R/site-library`
#> * [3] `/opt/R/4.6.2/lib/R/library`
```
