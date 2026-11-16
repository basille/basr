# Summary of AIC/BIC

Summarizes IC differences and weights to identify plausible models
(models with highest empirical support).

## Usage

``` r
summaryIC(x, delta = 2)
```

## Arguments

- x:

  A data frame with one row per model, and one column giving an
  information-theoretic index (AIC or BIC), such as returned by
  [`AIC`](https://rdrr.io/r/stats/AIC.html) or
  [`BIC`](https://rdrr.io/r/stats/AIC.html) with several models.

- delta:

  The difference threshold to identify models with similar support.

## Value

The input data frame with additional columns `delta` giving IC
differences with the best model, `best` identifying the best model(s)
(`*` for the absolute best model, `+` for models within the threshold),
and `omega` giving AIC weights.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
## Prepare two models:
lm1 <- lm(Fertility ~ . , data = swiss)
lm2 <- update(lm1, . ~ . -Examination)

## Check AIC:
AIC(lm1, lm2)
#>     df      AIC
#> lm1  7 326.0716
#> lm2  6 325.2408

## Summary of AIC and BIC:
summaryIC(AIC(lm1, lm2))
#>     df      AIC     delta     omega best
#> lm1  7 326.0716 0.8307244 0.3976271    +
#> lm2  6 325.2408 0.0000000 0.6023729    *
summaryIC(BIC(lm1, lm2))
#>     df      BIC    delta     omega best
#> lm1  7 339.0226 2.680872 0.2074384     
#> lm2  6 336.3417 0.000000 0.7925616    *
```
