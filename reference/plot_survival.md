# Plot survival or cumulative-incidence curves

Plot survival or cumulative-incidence curves

## Usage

``` r
plot_survival(
  x,
  estimand = c("survival", "cumulative_risk", "cumulative_incidence"),
  locale = "en"
)
```

## Arguments

- x:

  An `analysis_result`.

- estimand:

  Curve estimand to display.

- locale:

  Output locale, `en` or `ru`.

## Value

A `ggplot` object.
