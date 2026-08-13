# Plot longitudinal change contrasts

Plot longitudinal change contrasts

## Usage

``` r
plot_longitudinal(
  x,
  estimand = c("change_from_baseline", "difference_in_changes"),
  locale = "en"
)
```

## Arguments

- x:

  An `analysis_result`.

- estimand:

  Longitudinal contrast estimand.

- locale:

  Output locale, `en` or `ru`.

## Value

A `ggplot` object.
