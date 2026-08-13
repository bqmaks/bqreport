# Estimate predictor effects within modifier levels

Estimate predictor effects within modifier levels

## Usage

``` r
within_levels(modifier)
```

## Arguments

- modifier:

  Exactly one effect-modifier column selected when the spec is
  registered by
  [`set_comparisons()`](https://bqmaks.github.io/bqreport/reference/set_comparisons.md).

## Value

A conditional-effect `contrast_spec`.
