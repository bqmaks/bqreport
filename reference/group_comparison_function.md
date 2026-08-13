# Construct a custom descriptive group comparison

Construct a custom descriptive group comparison

## Usage

``` r
group_comparison_function(
  id,
  types,
  effect_measure,
  scale,
  compute,
  ci_method = "custom",
  required_packages = character()
)
```

## Arguments

- id:

  Stable comparison identifier.

- types:

  Supported analytical variable types.

- effect_measure:

  Declared effect measure.

- scale:

  Declared result scale.

- compute:

  Function accepting a read-only `group_comparison_context` and
  returning a value created by
  [`group_comparison_output()`](https://bqmaks.github.io/bqreport/reference/group_comparison_output.md).

- ci_method:

  Confidence interval implementation identifier.

- required_packages:

  Required packages checked during preflight.

## Value

A `group_comparison_spec`.
