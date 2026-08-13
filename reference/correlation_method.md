# Construct a custom correlation method

Construct a custom correlation method

## Usage

``` r
correlation_method(
  id,
  compute,
  effect_measure,
  scale = "minus_one_to_one",
  ci_method,
  supports_partial = FALSE,
  supports_strata = TRUE,
  supports_interaction = FALSE,
  supports_weights = FALSE,
  supports_id = FALSE,
  required_packages = character()
)
```

## Arguments

- id:

  Stable method identifier.

- compute:

  Function accepting a read-only `correlation_context` and returning a
  value constructed by
  [`correlation_output()`](https://bqmaks.github.io/bqreport/reference/correlation_output.md).

- effect_measure:

  Declared effect measure.

- scale:

  Output scale.

- ci_method:

  Confidence-interval method identifier.

- supports_partial, supports_strata, supports_interaction:

  Declared method capabilities checked before compilation.

- supports_weights, supports_id:

  Whether optional weights or subject IDs may be supplied to the method.

- required_packages:

  Optional packages checked during preflight.

## Value

A validated `correlation_method_spec`.
