# Construct a custom comparison function

Construct a custom comparison function

## Usage

``` r
comparison_function(
  id,
  compute,
  effect_measure,
  scale,
  model_scale = scale,
  exponentiate = FALSE,
  required_packages = character()
)
```

## Arguments

- id:

  Stable function identifier.

- compute:

  Function receiving `(model, context)` and returning a normalized
  contrasts data frame.

- effect_measure:

  Declared effect measure.

- scale:

  Final output scale.

- model_scale:

  Scale returned by `compute` before normalization.

- exponentiate:

  Whether normalization exponentiates estimate and limits.

- required_packages:

  Required packages checked during preflight.

## Value

A backend-independent `contrast_spec`.
