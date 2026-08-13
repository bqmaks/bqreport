# Construct a structured custom analysis method

Construct a structured custom analysis method

## Usage

``` r
analysis_method(
  id,
  fit,
  tidy_estimates,
  tidy_tests = NULL,
  compute_contrasts = NULL,
  diagnose = NULL,
  effect_measure,
  scale,
  required_packages = character(),
  exponentiate = FALSE,
  model_scale = scale
)
```

## Arguments

- id:

  Stable method identifier.

- fit:

  Fit callback.

- tidy_estimates:

  Estimate callback.

- tidy_tests:

  Test callback.

- compute_contrasts:

  Contrast callback.

- diagnose:

  Diagnostic callback.

- effect_measure:

  Declared effect measure.

- scale:

  Declared estimate scale.

- required_packages:

  Required packages.

- exponentiate:

  Whether to exponentiate normalized outputs.

- model_scale:

  Scale returned by callbacks before transformation.

## Value

A custom `method_spec`.
