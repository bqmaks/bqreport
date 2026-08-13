# Construct an atomic custom analysis function

Construct an atomic custom analysis function

## Usage

``` r
analysis_function(
  id,
  run,
  effect_measure,
  scale,
  required_packages = character(),
  exponentiate = FALSE,
  model_scale = scale,
  fallback = NULL,
  advance_on = character()
)
```

## Arguments

- id:

  Stable method identifier.

- run:

  Function accepting a read-only `analysis_context` and returning an
  [`analysis_output()`](https://bqmaks.github.io/bqreport/reference/analysis_output.md).

- effect_measure:

  Declared effect measure.

- scale:

  Declared estimate scale.

- required_packages:

  Packages required before execution.

- exponentiate:

  Whether the normalization layer exponentiates estimates, confidence
  limits, and contrasts returned on `model_scale`.

- model_scale:

  Scale returned by the custom callbacks before optional exponentiation.

- fallback:

  Optional named list of additional custom methods attempted
  sequentially after an explicitly allowed failure of `run`.

- advance_on:

  Condition classes that permit advancing to the next declared method.
  Contract violations never permit advancement.

## Value

A concrete custom `method_spec`.
