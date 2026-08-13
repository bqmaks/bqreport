# Compare each level with the global mean

Compare each level with the global mean

## Usage

``` r
against_global_mean(exponentiate, weights = c("equal", "observed"))
```

## Arguments

- exponentiate:

  Whether to exponentiate the model-scale contrast. This argument is
  required so ratio-scale interpretation is never implicit.

- weights:

  How levels contribute to the global mean: equally or in proportion to
  their observed analysis counts.

## Value

A backend-independent `contrast_spec`.
