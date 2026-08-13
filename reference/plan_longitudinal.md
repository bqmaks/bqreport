# Compile a longitudinal analysis plan

Compile a longitudinal analysis plan

## Usage

``` r
plan_longitudinal(
  .data,
  outcomes = tidyselect::everything(),
  method = lmm_model(),
  confidence_level = 0.95,
  comparisons = TRUE,
  adjust = "none"
)
```

## Arguments

- .data:

  A `bq_data` object with one longitudinal design.

- outcomes:

  Registered longitudinal outcomes selected by name.

- method:

  A longitudinal method specification.

- confidence_level:

  Confidence level.

- comparisons:

  Whether to compute change-from-baseline and difference-in-changes
  estimands.

- adjust:

  Multiplicity adjustment for the longitudinal contrast family.

## Value

An `analysis_plan` with one row per outcome.
