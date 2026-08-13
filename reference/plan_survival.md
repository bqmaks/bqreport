# Compile a Cox survival analysis plan

Compile a Cox survival analysis plan

## Usage

``` r
plan_survival(
  .data,
  outcomes = tidyselect::everything(),
  predictors = all_predictors(),
  covariates = tidyselect::any_of(character()),
  effect_modifiers = tidyselect::any_of(character()),
  confidence_level = 0.95,
  method = cox_model()
)
```

## Arguments

- .data:

  A `bq_data` object.

- outcomes:

  Composite survival outcomes selected with tidyselect syntax.

- predictors:

  Predictor columns selected with tidyselect syntax.

- covariates:

  Optional adjustment covariates selected with tidyselect.

- effect_modifiers:

  Optional variables interacting with the predictor.

- confidence_level:

  Confidence level.

- method:

  A concrete Cox method specification.

## Value

An `analysis_plan` tibble.
