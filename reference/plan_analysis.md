# Compile a univariable analysis plan

`plan_analysis()` creates one inspectable task for each selected
outcome-predictor pair. The initial vertical slice supports continuous
outcomes with linear models and binary outcomes with logistic models.

## Usage

``` r
plan_analysis(
  .data,
  outcomes = all_outcomes(),
  predictors = all_predictors(),
  covariates = tidyselect::any_of(character()),
  weights = tidyselect::any_of(character()),
  cluster = tidyselect::any_of(character()),
  strata = tidyselect::any_of(character()),
  effect_modifiers = tidyselect::any_of(character()),
  variance = NULL,
  rules = NULL,
  confidence_level = 0.95
)
```

## Arguments

- .data:

  A `bq_data` object.

- outcomes:

  Outcome columns selected with tidyselect.

- predictors:

  Predictor columns selected with tidyselect.

- covariates:

  Optional adjustment covariates selected with tidyselect.

- weights:

  Optional single configured weight column.

- cluster:

  Optional matched-set cluster column.

- strata:

  Optional columns defining independent analysis strata. Only observed
  complete combinations are compiled.

- effect_modifiers:

  Optional variables interacting with the predictor.

- variance:

  Variance estimator. Defaults to `robust` for IPW and `model_based`
  otherwise.

- rules:

  Optional concrete `analysis_rules`.

- confidence_level:

  Confidence level in the open interval `(0, 1)`.

## Value

An `analysis_plan` tibble.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  response = c(0, 0, 1, 0, 1, 1, 1, 0),
  treatment = factor(rep(c("Control", "Treatment"), each = 4)),
  age = c(44, 57, 51, 63, 46, 55, 60, 49)
)) |>
  set_outcome(response, type = "binary", event = 1) |>
  set_predictor(treatment, type = "binary", reference = "Control") |>
  set_predictor(age, type = "continuous")
plan <- plan_analysis(data, response, treatment, covariates = age)
plan[, c("analysis_id", "outcome", "predictor", "method", "status")]
#> # A tibble: 1 × 5
#>   analysis_id               outcome  predictor method         status
#>   <chr>                     <chr>    <chr>     <chr>          <chr> 
#> 1 analysis_be2bd84b98cc7388 response treatment logistic_model ready 
```
