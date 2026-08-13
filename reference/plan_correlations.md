# Compile a correlation analysis plan

Compile a correlation analysis plan

## Usage

``` r
plan_correlations(
  .data,
  variables = where_continuous(),
  with = NULL,
  adjust_for = tidyselect::any_of(character()),
  strata = tidyselect::any_of(character()),
  weights = tidyselect::any_of(character()),
  id = tidyselect::any_of(character()),
  interaction_test = FALSE,
  comparator = NULL,
  method = pearson_correlation(),
  missing = c("pairwise", "complete"),
  confidence_level = 0.95,
  adjust = "none"
)
```

## Arguments

- .data:

  A `bq_data` object.

- variables:

  Numeric variables selected with tidyselect.

- with:

  Optional second variable set. If omitted, unique pairs within
  `variables` are compiled.

- adjust_for:

  Optional numeric covariates for partial correlation.

- strata:

  Optional variables defining independent correlation strata.

- weights:

  Optional numeric analysis-weight column.

- id:

  Optional subject identifier for repeated-measures methods.

- interaction_test:

  Whether to test equality of Pearson correlations across strata and
  compute pairwise Fisher z contrasts.

- comparator:

  Optional comparator used when `interaction_test = TRUE`.

- method:

  A correlation method specification.

- missing:

  Pairwise or common complete-case analysis.

- confidence_level:

  Confidence level.

- adjust:

  Multiplicity adjustment accepted by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

## Value

An `analysis_plan` with one row per unique variable pair.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  crp = c(1.2, 5.4, 2.8, 9.1, 4.4, 6.0),
  bmi = c(21.4, 27.9, 24.2, 30.1, 26.6, 23.0)
))
result <- plan_correlations(data, crp, with = bmi) |>
  validate_plan(data) |>
  run_analysis(data)
correlations(result)
#> # A tibble: 1 × 43
#>   analysis_id       correlation_family_id variable_x_id variable_y_id variable_x
#>   <chr>             <chr>                 <chr>         <chr>         <chr>     
#> 1 analysis_d4f7fab… correlation_family_e… var_crp       var_bmi       crp       
#> # ℹ 38 more variables: variable_y <chr>, stratum_label <chr>, strata <list>,
#> #   correlation_interaction_id <chr>, interaction_test <lgl>,
#> #   correlation_comparator <list>, correlation_comparator_id <chr>,
#> #   transformation_x <chr>, transformation_y <chr>,
#> #   adjustment_variables <list>, n_adjustment <int>, estimand <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>, …
```
