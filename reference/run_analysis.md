# Run a validated analysis plan

Only validated tasks with status `ready` are executed. Other tasks are
retained in the result plan and represented in the issues component.
Engine failures never trigger an undeclared fallback method. Explicit
[`analysis_method_chain()`](https://bqmaks.github.io/bqreport/reference/analysis_method_chain.md)
objects retain every runtime attempt.

## Usage

``` r
run_analysis(plan, data, error = c("collect", "stop", "warn"))
```

## Arguments

- plan:

  A validated `analysis_plan`.

- data:

  A `bq_data` object.

- error:

  Runtime engine error handling: collect, stop, or warn.

## Value

An `analysis_result`.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  bmi = c(21.4, 27.9, 24.2, 30.1, 26.6, 23.0, 28.4, 22.1),
  treatment = factor(rep(c("Control", "Treatment"), 4))
)) |>
  set_outcome(bmi, type = "continuous") |>
  set_predictor(treatment, type = "binary", reference = "Control")
result <- plan_analysis(data, bmi, treatment) |>
  validate_plan(data) |>
  run_analysis(data)
estimates(result)
#> # A tibble: 2 × 22
#>   analysis_id               outcome predictor stratum_label transformation_id
#>   <chr>                     <chr>   <chr>     <chr>         <chr>            
#> 1 analysis_64bfe250ee6b5bb2 bmi     treatment NA            NA               
#> 2 analysis_64bfe250ee6b5bb2 bmi     treatment NA            NA               
#> # ℹ 17 more variables: transformation_label <chr>, term <chr>, level <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>,
#> #   effect_measure <chr>, scale <chr>, n <int>, n_events <int>, method <chr>,
#> #   variance <chr>
issues(result)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: analysis_id <chr>, stage <chr>, severity <chr>,
#> #   condition_class <chr>, message <chr>
```
