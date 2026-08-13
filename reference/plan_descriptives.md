# Compile a descriptive analysis plan

`plan_descriptives()` creates one inspectable task per selected
variable. Results may be computed for the complete population, levels of
one grouping variable, or both. The grouping label used by a report is
deliberately kept out of the numerical result.

## Usage

``` r
plan_descriptives(
  .data,
  variables = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  overall = TRUE,
  confidence_level = 0.95,
  functions = list(),
  comparisons = FALSE,
  contrasts = NULL,
  adjust = "none"
)
```

## Arguments

- .data:

  A `bq_data` object.

- variables:

  Variables selected with tidyselect.

- groups:

  Optional single grouping variable selected with tidyselect.

- overall:

  Whether to include statistics for the complete population.

- confidence_level:

  Confidence level reserved for model-based providers.

- functions:

  A list of explicit `descriptive_function` providers.

- comparisons:

  Whether to estimate an effect between two groups.

- contrasts:

  Target group comparisons. Supports
  [`against_reference()`](https://bqmaks.github.io/bqreport/reference/against_reference.md),
  [`all_pairwise()`](https://bqmaks.github.io/bqreport/reference/all_pairwise.md),
  and
  [`consecutive_comparisons()`](https://bqmaks.github.io/bqreport/reference/consecutive_comparisons.md).

- adjust:

  Multiplicity adjustment accepted by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

## Value

An `analysis_plan` tibble.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  bmi = c(21.4, 27.9, 24.2, 30.1, 26.6, 23.0),
  arm = factor(rep(c("A", "B"), 3))
)) |>
  set_outcome(bmi, type = "continuous") |>
  set_role(arm, "group")
result <- plan_descriptives(data, bmi, groups = arm) |>
  validate_plan(data) |>
  run_analysis(data)
descriptives(result)
#> # A tibble: 39 × 18
#>    analysis_id     variable_id variable variable_type group_id group group_level
#>    <chr>           <chr>       <chr>    <chr>         <chr>    <chr> <chr>      
#>  1 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  2 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  3 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  4 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  5 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  6 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  7 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  8 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#>  9 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#> 10 analysis_1522f… var_bmi     bmi      continuous    var_arm  arm   NA         
#> # ℹ 29 more rows
#> # ℹ 11 more variables: overall <lgl>, level <chr>, statistic <chr>,
#> #   value <dbl>, numerator <int>, denominator <int>, statistic_method <chr>,
#> #   source <chr>, method <chr>, status <chr>, message <chr>
```
