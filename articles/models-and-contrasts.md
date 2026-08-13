# Regression models, transformations, and contrasts

``` r

trial <- tibble::tibble(
  score = c(10, 12, 13, 15, 17, 18, 20, 22, 23, 25, 27, 30),
  arm = factor(rep(c("Control", "Low", "High"), each = 4),
    levels = c("Control", "Low", "High")),
  age = c(40, 51, 47, 60, 42, 55, 49, 63, 45, 53, 58, 61)
) |>
  as_bq_data() |>
  set_outcome(score, type = "continuous") |>
  set_predictor(arm, type = "nominal", reference = "Control") |>
  set_predictor(age, type = "continuous") |>
  set_comparisons(arm, against_reference("Control"), adjust = "holm") |>
  set_colors(arm, c(Control = "#4477AA", Low = "#DDCC77", High = "#CC6677"))

plan <- trial |>
  plan_analysis(score, arm, covariates = age) |>
  validate_plan(trial)
result <- run_analysis(plan, trial)

estimates(result)
#> # A tibble: 4 × 22
#>   analysis_id                  outcome predictor stratum_label transformation_id
#>   <chr>                        <chr>   <chr>     <chr>         <chr>            
#> 1 analysis_aba68623-2c72-4ac1… score   arm       NA            NA               
#> 2 analysis_aba68623-2c72-4ac1… score   arm       NA            NA               
#> 3 analysis_aba68623-2c72-4ac1… score   arm       NA            NA               
#> 4 analysis_aba68623-2c72-4ac1… score   arm       NA            NA               
#> # ℹ 17 more variables: transformation_label <chr>, term <chr>, level <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>,
#> #   effect_measure <chr>, scale <chr>, n <int>, n_events <int>, method <chr>,
#> #   variance <chr>
contrasts(result)
#> # A tibble: 2 × 19
#>   analysis_id       outcome predictor contrast_id contrast numerator denominator
#>   <chr>             <chr>   <chr>     <chr>       <chr>    <chr>     <chr>      
#> 1 analysis_aba6862… score   arm       contrast_a… Low - C… Low       Control    
#> 2 analysis_aba6862… score   arm       contrast_a… High - … High      Control    
#> # ℹ 12 more variables: modifier <chr>, modifier_level <chr>, estimate <dbl>,
#> #   std_error <dbl>, std_error_scale <chr>, conf_low <dbl>, conf_high <dbl>,
#> #   p_value <dbl>, p_adjusted <dbl>, adjust_method <chr>, effect_measure <chr>,
#> #   scale <chr>
table_body(tbl_comparison(result))
#> # A tibble: 2 × 7
#>   outcome predictor contrast       estimate conf_int      p_value p_adjusted
#>   <chr>   <chr>     <chr>          <chr>    <chr>         <chr>   <chr>     
#> 1 score   arm       Low - Control  6.04     3.82 – 8.25   <0.001  <0.001    
#> 2 score   arm       High - Control 12.52    10.25 – 14.78 <0.001  <0.001
```

Available target specifications include
[`all_pairwise()`](https://bqmaks.github.io/bqreport/reference/all_pairwise.md),
[`against_reference()`](https://bqmaks.github.io/bqreport/reference/against_reference.md),
[`consecutive_comparisons()`](https://bqmaks.github.io/bqreport/reference/consecutive_comparisons.md),
and
[`against_global_mean()`](https://bqmaks.github.io/bqreport/reference/against_global_mean.md).
Model coding and target comparisons remain separate.

## Explicit nonlinear covariates

Transformations and model terms are metadata and are compiled into the
plan.

``` r

trial <- trial |>
  set_transformation(age, log10_transform()) |>
  set_model_term(age, natural_spline(df = 3))

plan <- plan_analysis(trial, score, arm, covariates = age)
```

This represents a sequential log transformation followed by a spline
basis. The transformation identity and basis settings are included in
provenance.

## Other outcome families

System defaults cover linear, logistic, Poisson, proportional-odds
ordinal, and baseline-category multinomial models. Alternatives can be
selected with rules, for example:

``` r

rules <- analysis_rules(
  where_count() ~ negative_binomial_model(exponentiate = TRUE)
)

plan_analysis(data, outcomes = events, predictors = treatment, rules = rules)
```

Exponentiation is always explicit in the selected method specification;
SEs remain on their declared model scale.
