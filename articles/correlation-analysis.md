# Correlation analysis

``` r

data <- tibble::tibble(
  x = c(1, 2, 3, 4, 5, 6, 7, 8),
  y = c(1.2, 1.7, 3.4, 3.8, 5.3, 5.7, 7.4, 7.8),
  age = c(40, 52, 47, 60, 44, 56, 49, 63),
  arm = factor(rep(c("Control", "Treatment"), each = 4))
) |>
  as_bq_data() |>
  set_role(arm, "stratum")

result <- data |>
  plan_correlations(x, with = y, adjust_for = age,
    method = pearson_correlation()) |>
  validate_plan(data) |>
  run_analysis(data)

correlations(result)
#> # A tibble: 1 × 43
#>   analysis_id       correlation_family_id variable_x_id variable_y_id variable_x
#>   <chr>             <chr>                 <chr>         <chr>         <chr>     
#> 1 analysis_384a811… correlation_family_3… var_1cb463cb… var_82370649… x         
#> # ℹ 38 more variables: variable_y <chr>, stratum_label <chr>, strata <list>,
#> #   correlation_interaction_id <chr>, interaction_test <lgl>,
#> #   correlation_comparator <list>, correlation_comparator_id <chr>,
#> #   transformation_x <chr>, transformation_y <chr>,
#> #   adjustment_variables <list>, n_adjustment <int>, estimand <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>, …
table_body(tbl_correlation(result))
#> # A tibble: 1 × 6
#>   predictor group estimate conf_int    p_value method 
#>   <chr>     <chr> <chr>    <chr>       <chr>   <chr>  
#> 1 x × y     NA    1.00     0.98 – 1.00 <0.001  pearson
```

Built-in methods include Pearson, Spearman, Kendall, biweight, weighted
Pearson, repeated-measures, polychoric, and tetrachoric correlations.

## Resampling inference

``` r

resampled <- resampled_correlation(
  pearson_correlation(), bootstrap = 49, permutations = 49, seed = 2026
)

resampled_result <- data |>
  plan_correlations(x, with = y, method = resampled) |>
  validate_plan(data) |>
  run_analysis(data)

correlations(resampled_result)[, c("estimate", "std_error", "conf_low",
  "conf_high", "p_value", "bootstrap_successful", "permutation_successful")]
#> # A tibble: 1 × 7
#>   estimate std_error conf_low conf_high p_value bootstrap_successful
#>      <dbl>     <dbl>    <dbl>     <dbl>   <dbl>                <int>
#> 1    0.992   0.00478    0.980     0.999    0.02                   49
#> # ℹ 1 more variable: permutation_successful <int>
```

The seed and successful replicate counts are recorded without changing
the global RNG state.

## Strata and interaction

``` r

stratified <- data |>
  plan_correlations(x, with = y, strata = arm,
    interaction_test = TRUE, adjust = "holm") |>
  validate_plan(data) |>
  run_analysis(data)

correlations(stratified)
#> # A tibble: 2 × 43
#>   analysis_id       correlation_family_id variable_x_id variable_y_id variable_x
#>   <chr>             <chr>                 <chr>         <chr>         <chr>     
#> 1 analysis_ef04f27… correlation_family_c… var_1cb463cb… var_82370649… x         
#> 2 analysis_d5e25d9… correlation_family_3… var_1cb463cb… var_82370649… x         
#> # ℹ 38 more variables: variable_y <chr>, stratum_label <chr>, strata <list>,
#> #   correlation_interaction_id <chr>, interaction_test <lgl>,
#> #   correlation_comparator <list>, correlation_comparator_id <chr>,
#> #   transformation_x <chr>, transformation_y <chr>,
#> #   adjustment_variables <list>, n_adjustment <int>, estimand <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>, …
tests(stratified)
#> # A tibble: 1 × 13
#>   analysis_id   outcome predictor contrast numerator denominator test  statistic
#>   <chr>         <chr>   <chr>     <chr>    <chr>     <chr>       <chr>     <dbl>
#> 1 correlation_… x       y         NA       NA        NA          corr…   0.00211
#> # ℹ 5 more variables: df <dbl>, p_value <dbl>, p_adjusted <dbl>,
#> #   adjust_method <chr>, method <chr>
contrasts(stratified)
#> # A tibble: 1 × 23
#>   analysis_id       outcome predictor contrast_id contrast numerator denominator
#>   <chr>             <chr>   <chr>     <chr>       <chr>    <chr>     <chr>      
#> 1 correlation_inte… x       y         contrast_5… arm=Con… arm=Cont… arm=Treatm…
#> # ℹ 16 more variables: modifier <chr>, modifier_level <chr>,
#> #   inner_contrast <chr>, outer_contrast <chr>, estimand <chr>,
#> #   exponentiated <lgl>, estimate <dbl>, std_error <dbl>,
#> #   std_error_scale <chr>, conf_low <dbl>, conf_high <dbl>, p_value <dbl>,
#> #   p_adjusted <dbl>, adjust_method <chr>, effect_measure <chr>, scale <chr>
```

The omnibus test and pairwise differences compare correlations across
strata; they are distinct from merely reporting separate stratified
estimates.
