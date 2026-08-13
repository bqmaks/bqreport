# Get started with bqreport

`bqreport` treats analysis as a compilation pipeline. Data and metadata
are compiled into an inspectable plan; only a validated plan can be
executed.

## Register data and metadata

``` r

trial <- tibble::tibble(
  response = c(0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
  treatment = factor(rep(c("Control", "Treatment"), each = 6)),
  age = c(44, 57, 51, 63, 46, 55, 60, 49, 52, 67, 58, 45),
  biomarker = c(2.1, 3.4, 2.7, 4.2, 3.0, 3.8, 4.6, 3.9, 5.1, 4.8, 5.4, 4.2)
) |>
  as_bq_data() |>
  set_outcome(response, type = "binary", event = 1) |>
  set_predictor(treatment, type = "binary", reference = "Control") |>
  set_role(treatment, "group") |>
  set_predictor(c(age, biomarker), type = "continuous") |>
  set_colors(treatment, c(Control = "#4477AA", Treatment = "#CC6677"))

variables(trial)[, c("name", "role", "type", "reference", "status")]
#> # A tibble: 4 × 5
#>   name      role      type       reference status
#>   <chr>     <list>    <chr>      <list>    <chr> 
#> 1 response  <chr [1]> binary     <NULL>    valid 
#> 2 treatment <chr [2]> binary     <chr [1]> valid 
#> 3 age       <chr [1]> continuous <NULL>    valid 
#> 4 biomarker <chr [1]> continuous <NULL>    valid
```

Roles are additive, so `treatment` is both a grouping variable and
predictor. Stable `var_id` values allow registry references to survive
renaming.

## Compile and review a plan

``` r

plan <- trial |>
  plan_analysis(
    outcomes = response,
    predictors = treatment,
    covariates = age
  ) |>
  validate_plan(trial)

plan[, c("analysis_id", "formula", "method", "effect_measure", "scale",
         "n_analyzed", "status")]
#> # A tibble: 1 × 7
#>   analysis_id            formula   method effect_measure scale n_analyzed status
#>   <chr>                  <list>    <chr>  <chr>          <chr>      <int> <chr> 
#> 1 analysis_be2bd84b98cc… <formula> logis… odds_ratio     ratio         12 ready
```

At this point the method is concrete. A data-dependent selector, if
used, has already run and recorded its reason and diagnostics.

## Execute and inspect tidy components

``` r

result <- run_analysis(plan, trial)
estimates(result)
#> # A tibble: 3 × 22
#>   analysis_id               outcome  predictor stratum_label transformation_id
#>   <chr>                     <chr>    <chr>     <chr>         <chr>            
#> 1 analysis_be2bd84b98cc7388 response treatment NA            NA               
#> 2 analysis_be2bd84b98cc7388 response treatment NA            NA               
#> 3 analysis_be2bd84b98cc7388 response treatment NA            NA               
#> # ℹ 17 more variables: transformation_label <chr>, term <chr>, level <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>,
#> #   effect_measure <chr>, scale <chr>, n <int>, n_events <int>, method <chr>,
#> #   variance <chr>
tests(result)
#> # A tibble: 2 × 8
#>   analysis_id             outcome predictor test  statistic    df p_value method
#>   <chr>                   <chr>   <chr>     <chr>     <dbl> <dbl>   <dbl> <chr> 
#> 1 analysis_be2bd84b98cc7… respon… treatment like…     1.37      2   0.503 logis…
#> 2 analysis_be2bd84b98cc7… respon… treatment pred…     0.612     1   0.434 logis…
diagnostics(result)
#> # A tibble: 4 × 5
#>   analysis_id               metric           value status   message
#>   <chr>                     <chr>            <dbl> <chr>    <chr>  
#> 1 analysis_be2bd84b98cc7388 converged         1    observed NA     
#> 2 analysis_be2bd84b98cc7388 deviance         14.9  observed NA     
#> 3 analysis_be2bd84b98cc7388 null_deviance    16.3  observed NA     
#> 4 analysis_be2bd84b98cc7388 dispersion_ratio  1.66 observed NA
issues(result)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: analysis_id <chr>, stage <chr>, severity <chr>,
#> #   condition_class <chr>, message <chr>
```

Numbers remain unrounded. Formatting is a separate operation:

``` r

table <- tbl_regression(result, locale = "en")
table_body(table)
#> # A tibble: 3 × 8
#>   outcome  predictor term        level     estimate conf_int      p_value method
#>   <chr>    <chr>     <chr>       <chr>     <chr>    <chr>         <chr>   <chr> 
#> 1 response treatment (Intercept) NA        117.90   0.01 – 19868… 0.337   logis…
#> 2 response treatment treatment   Treatment 2.69     0.21 – 34.35  0.446   logis…
#> 3 response treatment age         NA        0.91     0.76 – 1.10   0.330   logis…
```

Optional renderers convert the backend-independent table to `gt` or
`flextable`, while `plot_forest(result)` returns a normal ggplot object.
