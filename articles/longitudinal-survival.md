# Longitudinal and survival workflows

## Longitudinal data

Long and wide inputs compile to the same internal long analysis frame
without mutating the original `bq_data`.

``` r

long <- expand.grid(
  id = factor(seq_len(12)), visit = factor(c("V0", "V1", "V2"),
    levels = c("V0", "V1", "V2"))
)
long$arm <- factor(ifelse(as.integer(long$id) <= 6, "Control", "Treatment"))
long$score <- 10 + as.integer(long$id) / 3 +
  as.numeric(long$visit) + (long$arm == "Treatment") * as.numeric(long$visit)

data <- tibble::as_tibble(long) |>
  as_bq_data() |>
  set_predictor(arm, type = "binary", reference = "Control") |>
  set_colors(arm, c(Control = "#4477AA", Treatment = "#CC6677")) |>
  set_longitudinal_design(id, visit, group = arm, layout = "long",
    baseline = "V0", time_scale = "categorical") |>
  add_longitudinal_outcome(score_change, values = score,
    baseline = "V0", type = "continuous")

result <- data |>
  plan_longitudinal(score_change, method = lmm_model(),
    comparisons = TRUE, adjust = "holm") |>
  validate_plan(data) |>
  run_analysis(data)
#> Warning in optwrap(optimizer, devfun, start, lower = lower, upper = upper, :
#> convergence code -4 from nloptwrap: NLOPT_ROUNDOFF_LIMITED: Roundoff errors led
#> to a breakdown of the optimization algorithm. In this case, the returned
#> minimum may still be useful. (e.g. this error occurs in NEWUOA if one tries to
#> achieve a tolerance too close to machine precision.)
#> Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
#> unable to evaluate scaled gradient
#> Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
#>   See ?lme4::convergence and ?lme4::troubleshooting.

estimates(result)
#> # A tibble: 6 × 22
#>   analysis_id                  outcome predictor stratum_label transformation_id
#>   <chr>                        <chr>   <chr>     <chr>         <chr>            
#> 1 analysis_c45a2262-35a6-4f50… score_… arm       NA            NA               
#> 2 analysis_c45a2262-35a6-4f50… score_… arm       NA            NA               
#> 3 analysis_c45a2262-35a6-4f50… score_… arm       NA            NA               
#> 4 analysis_c45a2262-35a6-4f50… score_… arm       NA            NA               
#> 5 analysis_c45a2262-35a6-4f50… score_… arm       NA            NA               
#> 6 analysis_c45a2262-35a6-4f50… score_… arm       NA            NA               
#> # ℹ 17 more variables: transformation_label <chr>, term <chr>, level <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>,
#> #   effect_measure <chr>, scale <chr>, n <int>, n_events <int>, method <chr>,
#> #   variance <chr>
contrasts(result)
#> # A tibble: 6 × 23
#>   analysis_id       outcome predictor contrast_id contrast numerator denominator
#>   <chr>             <chr>   <chr>     <chr>       <chr>    <chr>     <chr>      
#> 1 analysis_c45a226… score_… arm       contrast_f… Control… Control@… Control@V0 
#> 2 analysis_c45a226… score_… arm       contrast_c… Control… Control@… Control@V0 
#> 3 analysis_c45a226… score_… arm       contrast_9… Treatme… Treatmen… Treatment@…
#> 4 analysis_c45a226… score_… arm       contrast_8… Treatme… Treatmen… Control ch…
#> 5 analysis_c45a226… score_… arm       contrast_7… Treatme… Treatmen… Treatment@…
#> 6 analysis_c45a226… score_… arm       contrast_1… Treatme… Treatmen… Control ch…
#> # ℹ 16 more variables: modifier <chr>, modifier_level <chr>,
#> #   inner_contrast <chr>, outer_contrast <chr>, estimand <chr>,
#> #   exponentiated <lgl>, estimate <dbl>, std_error <dbl>,
#> #   std_error_scale <chr>, conf_low <dbl>, conf_high <dbl>, p_value <dbl>,
#> #   p_adjusted <dbl>, adjust_method <chr>, effect_measure <chr>, scale <chr>
```

Use
[`gee_model()`](https://bqmaks.github.io/bqreport/reference/lmm_model.md)
for a population-average estimand and
[`lmm_model()`](https://bqmaks.github.io/bqreport/reference/lmm_model.md)
for a mixed-model estimand. The choice is explicit and never treated as
interchangeable.

## Kaplan–Meier, RMST, and Cox regression

``` r

survival_data <- tibble::tibble(
  time = c(3, 5, 8, 10, 4, 7, 9, 12, 2, 6, 11, 13),
  event = c(1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1),
  arm = factor(rep(c("Control", "Treatment"), each = 6)),
  age = c(44, 57, 51, 63, 46, 55, 60, 49, 52, 67, 58, 45)
) |>
  as_bq_data() |>
  set_predictor(arm, type = "binary", reference = "Control") |>
  set_predictor(age, type = "continuous") |>
  set_colors(arm, c(Control = "#4477AA", Treatment = "#CC6677")) |>
  add_survival_outcome(os, time, event, event_value = 1, time_unit = "months")

km <- survival_data |>
  plan_kaplan_meier(os, groups = arm, times = c(3, 6, 9),
    quantiles = c(.25, .5), rmst_tau = 9,
    estimates = c("survival", "cumulative_risk")) |>
  validate_plan(survival_data) |>
  run_analysis(survival_data)

survival_estimates(km)
#> # A tibble: 20 × 18
#>    analysis_id  outcome group group_level  time n_risk n_event n_censor estimate
#>    <chr>        <chr>   <chr> <chr>       <dbl>  <int>   <int>    <int>    <dbl>
#>  1 analysis_15… os      arm   Control         3      6       1        0    0.833
#>  2 analysis_15… os      arm   Control         6      3       1        1    0.667
#>  3 analysis_15… os      arm   Control         9      1       1        1    0.333
#>  4 analysis_15… os      arm   Treatment       3      5       1        0    0.833
#>  5 analysis_15… os      arm   Treatment       6      5       1        0    0.667
#>  6 analysis_15… os      arm   Treatment       9      4       1        0    0.5  
#>  7 analysis_15… os      arm   Control         3      6       1        0    0.167
#>  8 analysis_15… os      arm   Control         6      3       1        1    0.333
#>  9 analysis_15… os      arm   Control         9      1       1        1    0.667
#> 10 analysis_15… os      arm   Treatment       3      5       1        0    0.167
#> 11 analysis_15… os      arm   Treatment       6      5       1        0    0.333
#> 12 analysis_15… os      arm   Treatment       9      4       1        0    0.5  
#> 13 analysis_15… os      arm   Control        NA     NA      NA       NA    8    
#> 14 analysis_15… os      arm   Treatment      NA     NA      NA       NA   11    
#> 15 analysis_15… os      arm   Control        NA     NA      NA       NA    4    
#> 16 analysis_15… os      arm   Control        NA     NA      NA       NA    8    
#> 17 analysis_15… os      arm   Treatment      NA     NA      NA       NA    6    
#> 18 analysis_15… os      arm   Treatment      NA     NA      NA       NA   11    
#> 19 analysis_15… os      arm   Control        NA     NA      NA       NA    6.83 
#> 20 analysis_15… os      arm   Treatment      NA     NA      NA       NA    7.33 
#> # ℹ 9 more variables: std_error <dbl>, conf_low <dbl>, conf_high <dbl>,
#> #   estimate_type <chr>, quantile_probability <dbl>, restriction_time <dbl>,
#> #   scale <chr>, time_unit <chr>, method <chr>

cox <- survival_data |>
  plan_survival(os, arm, covariates = age) |>
  validate_plan(survival_data) |>
  run_analysis(survival_data)

estimates(cox)
#> # A tibble: 2 × 22
#>   analysis_id                  outcome predictor stratum_label transformation_id
#>   <chr>                        <chr>   <chr>     <chr>         <chr>            
#> 1 analysis_720bb61d-64a6-4491… os      arm       NA            NA               
#> 2 analysis_720bb61d-64a6-4491… os      arm       NA            NA               
#> # ℹ 17 more variables: transformation_label <chr>, term <chr>, level <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>,
#> #   effect_measure <chr>, scale <chr>, n <int>, n_events <int>, method <chr>,
#> #   variance <chr>
diagnostics(cox)
#> # A tibble: 3 × 5
#>   analysis_id                                   metric value status   message
#>   <chr>                                         <chr>  <dbl> <chr>    <chr>  
#> 1 analysis_720bb61d-64a6-4491-9217-980fd567edf3 arm    0.212 observed NA     
#> 2 analysis_720bb61d-64a6-4491-9217-980fd567edf3 age    0.106 observed NA     
#> 3 analysis_720bb61d-64a6-4491-9217-980fd567edf3 GLOBAL 0.224 observed NA
```

Competing-risk outcomes use
[`add_competing_risk_outcome()`](https://bqmaks.github.io/bqreport/reference/add_competing_risk_outcome.md)
and
[`plan_cumulative_incidence()`](https://bqmaks.github.io/bqreport/reference/plan_cumulative_incidence.md),
which estimate Aalen–Johansen cumulative incidence rather than
substituting `1 - S(t)`.
