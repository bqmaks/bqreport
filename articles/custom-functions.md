# Custom functions, methods, and selectors

Extensions are introduced through constructors with validated outputs.
The package does not infer the meaning of an arbitrary return value.

## A custom correlation method

``` r

cosine_correlation <- correlation_method(
  id = "cosine_similarity",
  effect_measure = "cosine_similarity",
  scale = "minus_one_to_one",
  ci_method = "none",
  supports_strata = TRUE,
  compute = function(context) {
    estimate <- sum(context$x * context$y) /
      sqrt(sum(context$x^2) * sum(context$y^2))
    correlation_output(
      estimate = estimate,
      std_error = NA_real_, std_error_scale = "similarity",
      conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, df = NA_real_, p_value = NA_real_
    )
  }
)

data <- as_bq_data(tibble::tibble(x = 1:8, y = c(1, 3, 2, 5, 4, 7, 6, 8)))
result <- data |>
  plan_correlations(x, with = y, method = cosine_correlation) |>
  validate_plan(data) |>
  run_analysis(data)
correlations(result)
#> # A tibble: 1 × 43
#>   analysis_id       correlation_family_id variable_x_id variable_y_id variable_x
#>   <chr>             <chr>                 <chr>         <chr>         <chr>     
#> 1 analysis_f456ce1… correlation_family_2… var_f3a0b123… var_23147973… x         
#> # ℹ 38 more variables: variable_y <chr>, stratum_label <chr>, strata <list>,
#> #   correlation_interaction_id <chr>, interaction_test <lgl>,
#> #   correlation_comparator <list>, correlation_comparator_id <chr>,
#> #   transformation_x <chr>, transformation_y <chr>,
#> #   adjustment_variables <list>, n_adjustment <int>, estimand <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>, …
```

The callback receives only the prepared analysis context. Its identity
and hash are written to the plan and provenance.

## A complete custom analysis function

This example estimates relative risks with a Poisson log-link model and
robust interpretation declared in advance.

``` r

custom_rr <- analysis_function(
  id = "custom_relative_risk",
  effect_measure = "risk_ratio",
  scale = "ratio",
  run = function(context) {
    fit <- stats::glm(context$formula, data = context$model_frame,
      family = stats::poisson("log"))
    sm <- summary(fit)$coefficients
    critical <- stats::qnorm((1 + context$confidence_level) / 2)
    analysis_output(
      model = fit,
      estimates = tibble::tibble(
        analysis_id = context$analysis_id,
        outcome = context$outcome_spec$name,
        predictor = context$predictor_spec$name,
        term = rownames(sm), level = NA_character_,
        estimate = exp(unname(sm[, "Estimate"])),
        std_error = unname(sm[, "Std. Error"]),
        std_error_scale = "log_risk",
        conf_low = exp(unname(sm[, "Estimate"] - critical * sm[, "Std. Error"])),
        conf_high = exp(unname(sm[, "Estimate"] + critical * sm[, "Std. Error"])),
        statistic = unname(sm[, "z value"]), df = NA_real_,
        p_value = unname(sm[, "Pr(>|z|)"]),
        effect_measure = "risk_ratio", scale = "ratio",
        n = as.integer(stats::nobs(fit)),
        n_events = as.integer(sum(context$response == 1)),
        method = "custom_relative_risk", variance = "model_based"
      )
    )
  }
)

binary <- tibble::tibble(
  outcome = c(0, 0, 1, 0, 1, 1, 1, 0),
  exposed = c(0, 0, 0, 1, 1, 1, 1, 0)
) |>
  as_bq_data() |>
  set_outcome(outcome, type = "binary", event = 1) |>
  set_predictor(exposed, type = "continuous")

rules <- analysis_rules(where_binary() ~ custom_rr)
custom_result <- binary |>
  plan_analysis(outcome, exposed, rules = rules) |>
  validate_plan(binary) |>
  run_analysis(binary)

estimates(custom_result)
#> # A tibble: 2 × 22
#>   analysis_id                  outcome predictor stratum_label transformation_id
#>   <chr>                        <chr>   <chr>     <chr>         <chr>            
#> 1 analysis_966a43c3-22e2-4bd9… outcome exposed   NA            NA               
#> 2 analysis_966a43c3-22e2-4bd9… outcome exposed   NA            NA               
#> # ℹ 17 more variables: transformation_label <chr>, term <chr>, level <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>,
#> #   effect_measure <chr>, scale <chr>, n <int>, n_events <int>, method <chr>,
#> #   variance <chr>
provenance <- custom_result$provenance
provenance[, c("function_id", "function_hash")]
#> # A tibble: 1 × 2
#>   function_id          function_hash                   
#>   <chr>                <chr>                           
#> 1 custom_relative_risk 9c255581ec19067d7fa6168f6857bbad
```

Malformed output becomes a typed issue; the runtime does not guess
missing columns or silently switch engines.

## Data-dependent selection before fitting

Selectors choose only among declared candidates during preflight.

``` r

sample_size_policy <- method_selector(
  id = "sample_size_policy",
  candidates = list(ordinary = linear_model(), robust = linear_model()),
  select = function(context) {
    n <- nrow(context$model_frame)
    method_choice(
      method = if (n >= 30) "ordinary" else "robust",
      reason = paste("Analyzed sample size:", n),
      diagnostics = tibble::tibble(n = n)
    )
  }
)
```

The selected concrete method, reason, diagnostics, callback hash, and
package requirements are inspectable before
[`run_analysis()`](https://bqmaks.github.io/bqreport/reference/run_analysis.md).
A fit failure never causes the selector to run again.
