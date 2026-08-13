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
#> 1 analysis_a7ca63a… correlation_family_8… var_8d91008e… var_de521b49… x         
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
#> 1 analysis_0c053145-0fb7-4b2f… outcome exposed   NA            NA               
#> 2 analysis_0c053145-0fb7-4b2f… outcome exposed   NA            NA               
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

## An explicit fallback chain

A runtime fallback is available only as an inspectable chain. Each next
method must have the same estimand and output scale, and is attempted
only for a condition class listed in `advance_on`.

``` r

primary_rr <- analysis_function(
  id = "primary_rr",
  effect_measure = "risk_ratio",
  scale = "ratio",
  run = function(context) {
    stop(structure(
      list(message = "Numerical method did not converge", call = NULL),
      class = c("bq_error_numerical_failure", "error", "condition")
    ))
  },
  fallback = list(poisson_rr = custom_rr),
  advance_on = "bq_error_numerical_failure"
)

fallback_result <- binary |>
  plan_analysis(
    outcome, exposed,
    rules = analysis_rules(where_binary() ~ primary_rr)
  ) |>
  validate_plan(binary) |>
  run_analysis(binary)

attempts(fallback_result)
#> # A tibble: 2 × 8
#>   analysis_id      chain_id attempt member method status condition_class message
#>   <chr>            <chr>      <int> <chr>  <chr>  <chr>  <chr>           <chr>  
#> 1 analysis_0431eb… primary…       1 prima… prima… failed bq_error_numer… Numeri…
#> 2 analysis_0431eb… primary…       2 poiss… custo… succe… NA              NA
fallback_result$provenance[, c(
  "method_chain", "fallback_conditions", "executed_method", "fallback_used"
)]
#> # A tibble: 1 × 4
#>   method_chain fallback_conditions executed_method      fallback_used
#>   <list>       <list>              <chr>                <lgl>        
#> 1 <chr [2]>    <chr [1]>           custom_relative_risk TRUE
```

`bq_error_invalid_engine_output` and other contract violations never
advance the chain. They indicate an implementation bug rather than an
expected statistical failure.

## A custom model in a three-group workflow

The next model keeps the ordinary least-squares estimand but replaces
the model-based covariance matrix with HC3 robust standard errors.
Because the method returns the stable estimates schema, it can be used
by the same planner and reporting layer as built-in models.

``` r

hc3_linear_model <- analysis_method(
  id = "hc3_linear_model",
  effect_measure = "mean_difference",
  scale = "identity",
  required_packages = "sandwich",

  fit = function(context) {
    stats::lm(
      context$formula,
      data = context$model_frame,
      na.action = stats::na.omit
    )
  },

  tidy_estimates = function(fit, context) {
    beta <- stats::coef(fit)
    covariance <- sandwich::vcovHC(fit, type = "HC3")
    standard_error <- sqrt(diag(covariance))
    degrees <- stats::df.residual(fit)
    critical <- stats::qt(
      (1 + context$confidence_level) / 2,
      df = degrees
    )
    statistic <- beta / standard_error

    tibble::tibble(
      analysis_id = context$analysis_id,
      outcome = context$outcome_spec$name,
      predictor = context$predictor_spec$name,
      term = names(beta),
      level = NA_character_,
      estimate = unname(beta),
      std_error = unname(standard_error),
      std_error_scale = "identity",
      conf_low = unname(beta - critical * standard_error),
      conf_high = unname(beta + critical * standard_error),
      statistic = unname(statistic),
      df = rep(degrees, length(beta)),
      p_value = unname(
        2 * stats::pt(abs(statistic), df = degrees, lower.tail = FALSE)
      ),
      effect_measure = "mean_difference",
      scale = "identity",
      n = rep(as.integer(stats::nobs(fit)), length(beta)),
      n_events = NA_integer_,
      method = "hc3_linear_model",
      variance = "HC3"
    )
  },

  tidy_tests = function(fit, context) {
    omnibus <- stats::anova(fit)
    predictor_row <- 1L

    tibble::tibble(
      analysis_id = context$analysis_id,
      outcome = context$outcome_spec$name,
      predictor = context$predictor_spec$name,
      contrast = NA_character_,
      numerator = NA_character_,
      denominator = NA_character_,
      test = "predictor_omnibus",
      statistic = unname(omnibus$`F value`[[predictor_row]]),
      df = unname(omnibus$Df[[predictor_row]]),
      p_value = unname(omnibus$`Pr(>F)`[[predictor_row]]),
      p_adjusted = NA_real_,
      adjust_method = "none",
      method = "hc3_linear_model"
    )
  },

  compute_contrasts = function(fit, context) {
    group_name <- context$predictor_spec$name[[1]]
    group_levels <- levels(context$model_frame[[group_name]])
    pairs <- t(utils::combn(seq_along(group_levels), 2L))

    reference_grid <- stats::setNames(
      list(factor(group_levels, levels = group_levels)),
      group_name
    ) |>
      as.data.frame()
    design <- stats::model.matrix(
      stats::delete.response(stats::terms(fit)),
      reference_grid
    )

    beta <- stats::coef(fit)
    covariance <- sandwich::vcovHC(fit, type = "HC3")
    degrees <- stats::df.residual(fit)
    critical <- stats::qt(
      (1 + context$confidence_level) / 2,
      df = degrees
    )

    contrast_matrix <- t(vapply(
      seq_len(nrow(pairs)),
      function(i) design[pairs[i, 2L], ] - design[pairs[i, 1L], ],
      numeric(ncol(design))
    ))
    estimate <- as.numeric(contrast_matrix %*% beta)
    standard_error <- sqrt(diag(
      contrast_matrix %*% covariance %*% t(contrast_matrix)
    ))
    statistic <- estimate / standard_error
    p_value <- 2 * stats::pt(
      abs(statistic), df = degrees, lower.tail = FALSE
    )
    comparison <- context$contrast_specs[1, , drop = FALSE]
    adjust <- comparison$adjust_method[[1]]
    numerator <- group_levels[pairs[, 2L]]
    denominator <- group_levels[pairs[, 1L]]

    tibble::tibble(
      analysis_id = context$analysis_id,
      outcome = context$outcome_spec$name,
      predictor = group_name,
      contrast_id = comparison$contrast_id[[1]],
      contrast = paste(numerator, "vs", denominator),
      numerator = numerator,
      denominator = denominator,
      estimate = estimate,
      std_error = standard_error,
      std_error_scale = "identity",
      conf_low = estimate - critical * standard_error,
      conf_high = estimate + critical * standard_error,
      p_value = p_value,
      p_adjusted = stats::p.adjust(p_value, method = adjust),
      adjust_method = adjust,
      effect_measure = "mean_difference",
      scale = "identity"
    )
  }
)
```

The model is then registered through an ordinary analysis rule.

``` r

three_groups <- tibble::tibble(
  arm = factor(
    rep(c("Control", "Treatment A", "Treatment B"), each = 8),
    levels = c("Control", "Treatment A", "Treatment B")
  ),
  score = c(
    42, 45, 48, 50, 51, 53, 54, 57,
    47, 49, 52, 55, 57, 58, 61, 64,
    51, 54, 56, 59, 62, 64, 67, 70
  )
) |>
  as_bq_data() |>
  set_outcome(score, type = "continuous") |>
  set_predictor(arm, type = "nominal", reference = "Control") |>
  set_comparisons(arm, all_pairwise(), adjust = "holm")

hc3_plan <- three_groups |>
  plan_analysis(
    outcomes = score,
    predictors = arm,
    rules = analysis_rules(where_continuous() ~ hc3_linear_model)
  ) |>
  validate_plan(three_groups)

hc3_result <- run_analysis(hc3_plan, three_groups)

estimates(hc3_result)
#> # A tibble: 3 × 22
#>   analysis_id                  outcome predictor stratum_label transformation_id
#>   <chr>                        <chr>   <chr>     <chr>         <chr>            
#> 1 analysis_d6eea5eb-e597-4fc8… score   arm       NA            NA               
#> 2 analysis_d6eea5eb-e597-4fc8… score   arm       NA            NA               
#> 3 analysis_d6eea5eb-e597-4fc8… score   arm       NA            NA               
#> # ℹ 17 more variables: transformation_label <chr>, term <chr>, level <chr>,
#> #   estimate <dbl>, std_error <dbl>, std_error_scale <chr>, conf_low <dbl>,
#> #   conf_high <dbl>, statistic <dbl>, df <dbl>, p_value <dbl>,
#> #   effect_measure <chr>, scale <chr>, n <int>, n_events <int>, method <chr>,
#> #   variance <chr>
tests(hc3_result)
#> # A tibble: 1 × 13
#>   analysis_id   outcome predictor contrast numerator denominator test  statistic
#>   <chr>         <chr>   <chr>     <chr>    <chr>     <chr>       <chr>     <dbl>
#> 1 analysis_d6e… score   arm       NA       NA        NA          pred…      6.39
#> # ℹ 5 more variables: df <dbl>, p_value <dbl>, p_adjusted <dbl>,
#> #   adjust_method <chr>, method <chr>
contrasts(hc3_result)
#> # A tibble: 3 × 23
#>   analysis_id       outcome predictor contrast_id contrast numerator denominator
#>   <chr>             <chr>   <chr>     <chr>       <chr>    <chr>     <chr>      
#> 1 analysis_d6eea5e… score   arm       contrast_0… Treatme… Treatmen… Control    
#> 2 analysis_d6eea5e… score   arm       contrast_0… Treatme… Treatmen… Control    
#> 3 analysis_d6eea5e… score   arm       contrast_0… Treatme… Treatmen… Treatment A
#> # ℹ 16 more variables: modifier <chr>, modifier_level <chr>,
#> #   inner_contrast <chr>, outer_contrast <chr>, estimand <chr>,
#> #   exponentiated <lgl>, estimate <dbl>, std_error <dbl>,
#> #   std_error_scale <chr>, conf_low <dbl>, conf_high <dbl>, p_value <dbl>,
#> #   p_adjusted <dbl>, adjust_method <chr>, effect_measure <chr>, scale <chr>
table_body(tbl_comparison(hc3_result))
#> # A tibble: 3 × 7
#>   outcome predictor contrast                estimate conf_int p_value p_adjusted
#>   <chr>   <chr>     <chr>                   <chr>    <chr>    <chr>   <chr>     
#> 1 score   arm       Treatment A vs Control  5.37     -0.61 –… 0.076   0.152     
#> 2 score   arm       Treatment B vs Control  10.37    3.94 – … 0.003   0.009     
#> 3 score   arm       Treatment B vs Treatme… 5.00     -1.90 –… 0.147   0.152
```

The model callback owns fitting and normalization, including its
model-based contrast algorithm. The registered comparison specification
remains the source of the contrast id and multiplicity adjustment. The
function id, hash, and required package version are retained in the plan
and provenance.

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
Unless an explicit method chain was declared, a fit failure never causes
the selector to run again.
