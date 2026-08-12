test_that("analysis rules override system defaults with concrete methods", {
  x <- as_bq_data(tibble::tibble(y = 1:5, x = 2:6)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  rules <- analysis_rules(where_continuous() ~ linear_model(ci_method = "wald"))

  plan <- plan_analysis(x, rules = rules)

  expect_identical(plan$method_policy, "user_rule")
  expect_identical(plan$method, "linear_model")
  expect_identical(plan$ci_method, "wald")
})

test_that("ambiguous rules fail during compilation", {
  x <- as_bq_data(tibble::tibble(y = 1:5, x = 2:6)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  rules <- analysis_rules(
    where_continuous() ~ linear_model(),
    where_type("continuous") ~ linear_model()
  )

  expect_error(
    plan_analysis(x, rules = rules),
    class = "bq_error_ambiguous_rule"
  )
})

test_that("custom atomic analysis function runs through a validated contract", {
  custom_rr <- analysis_function(
    id = "custom_relative_risk",
    effect_measure = "risk_ratio",
    scale = "ratio",
    run = function(context) {
      fit <- stats::glm(
        context$formula,
        data = context$model_frame,
        family = stats::poisson("log")
      )
      sm <- summary(fit)$coefficients
      analysis_output(
        model = fit,
        estimates = tibble::tibble(
          analysis_id = context$analysis_id,
          outcome = context$outcome_spec$name,
          predictor = context$predictor_spec$name,
          term = rownames(sm),
          level = NA_character_,
          estimate = exp(unname(sm[, "Estimate"])),
          std_error = unname(sm[, "Std. Error"]),
          std_error_scale = "log_risk",
          conf_low = exp(unname(sm[, "Estimate"] - 1.96 * sm[, "Std. Error"])),
          conf_high = exp(unname(sm[, "Estimate"] + 1.96 * sm[, "Std. Error"])),
          statistic = unname(sm[, "z value"]),
          df = NA_real_,
          p_value = unname(sm[, "Pr(>|z|)"]),
          effect_measure = "risk_ratio",
          scale = "ratio",
          n = as.integer(stats::nobs(fit)),
          n_events = as.integer(sum(context$response == 1)),
          method = "custom_relative_risk",
          variance = "model_based"
        )
      )
    }
  )
  x <- as_bq_data(tibble::tibble(y = c(0, 0, 1, 0, 1, 1), x = c(0, 1, 0, 1, 1, 1))) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(x, type = "continuous")
  rules <- analysis_rules(where_binary() ~ custom_rr)
  plan <- validate_plan(plan_analysis(x, rules = rules), x)

  result <- run_analysis(plan, x)

  expect_identical(unique(estimates(result)$effect_measure), "risk_ratio")
  expect_identical(unique(estimates(result)$scale), "ratio")
  expect_identical(plan$function_id, "custom_relative_risk")
  expect_true(nzchar(plan$function_hash))
})

test_that("malformed custom output is a typed engine issue", {
  bad <- analysis_function(
    id = "bad_method", effect_measure = "risk_ratio", scale = "ratio",
    run = function(context) analysis_output(
      estimates = tibble::tibble(estimate = 1)
    )
  )
  x <- as_bq_data(tibble::tibble(y = c(0, 1, 0), x = 1:3)) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(x, type = "continuous")
  result <- run_analysis(
    validate_plan(plan_analysis(x, rules = analysis_rules(where_binary() ~ bad)), x),
    x
  )

  expect_equal(nrow(estimates(result)), 0L)
  expect_match(issues(result)$condition_class, "bq_error_invalid_engine_output")
})

test_that("logistic method controls exponentiation explicitly", {
  x <- as_bq_data(tibble::tibble(
    y = c(0, 0, 0, 1, 0, 1, 1, 1),
    x = c(0.2, 0.8, 1.1, 1.5, 2.0, 2.4, 3.0, 3.5)
  )) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(x, type = "continuous")

  ratio_plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_binary() ~ logistic_model(exponentiate = TRUE))
  ), x)
  link_plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_binary() ~ logistic_model(exponentiate = FALSE))
  ), x)
  ratio <- estimates(run_analysis(ratio_plan, x))
  link <- estimates(run_analysis(link_plan, x))

  expect_equal(ratio$estimate, exp(link$estimate))
  expect_identical(unique(ratio$effect_measure), "odds_ratio")
  expect_identical(unique(ratio$scale), "ratio")
  expect_identical(unique(link$effect_measure), "log_odds")
  expect_identical(unique(link$scale), "link")
  expect_true(ratio_plan$exponentiate)
  expect_false(link_plan$exponentiate)
  expect_identical(ratio$std_error, link$std_error)
})
