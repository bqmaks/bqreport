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

test_that("method selector resolves one declared candidate during preflight", {
  selector <- method_selector(
    id = "sample_size_policy",
    candidates = list(
      standard = linear_model(ci_method = "t"),
      large_sample = linear_model(ci_method = "wald")
    ),
    select = function(context) {
      method_choice(
        method = if (nrow(context$model_frame) >= 6L) "large_sample" else "standard",
        reason = "Selected from the analyzed sample size.",
        diagnostics = tibble::tibble(n = nrow(context$model_frame))
      )
    }
  )
  x <- as_bq_data(tibble::tibble(y = 1:6, x = c(2, 1, 4, 3, 6, 5))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")

  compiled <- plan_analysis(
    x, rules = analysis_rules(where_continuous() ~ selector)
  )
  plan <- validate_plan(compiled, x)

  expect_identical(compiled$method, NA_character_)
  expect_identical(plan$method, "linear_model")
  expect_identical(plan$ci_method, "wald")
  expect_identical(plan$selector_id, "sample_size_policy")
  expect_identical(plan$candidate_methods[[1]], c("standard", "large_sample"))
  expect_identical(plan$selection_reason, "Selected from the analyzed sample size.")
  expect_equal(plan$selection_diagnostics[[1]]$n, 6L)
})

test_that("method selector is not rerun during analysis", {
  state <- new.env(parent = emptyenv())
  state$n <- 0L
  selector <- method_selector(
    id = "counted_policy",
    candidates = list(selected = linear_model()),
    select = function(context) {
      state$n <- state$n + 1L
      method_choice("selected", "Only candidate.")
    }
  )
  x <- as_bq_data(tibble::tibble(y = 1:5, x = c(5, 2, 4, 1, 3))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_continuous() ~ selector)
  ), x)

  expect_equal(state$n, 1L)
  result <- run_analysis(plan, x)
  expect_equal(state$n, 1L)
  expect_identical(result$provenance$selector_id, "counted_policy")
  expect_true(nzchar(result$provenance$selector_hash))
  expect_identical(result$provenance$candidate_methods[[1]], "selected")
  expect_identical(result$provenance$selection_reason, "Only candidate.")
})

test_that("invalid selector choices make the task invalid", {
  x <- as_bq_data(tibble::tibble(y = 1:5, x = c(5, 2, 4, 1, 3))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  unknown <- method_selector(
    "unknown_choice", list(allowed = linear_model()),
    function(context) method_choice("not_declared", "Bad choice.")
  )
  malformed <- method_selector(
    "malformed_choice", list(allowed = linear_model()),
    function(context) list(method = "allowed")
  )

  unknown_plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_continuous() ~ unknown)
  ), x)
  malformed_plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_continuous() ~ malformed)
  ), x)

  expect_identical(unknown_plan$status, "invalid")
  expect_match(unknown_plan$reason, "not an announced candidate")
  expect_identical(malformed_plan$status, "invalid")
  expect_match(malformed_plan$reason, "method_choice")
})

test_that("method selector constructor validates its public contract", {
  expect_error(
    method_selector("bad", list(linear_model()), function(context) NULL),
    class = "bq_error_invalid_method_contract"
  )
  expect_error(
    method_choice("method", "reason", diagnostics = list(value = 1)),
    class = "bq_error_invalid_method_choice"
  )
})

test_that("selector errors and missing selector packages are preflight issues", {
  x <- as_bq_data(tibble::tibble(y = 1:5, x = c(5, 2, 4, 1, 3))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  failing <- method_selector(
    "failing", list(allowed = linear_model()),
    function(context) stop("deliberate selector failure")
  )
  missing_backend <- method_selector(
    "missing_backend", list(allowed = linear_model()),
    function(context) method_choice("allowed", "Available candidate."),
    required_packages = "bqreportPackageThatCannotExist"
  )

  failed_plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_continuous() ~ failing)
  ), x)
  missing_plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_continuous() ~ missing_backend)
  ), x)

  expect_identical(failed_plan$status, "invalid")
  expect_match(failed_plan$reason, "deliberate selector failure")
  expect_identical(missing_plan$status, "invalid")
  expect_match(missing_plan$reason, "bqreportPackageThatCannotExist")
})
