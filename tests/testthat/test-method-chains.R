chain_estimates <- function(context, method) {
  tibble::tibble(
    analysis_id = context$analysis_id,
    outcome = context$outcome_spec$name,
    predictor = context$predictor_spec$name,
    term = "predictor", level = NA_character_, estimate = 1,
    std_error = 0.2, std_error_scale = "identity",
    conf_low = 0.6, conf_high = 1.4, statistic = 5, df = 8,
    p_value = 0.001, effect_measure = "mean_difference",
    scale = "identity", n = as.integer(nrow(context$model_frame)),
    n_events = NA_integer_, method = method, variance = "model_based"
  )
}

chain_data <- function() {
  as_bq_data(tibble::tibble(y = 1:10, x = 11:20)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
}

chain_method <- function(id, run) {
  analysis_function(
    id = id, run = run,
    effect_measure = "mean_difference", scale = "identity"
  )
}

typed_chain_error <- function(class, message = "Declared numerical failure") {
  stop(structure(
    list(message = message, call = NULL),
    class = c(class, "error", "condition")
  ))
}

test_that("method chains are explicit and inspectable in the plan", {
  primary <- chain_method("primary", function(context) {
    analysis_output(estimates = chain_estimates(context, "primary"))
  })
  secondary <- chain_method("secondary", function(context) {
    analysis_output(estimates = chain_estimates(context, "secondary"))
  })
  chain <- analysis_method_chain(
    "declared_chain", list(primary = primary, secondary = secondary),
    advance_on = "bq_error_numerical_failure"
  )

  plan <- plan_analysis(
    chain_data(),
    rules = analysis_rules(where_continuous() ~ chain)
  )

  expect_s3_class(chain, "analysis_method_chain")
  expect_identical(plan$method, "declared_chain")
  expect_identical(plan$engine, "method_chain")
  expect_identical(plan$method_chain[[1]], c("primary", "secondary"))
  expect_identical(plan$fallback_conditions[[1]], "bq_error_numerical_failure")
})

test_that("a declared condition advances to the next method and records attempts", {
  primary <- chain_method("primary", function(context) {
    typed_chain_error("bq_error_numerical_failure")
  })
  secondary <- chain_method("secondary", function(context) {
    analysis_output(estimates = chain_estimates(context, "secondary"))
  })
  chain <- analysis_method_chain(
    "declared_chain", list(primary = primary, secondary = secondary),
    advance_on = "bq_error_numerical_failure"
  )
  x <- chain_data()
  result <- run_analysis(validate_plan(plan_analysis(
    x, rules = analysis_rules(where_continuous() ~ chain)
  ), x), x)

  expect_identical(attempts(result)$status, c("failed", "success"))
  expect_identical(attempts(result)$method, c("primary", "secondary"))
  expect_identical(unique(estimates(result)$method), "secondary")
  expect_identical(result$plan$executed_method, "secondary")
  expect_identical(result$provenance$method, "declared_chain")
  expect_identical(result$provenance$executed_method, "secondary")
  expect_true(result$provenance$fallback_used)
  expect_match(issues(result)$condition_class, "bq_error_numerical_failure")
})

test_that("undeclared failures and invalid outputs never advance", {
  secondary <- chain_method("secondary", function(context) {
    analysis_output(estimates = chain_estimates(context, "secondary"))
  })
  undeclared <- chain_method("undeclared", function(context) {
    typed_chain_error("bq_error_programming_failure")
  })
  malformed <- chain_method("malformed", function(context) {
    analysis_output(estimates = tibble::tibble(estimate = 1))
  })
  x <- chain_data()

  run_chain <- function(first, advance_on) {
    chain <- analysis_method_chain(
      "declared_chain", list(first = first, secondary = secondary), advance_on
    )
    run_analysis(validate_plan(plan_analysis(
      x, rules = analysis_rules(where_continuous() ~ chain)
    ), x), x)
  }

  unlisted_result <- run_chain(undeclared, "bq_error_numerical_failure")
  contract_result <- run_chain(malformed, "bq_error_invalid_engine_output")

  expect_equal(nrow(attempts(unlisted_result)), 1L)
  expect_equal(nrow(attempts(contract_result)), 1L)
  expect_equal(nrow(estimates(unlisted_result)), 0L)
  expect_equal(nrow(estimates(contract_result)), 0L)
  expect_identical(
    attempts(contract_result)$condition_class,
    "bq_error_invalid_engine_output"
  )
})

test_that("analysis_function accepts a declared fallback", {
  secondary <- chain_method("secondary", function(context) {
    analysis_output(estimates = chain_estimates(context, "secondary"))
  })
  method <- analysis_function(
    "primary", function(context) typed_chain_error("bq_error_numerical_failure"),
    effect_measure = "mean_difference", scale = "identity",
    fallback = list(secondary = secondary),
    advance_on = "bq_error_numerical_failure"
  )

  expect_s3_class(method, "analysis_method_chain")
  expect_identical(names(method$methods), c("primary", "secondary"))
})

test_that("method chains reject incompatible estimands", {
  primary <- chain_method("primary", function(context) NULL)
  incompatible <- analysis_function(
    "incompatible", function(context) NULL,
    effect_measure = "risk_ratio", scale = "ratio"
  )

  expect_error(
    analysis_method_chain(
      "bad_chain", list(primary = primary, incompatible = incompatible),
      advance_on = "bq_error_numerical_failure"
    ),
    class = "bq_error_invalid_method_contract"
  )
})
