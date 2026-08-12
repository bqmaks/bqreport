test_that("linear engine matches stats::lm without rounding", {
  x <- as_bq_data(tibble::tibble(
    outcome = c(2.1, 3.9, 6.2, 7.8, 10.1),
    predictor = c(1, 2, 3, 4, 5)
  )) |>
    set_outcome(outcome, type = "continuous") |>
    set_predictor(predictor, type = "continuous")
  plan <- validate_plan(plan_analysis(x), x)

  result <- run_analysis(plan, x)
  direct <- stats::lm(outcome ~ predictor, data = x)
  direct_summary <- summary(direct)$coefficients
  estimate <- estimates(result)

  expect_s3_class(result, "analysis_result")
  expect_identical(estimate$estimate, unname(direct_summary[, "Estimate"]))
  expect_identical(estimate$std_error, unname(direct_summary[, "Std. Error"]))
  expect_identical(estimate$p_value, unname(direct_summary[, "Pr(>|t|)"]))
  expect_true(all(estimate$scale == "identity"))
  expect_true(all(estimate$std_error_scale == "identity"))
  expect_identical(stats::nobs(models(result)[[plan$analysis_id]]), 5L)
})

test_that("logistic engine stores odds ratios and log-odds standard errors", {
  x <- as_bq_data(tibble::tibble(
    outcome = c(0, 0, 0, 1, 0, 1, 1, 1),
    predictor = c(0.2, 0.8, 1.1, 1.5, 2.0, 2.4, 3.0, 3.5)
  )) |>
    set_outcome(outcome, type = "binary", event = 1) |>
    set_predictor(predictor, type = "continuous")
  plan <- validate_plan(plan_analysis(x), x)

  result <- run_analysis(plan, x)
  direct <- stats::glm(outcome ~ predictor, data = x, family = stats::binomial("logit"))
  direct_coefficients <- summary(direct)$coefficients
  estimate <- estimates(result)
  critical <- stats::qnorm(0.975)

  expect_equal(estimate$estimate, exp(unname(direct_coefficients[, "Estimate"])))
  expect_identical(
    estimate$std_error,
    unname(direct_coefficients[, "Std. Error"])
  )
  expect_equal(
    estimate$conf_low,
    exp(unname(direct_coefficients[, "Estimate"] - critical * direct_coefficients[, "Std. Error"]))
  )
  expect_true(all(estimate$effect_measure == "odds_ratio"))
  expect_true(all(estimate$scale == "ratio"))
  expect_true(all(estimate$std_error_scale == "log_odds"))
  expect_identical(unique(estimate$n_events), 4L)
})

test_that("run_analysis excludes labelled special missings only in model frame", {
  raw <- tibble::tibble(outcome = c(1, 2, 99, 4), predictor = c(1, 2, 3, 4))
  x <- as_bq_data(
    raw,
    metadata = tibble::tibble(name = "outcome", na_values = list(99))
  ) |>
    set_outcome(outcome, type = "continuous") |>
    set_predictor(predictor, type = "continuous")
  plan <- validate_plan(plan_analysis(x), x)

  result <- run_analysis(plan, x)

  expect_identical(stats::nobs(models(result)[[plan$analysis_id]]), 3L)
  expect_identical(as.numeric(x$outcome[[3]]), 99)
  expect_identical(unique(estimates(result)$n), 3L)
})

test_that("analysis_result accessors return stable component types", {
  x <- as_bq_data(tibble::tibble(y = 1:4, x = 2:5)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  result <- run_analysis(validate_plan(plan_analysis(x), x), x)

  expect_s3_class(estimates(result), "tbl_df")
  expect_s3_class(contrasts(result), "tbl_df")
  expect_s3_class(tests(result), "tbl_df")
  expect_s3_class(diagnostics(result), "tbl_df")
  expect_s3_class(issues(result), "tbl_df")
  expect_s3_class(result$descriptives, "tbl_df")
  expect_s3_class(result$provenance, "tbl_df")
  expect_type(models(result), "list")
})

test_that("run_analysis refuses unvalidated plans", {
  x <- as_bq_data(tibble::tibble(y = 1:3, x = 2:4)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")

  expect_error(
    run_analysis(plan_analysis(x), x),
    class = "bq_error_unvalidated_plan"
  )
})

test_that("run_analysis skips non-ready tasks and records issues", {
  x <- as_bq_data(tibble::tibble(
    review_outcome = c("no", "yes", "no"),
    invalid_outcome = as.Date("2024-01-01") + 0:2,
    predictor = 1:3
  )) |>
    set_outcome(review_outcome, event = "yes") |>
    set_outcome(invalid_outcome) |>
    set_predictor(predictor, type = "continuous")
  plan <- validate_plan(plan_analysis(x), x)

  result <- run_analysis(plan, x, error = "collect")

  expect_length(models(result), 0L)
  expect_equal(nrow(issues(result)), 2L)
  expect_setequal(issues(result)$severity, c("info", "error"))
})

test_that("categorical predictor coefficients have normalized term and level", {
  x <- as_bq_data(tibble::tibble(
    outcome = c(2.0, 2.3, 3.8, 4.1, 6.2, 6.5),
    treatment = c("Placebo", "Placebo", "Drug A", "Drug A", "Drug B", "Drug B")
  )) |>
    set_outcome(outcome, type = "continuous") |>
    set_predictor(treatment, type = "nominal", reference = "Placebo")
  plan <- validate_plan(plan_analysis(x), x)

  result <- run_analysis(plan, x)
  estimate <- estimates(result)
  direct_data <- tibble::tibble(
    outcome = x$outcome,
    treatment = stats::relevel(factor(x$treatment), ref = "Placebo")
  )
  direct <- stats::lm(outcome ~ treatment, data = direct_data)

  expect_identical(estimate$term, c("(Intercept)", "treatment", "treatment"))
  expect_identical(estimate$level, c(NA_character_, "Drug A", "Drug B"))
  expect_identical(estimate$estimate, unname(stats::coef(direct)))
})

test_that("normalized coefficient metadata supports non-syntactic names", {
  x <- as_bq_data(tibble::tibble(
    outcome = c(0, 0, 1, 1, 0, 1),
    `treatment group` = c("Control", "Control", "Drug A", "Drug A", "Drug B", "Drug B")
  )) |>
    set_outcome(outcome, type = "binary", event = 1) |>
    set_predictor(
      `treatment group`,
      type = "nominal",
      reference = "Control"
    )
  plan <- validate_plan(plan_analysis(x), x)

  result <- run_analysis(plan, x)
  estimate <- estimates(result)

  expect_identical(
    estimate$term,
    c("(Intercept)", "treatment group", "treatment group")
  )
  expect_identical(estimate$level, c(NA_character_, "Drug A", "Drug B"))
})
