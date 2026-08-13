test_that("plan_analysis creates one tidy task per outcome-predictor pair", {
  x <- as_bq_data(tibble::tibble(
    `blood pressure` = c(120, 130, 140),
    response = c(0, 1, 0),
    age = c(40, 50, 60)
  )) |>
    set_outcome(`blood pressure`, type = "continuous") |>
    set_outcome(response, type = "binary", event = 1) |>
    set_predictor(age, type = "continuous")

  plan <- plan_analysis(x)

  expect_s3_class(plan, "analysis_plan")
  expect_s3_class(plan, "tbl_df")
  expect_equal(nrow(plan), 2L)
  expect_identical(plan$method, c("linear_model", "logistic_model"))
  expect_identical(plan$engine, c("lm", "glm"))
  expect_true(all(plan$status == "ready"))
  expect_true(all(!plan$validated))
  expect_true(all(!plan$approved))
  expect_true(all(plan$method_policy == "system_default"))
  expect_identical(plan$contrast_ids, list(character(), character()))
  expect_true(all(vapply(plan$formula, inherits, logical(1), "formula")))
  expect_identical(
    deparse(plan$formula[[1]]),
    "`blood pressure` ~ age"
  )
  expect_identical(plan$outcome_id, variables(x)$var_id[1:2])
  expect_identical(plan$predictor_id, rep(variables(x)$var_id[[3]], 2))
})

test_that("plan status reflects review and unsupported metadata", {
  x <- as_bq_data(tibble::tibble(
    inferred = c("no", "yes", "no"),
    unsupported = as.Date("2024-01-01") + 0:2,
    predictor = 1:3
  )) |>
    set_outcome(c(inferred, unsupported)) |>
    set_predictor(predictor, type = "continuous")

  plan <- plan_analysis(x)

  expect_identical(plan$status, c("review", "invalid"))
  expect_identical(plan$method[[1]], "logistic_model")
  expect_true(is.na(plan$method[[2]]))
  expect_match(plan$reason[[2]], "Unsupported outcome type")
})

test_that("plan selectors compose and do not create self-regressions silently", {
  x <- as_bq_data(tibble::tibble(y = 1:3, x = 2:4)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(c(y, x), type = "continuous")

  plan <- plan_analysis(
    x,
    outcomes = all_outcomes(),
    predictors = all_predictors()
  )

  expect_equal(nrow(plan), 2L)
  expect_identical(plan$status, c("invalid", "ready"))
  expect_match(plan$reason[[1]], "same variable")
})

test_that("validate_plan computes missing counts including labelled missings", {
  x <- as_bq_data(
    tibble::tibble(outcome = c(1, 2, 99, NA), group = c("A", "B", "A", "B")),
    metadata = tibble::tibble(name = "outcome", na_values = list(99))
  ) |>
    set_outcome(outcome, type = "continuous") |>
    set_predictor(group, type = "binary", reference = "A")

  plan <- validate_plan(plan_analysis(x), x)

  expect_identical(plan$n_total, 4L)
  expect_identical(plan$n_analyzed, 2L)
  expect_identical(plan$n_missing_outcome, 2L)
  expect_identical(plan$n_missing_predictor, 0L)
  expect_identical(plan$status, "ready")
  expect_true(plan$validated)
})

test_that("validate_plan rejects missing event and reference configurations", {
  x <- as_bq_data(tibble::tibble(
    outcome = c(0, 1, 0),
    group = c("A", "B", "A")
  )) |>
    set_outcome(outcome, type = "binary") |>
    set_predictor(group, type = "binary")

  plan <- validate_plan(plan_analysis(x), x)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "event")
  expect_match(plan$reason, "reference")
})

test_that("validate_plan rejects absent configured values and no variation", {
  x <- as_bq_data(tibble::tibble(
    outcome = c(0, 0, 0),
    group = c("A", "A", "A")
  )) |>
    set_outcome(outcome, type = "binary", event = 1) |>
    set_predictor(group, type = "binary", reference = "B")

  plan <- validate_plan(plan_analysis(x), x)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "event value")
  expect_match(plan$reason, "reference value")
  expect_match(plan$reason, "variation")
})

test_that("validate_plan follows stable ids after rename", {
  x <- as_bq_data(tibble::tibble(y = 1:3, x = 2:4)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  plan <- plan_analysis(x)
  renamed <- dplyr::rename(x, outcome_new = y, predictor_new = x)

  validated <- validate_plan(plan, renamed)

  expect_identical(validated$outcome, "outcome_new")
  expect_identical(validated$predictor, "predictor_new")
  expect_identical(deparse(validated$formula[[1]]), "outcome_new ~ predictor_new")
})

test_that("approve_plan promotes validated review tasks", {
  x <- as_bq_data(tibble::tibble(
    outcome = c("no", "yes", "no"),
    predictor = 1:3
  )) |>
    set_outcome(outcome, event = "yes") |>
    set_predictor(predictor, type = "continuous")
  plan <- validate_plan(plan_analysis(x), x)

  approved <- approve_plan(plan, analysis_id = plan$analysis_id)

  expect_identical(plan$status, "review")
  expect_identical(approved$status, "ready")
  expect_true(approved$validated)
  expect_true(approved$approved)
  expect_true(is.na(approved$reason))
})

test_that("approve_plan requires preflight and rejects invalid tasks", {
  x <- as_bq_data(tibble::tibble(
    outcome = c("no", "yes", "no"),
    predictor = 1:3
  )) |>
    set_outcome(outcome, event = "yes") |>
    set_predictor(predictor, type = "continuous")
  unvalidated <- plan_analysis(x)

  expect_error(
    approve_plan(unvalidated),
    class = "bq_error_unvalidated_plan"
  )

  invalid_data <- as_bq_data(tibble::tibble(y = 1:3, x = 1:3)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(c(y, x), type = "continuous")
  invalid_plan <- validate_plan(plan_analysis(invalid_data), invalid_data)

  expect_error(
    approve_plan(invalid_plan, invalid_plan$analysis_id[[1]]),
    class = "bq_error_invalid_plan_approval"
  )
  expect_error(
    approve_plan(invalid_plan, "unknown-id"),
    class = "bq_error_unknown_analysis_id"
  )
})

test_that("analysis_plan builder accumulates regression blocks through pipes", {
  data <- as_bq_data(tibble::tibble(
    y1 = c(1, 2, 3, 4),
    y2 = c(0, 1, 0, 1),
    treatment = factor(c("A", "A", "B", "B")),
    age = c(40, 50, 60, 70)
  )) |>
    set_outcome(y1, type = "continuous") |>
    set_outcome(y2, type = "binary", event = 1) |>
    set_predictor(treatment, type = "binary", reference = "A") |>
    set_predictor(age, type = "continuous")

  plan <- data |>
    analysis_plan() |>
    add_analysis(y1, treatment) |>
    add_analysis(y2, treatment, covariates = age) |>
    validate_plan()

  expect_s3_class(plan, "analysis_plan")
  expect_identical(plan$outcome, c("y1", "y2"))
  expect_identical(plan$covariates, list(character(), "age"))
  expect_true(all(plan$validated))
})

test_that("analysis_plan builder accumulates descriptive blocks", {
  data <- as_bq_data(tibble::tibble(
    age = c(40, 50, 60, 70),
    response = c(0, 1, 0, 1),
    arm = factor(c("A", "A", "B", "B"))
  )) |>
    set_outcome(age, type = "continuous") |>
    set_outcome(response, type = "binary", event = 1) |>
    set_role(arm, "group")

  plan <- data |>
    analysis_plan() |>
    add_descriptives(age, groups = arm, overall = TRUE) |>
    add_descriptives(response, groups = arm, overall = FALSE) |>
    validate_plan()

  expect_identical(plan$analysis_type, rep("descriptive", 2L))
  expect_identical(plan$variable, c("age", "response"))
  expect_identical(plan$overall, c(TRUE, FALSE))
})

test_that("analysis_plan builder rejects duplicate tasks", {
  data <- as_bq_data(tibble::tibble(y = 1:4, x = 2:5)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")

  expect_error(
    data |>
      analysis_plan() |>
      add_analysis(y, x) |>
      add_analysis(y, x),
    class = "bq_error_duplicate_analysis"
  )
})

test_that("build_plan exposes an unvalidated accumulated plan", {
  data <- as_bq_data(tibble::tibble(y = 1:4, x = 2:5)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")

  plan <- data |>
    analysis_plan() |>
    add_analysis(y, x) |>
    build_plan()

  expect_s3_class(plan, "analysis_plan")
  expect_false(any(plan$validated))
  expect_error(
    data |> analysis_plan() |> build_plan(),
    class = "bq_error_empty_plan_builder"
  )
})

test_that("analysis_plan builder executes mixed analysis blocks end to end", {
  data <- as_bq_data(tibble::tibble(
    y = c(1.1, 2.0, 2.9, 4.2, 4.8, 6.1, 7.0, 7.9),
    z = c(1.4, 1.8, 3.2, 3.8, 5.1, 5.9, 7.2, 7.7),
    treatment = factor(rep(c("A", "B"), each = 4L))
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "A")

  result <- data |>
    analysis_plan() |>
    add_descriptives(y) |>
    add_analysis(y, treatment) |>
    add_correlations(y, with = z) |>
    validate_plan() |>
    run_analysis(data)

  expect_identical(
    result$plan$analysis_type,
    c("descriptive", "univariable_regression", "correlation")
  )
  expect_gt(nrow(descriptives(result)), 0L)
  expect_gt(nrow(estimates(result)), 0L)
  expect_identical(nrow(correlations(result)), 1L)
  expect_equal(correlations(result)$estimate, stats::cor(data$y, data$z))
})
