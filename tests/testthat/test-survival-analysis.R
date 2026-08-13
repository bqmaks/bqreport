test_that("plan_survival compiles Cox tasks from composite outcomes", {
  data <- as_bq_data(tibble::tibble(
    time = c(1, 2, 3, 4, 5, 6),
    event = c(1, 0, 1, 1, 0, 1),
    treatment = c("A", "A", "A", "B", "B", "B")
  )) |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")

  plan <- plan_survival(data, os, treatment)

  expect_s3_class(plan, "analysis_plan")
  expect_identical(plan$analysis_type, "survival_regression")
  expect_identical(plan$outcome, "os")
  expect_identical(plan$predictor, "treatment")
  expect_identical(plan$method, "cox_proportional_hazards")
  expect_identical(plan$effect_measure, "hazard_ratio")
})

test_that("Cox estimates agree with direct survival backend", {
  skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(
    time = c(2, 3, 4, 5, 6, 7, 8, 10, 12, 15),
    event = c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1),
    treatment = factor(rep(c("A", "B"), 5))
  )) |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")
  direct <- survival::coxph(
    survival::Surv(time, event == 1) ~ treatment,
    data = tibble::as_tibble(data), ties = "efron"
  )

  result <- data |>
    plan_survival(os, treatment) |>
    validate_plan(data) |>
    run_analysis(data)
  estimate <- estimates(result)

  expect_equal(estimate$estimate, unname(exp(stats::coef(direct))))
  expect_equal(estimate$std_error, unname(sqrt(diag(stats::vcov(direct)))))
  expect_equal(estimate$p_value, summary(direct)$coefficients[, "Pr(>|z|)"])
  expect_identical(estimate$effect_measure, "hazard_ratio")
  expect_identical(estimate$scale, "ratio")
  expect_identical(estimate$n_events, as.integer(direct$nevent))
  expect_s3_class(models(result)[[1]], "coxph")
})

test_that("Cox analysis returns model tests and PH diagnostics", {
  skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(
    time = 1:12,
    event = rep(c(1, 1, 0), 4),
    treatment = rep(c("A", "B"), 6)
  )) |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")
  result <- data |>
    plan_survival(os, treatment) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_true("likelihood_ratio" %in% tests(result)$test)
  expect_true(all(c("treatment", "GLOBAL") %in% diagnostics(result)$metric))
  expect_true(all(diagnostics(result)$status == "observed"))
})

test_that("survival preflight checks time, events, and predictor variation", {
  data <- as_bq_data(tibble::tibble(
    time = c(1, -1, 3), event = c(0, 0, 0), treatment = c("A", "A", "A")
  )) |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")
  plan <- validate_plan(plan_survival(data, os, treatment), data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "positive")
  expect_match(plan$reason, "events")
  expect_match(plan$reason, "variation")
})

test_that("Cox models apply scalar transformations before spline covariates", {
  skip_if_not_installed("survival")
  set.seed(101)
  raw <- tibble::tibble(
    time = rexp(100, 0.1) + 0.1,
    event = rbinom(100, 1, 0.7),
    treatment = rep(c("A", "B"), 50),
    biomarker = exp(rnorm(100, 2, 0.5))
  )
  data <- as_bq_data(raw) |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    set_transformation(biomarker, log2_transform()) |>
    set_model_term(biomarker, natural_spline(df = 3)) |>
    add_survival_outcome(os, time, event, 1, "months")
  plan <- data |>
    plan_survival(os, treatment, covariates = biomarker) |>
    validate_plan(data)
  result <- run_analysis(plan, data)
  term <- plan$model_term_specs[[1]][[variables(data)$var_id[variables(data)$name == "biomarker"]]]
  direct <- survival::coxph(
    survival::Surv(time, event == 1) ~ treatment + splines::ns(
      log2(biomarker), knots = term$resolved_parameters$knots,
      Boundary.knots = term$resolved_parameters$boundary_knots
    ),
    data = raw, ties = "efron"
  )

  expect_identical(plan$status, "ready")
  expect_equal(stats::coef(models(result)[[1]]), stats::coef(direct), ignore_attr = TRUE)
  expect_true("model_term_omnibus" %in% tests(result)$test)
  expect_identical(
    tests(result)$predictor[tests(result)$test == "model_term_omnibus"],
    "biomarker"
  )
})

test_that("Cox preflight rejects nonlinear primary predictors", {
  data <- as_bq_data(tibble::tibble(
    time = 1:10, event = rep(c(1, 0), 5), age = 21:30
  )) |>
    set_predictor(age, type = "continuous") |>
    set_model_term(age, polynomial_term(2)) |>
    add_survival_outcome(os, time, event, 1, "months")

  plan <- validate_plan(plan_survival(data, os, age), data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "primary predictor", ignore.case = TRUE)
})

test_that("Cox models honor multi-group comparison strategies", {
  skip_if_not_installed("survival")
  set.seed(88)
  data <- as_bq_data(tibble::tibble(
    time = rexp(90, 0.1) + 0.1, event = rbinom(90, 1, 0.7),
    arm = factor(rep(c("A", "B", "C"), 30))
  )) |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_comparisons(arm, consecutive_comparisons(), adjust = "holm") |>
    add_survival_outcome(os, time, event, 1, "months")

  result <- data |>
    plan_survival(os, arm) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_identical(
    paste(contrasts(result)$numerator, contrasts(result)$denominator),
    c("B A", "C B")
  )
  expect_true(all(contrasts(result)$effect_measure == "hazard_ratio"))
})
