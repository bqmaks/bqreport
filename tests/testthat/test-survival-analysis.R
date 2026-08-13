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

test_that("analysis_plan builder accumulates and executes survival tasks", {
  skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(
    time = c(5, 8, 12, 4, 10, 15, 7, 9),
    event = c(1, 0, 1, 1, 0, 1, 0, 1),
    arm = factor(rep(c("A", "B"), each = 4L))
  )) |>
    set_predictor(arm, type = "binary", reference = "A") |>
    add_survival_outcome(
      os, time = time, event = event, event_value = 1, time_unit = "months"
    )

  result <- data |>
    analysis_plan() |>
    add_survival(os, arm) |>
    add_kaplan_meier(os, groups = arm, times = c(5, 10)) |>
    validate_plan() |>
    run_analysis(data)

  expect_identical(
    result$plan$analysis_type,
    c("survival_regression", "kaplan_meier")
  )
  expect_gt(nrow(estimates(result)), 0L)
  expect_gt(nrow(survival_estimates(result)), 0L)
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

test_that("Cox interactions return ratios of hazard-ratio contrasts", {
  skip_if_not_installed("survival")
  set.seed(412)
  raw <- tibble::tibble(
    time = rexp(160, rate = rep(c(0.08, 0.12, 0.1, 0.2), each = 40)) + 0.1,
    event = rbinom(160, 1, 0.8),
    treatment = factor(rep(rep(c("P", "D"), each = 40), 2), levels = c("P", "D")),
    sex = factor(rep(c("F", "M"), each = 80), levels = c("F", "M"))
  )
  data <- as_bq_data(raw) |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_comparisons(
      treatment,
      contrast_of_contrasts(
        sex, inner = against_reference("P"),
        outer = against_reference("F"), exponentiate = TRUE
      )
    ) |>
    add_survival_outcome(os, time, event, 1, "months")
  plan <- data |>
    plan_survival(os, treatment, effect_modifiers = sex) |>
    validate_plan(data)
  result <- run_analysis(plan, data)
  output <- contrasts(result)
  fit <- models(result)[[plan$analysis_id]]
  interaction_name <- grep(":", names(stats::coef(fit)), value = TRUE)

  expect_identical(plan$status, "ready")
  expect_equal(output$estimate, exp(unname(stats::coef(fit)[interaction_name])))
  expect_equal(output$std_error,
    sqrt(stats::vcov(fit)[interaction_name, interaction_name]))
  expect_identical(output$effect_measure, "ratio_of_hazard_ratios")
  expect_identical(output$scale, "ratio")
  expect_true("interaction" %in% tests(result)$test)
})

test_that("Cox interactions expose conditional hazard ratios", {
  skip_if_not_installed("survival")
  set.seed(413)
  data <- as_bq_data(tibble::tibble(
    time = rexp(120, 0.1) + 0.1, event = rbinom(120, 1, 0.75),
    treatment = factor(rep(c("P", "D"), 60), levels = c("P", "D")),
    sex = factor(rep(c("F", "M"), each = 60), levels = c("F", "M"))
  )) |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_comparisons(treatment, within_levels(sex)) |>
    add_survival_outcome(os, time, event, 1, "months")
  result <- data |>
    plan_survival(os, treatment, effect_modifiers = sex) |>
    validate_plan(data) |>
    run_analysis(data)

  output <- contrasts(result)
  expect_identical(output$modifier_level, c("F", "M"))
  expect_true(all(output$effect_measure == "hazard_ratio"))
  expect_true(all(output$scale == "ratio"))
})

test_that("Cox baseline and subgroup strategies are explicit", {
  method <- cox_model(
    ties = "efron", baseline = common_baseline(),
    subgroup = no_subgroup_analysis()
  )
  expect_identical(method$baseline$type, "common")
  expect_identical(method$subgroup$type, "none")
})

test_that("joint and separate Cox subgroup strategies are distinct", {
  skip_if_not_installed("survival")
  set.seed(932)
  raw <- tibble::tibble(
    time = rexp(240, rep(c(0.05, 0.2, 0.1, 0.13), each = 60)) + 0.1,
    event = rbinom(240, 1, 0.8),
    treatment = factor(rep(rep(c("P", "D"), each = 60), 2), levels = c("P", "D")),
    sex = factor(rep(c("F", "M"), each = 120), levels = c("F", "M")),
    center = factor(rep(c("C1", "C2"), 120))
  )
  data <- as_bq_data(raw) |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_role(center, "stratum") |>
    add_survival_outcome(os, time, event, 1, "months")

  joint <- data |>
    plan_survival(
      os, treatment,
      method = cox_model(
        ties = "efron", baseline = stratified_baseline(by = center),
        subgroup = joint_interaction(modifier = sex)
      )
    ) |>
    validate_plan(data) |>
    run_analysis(data)
  separate <- data |>
    plan_survival(
      os, treatment,
      method = cox_model(
        ties = "efron", baseline = stratified_baseline(by = center),
        subgroup = separate_subgroup_models(by = sex)
      )
    ) |>
    validate_plan(data) |>
    run_analysis(data)

  joint_model <- models(joint)[[joint$plan$analysis_id]]
  separate_models <- models(separate)[[separate$plan$analysis_id]]
  expect_s3_class(joint_model, "coxph")
  expect_s3_class(separate_models, "cox_subgroup_fits")
  expect_length(separate_models$fits, 2L)
  expect_true(all(contrasts(joint)$fit_strategy == "joint_interaction"))
  expect_true(all(contrasts(separate)$fit_strategy == "separate_subgroup_models"))
  expect_true("interaction" %in% tests(joint)$test)
  expect_false("interaction" %in% tests(separate)$test)
  expect_false(isTRUE(all.equal(
    contrasts(joint)$estimate, contrasts(separate)$estimate
  )))
  expect_identical(joint$provenance$baseline_strategy, "stratified")
  expect_identical(joint$provenance$subgroup_strategy, "joint_interaction")
  expect_identical(
    separate$provenance$subgroup_strategy, "separate_subgroup_models"
  )
})
