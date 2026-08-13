test_that("plan_kaplan_meier compiles inspectable grouped tasks", {
  data <- as_bq_data(tibble::tibble(
    time = 1:6, event = c(1, 0, 1, 0, 1, 1), arm = rep(c("A", "B"), 3)
  )) |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")

  plan <- plan_kaplan_meier(data, os, groups = arm, times = c(2, 4))

  expect_s3_class(plan, "analysis_plan")
  expect_identical(plan$analysis_type, "kaplan_meier")
  expect_identical(plan$outcome, "os")
  expect_identical(plan$group, "arm")
  expect_identical(plan$evaluation_times[[1]], c(2, 4))
  expect_identical(plan$method, "kaplan_meier")
})

test_that("Kaplan-Meier probabilities agree with survival backend", {
  skip_if_not_installed("survival")
  raw <- tibble::tibble(
    time = c(1, 2, 3, 4, 5, 6, 7, 8),
    event = c(1, 0, 1, 1, 0, 1, 0, 1),
    arm = rep(c("A", "B"), each = 4)
  )
  data <- as_bq_data(raw) |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")
  direct <- survival::survfit(
    survival::Surv(time, event == 1) ~ arm,
    data = raw, conf.int = 0.95
  )

  result <- data |>
    plan_kaplan_meier(os, groups = arm, times = c(2, 4, 6)) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- survival_estimates(result)
  probabilities <- out[out$estimate_type == "survival_probability", ]
  direct_summary <- summary(direct, times = c(2, 4, 6), extend = FALSE)

  expect_equal(probabilities$estimate, direct_summary$surv)
  expect_equal(probabilities$std_error, direct_summary$std.err)
  expect_equal(probabilities$conf_low, direct_summary$lower)
  expect_equal(probabilities$conf_high, direct_summary$upper)
  expect_identical(probabilities$scale, rep("probability", nrow(probabilities)))
})

test_that("Kaplan-Meier without evaluation times returns the full step curve", {
  skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(
    time = c(1, 2, 3, 4), event = c(1, 0, 1, 1)
  )) |>
    add_survival_outcome(os, time, event, 1, "months")

  result <- data |>
    plan_kaplan_meier(os) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- survival_estimates(result)
  curve <- out[out$estimate_type == "survival_curve", ]

  expect_equal(curve$time, c(1, 2, 3, 4))
  expect_equal(curve$n_risk, c(4L, 3L, 2L, 1L))
  expect_equal(curve$n_event, c(1L, 0L, 1L, 1L))
  expect_equal(curve$n_censor, c(0L, 1L, 0L, 0L))
})

test_that("Kaplan-Meier returns median survival and log-rank test", {
  skip_if_not_installed("survival")
  raw <- tibble::tibble(
    time = c(1, 2, 3, 4, 5, 6, 7, 8),
    event = c(1, 1, 1, 0, 0, 1, 0, 1),
    arm = rep(c("A", "B"), each = 4)
  )
  data <- as_bq_data(raw) |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")
  direct_test <- survival::survdiff(
    survival::Surv(time, event == 1) ~ arm, data = raw, rho = 0
  )

  result <- data |>
    plan_kaplan_meier(os, groups = arm) |>
    validate_plan(data) |>
    run_analysis(data)
  medians <- survival_estimates(result)
  medians <- medians[medians$estimate_type == "median_survival", ]
  logrank <- tests(result)

  expect_setequal(medians$group_level, c("A", "B"))
  expect_equal(logrank$statistic, unname(direct_test$chisq))
  expect_equal(logrank$df, 1)
  expect_equal(logrank$p_value, stats::pchisq(direct_test$chisq, 1, lower.tail = FALSE))
  expect_identical(logrank$test, "log_rank")
})

test_that("KM preflight validates explicit times and grouping", {
  data <- as_bq_data(tibble::tibble(
    time = c(1, 2, 3), event = c(0, 0, 0), arm = c("A", "A", "A")
  )) |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")

  expect_error(
    plan_kaplan_meier(data, os, times = c(1, -2)),
    class = "bq_error_invalid_survival_plan"
  )
  plan <- validate_plan(plan_kaplan_meier(data, os, groups = arm), data)
  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "variation")
})

test_that("KM with no events retains an estimable all-surviving curve", {
  skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(
    time = c(1, 2, 3), event = c(0, 0, 0)
  )) |>
    add_survival_outcome(os, time, event, 1, "months")
  result <- data |>
    plan_kaplan_meier(os, times = c(1, 2)) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- survival_estimates(result)

  expect_true(all(out$estimate[out$estimate_type == "survival_probability"] == 1))
  expect_true(is.na(out$estimate[out$estimate_type == "median_survival"]))
})

test_that("Kaplan-Meier returns explicit survival-time quantiles", {
  skip_if_not_installed("survival")
  raw <- tibble::tibble(
    time = c(1, 2, 3, 4, 5, 6),
    event = c(1, 0, 1, 1, 0, 1),
    arm = rep(c("A", "B"), 3)
  )
  data <- as_bq_data(raw) |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")
  direct <- survival::survfit(
    survival::Surv(time, event == 1) ~ arm, data = raw
  )
  direct_quantiles <- stats::quantile(direct, probs = c(0.25, 0.75))

  result <- data |>
    plan_kaplan_meier(os, groups = arm, quantiles = c(0.25, 0.75)) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- survival_estimates(result)
  out <- out[out$estimate_type == "survival_quantile", ]

  expect_equal(out$estimate, as.numeric(t(direct_quantiles$quantile)))
  expect_equal(out$conf_low, as.numeric(t(direct_quantiles$lower)))
  expect_equal(out$conf_high, as.numeric(t(direct_quantiles$upper)))
  expect_equal(out$quantile_probability, rep(c(0.25, 0.75), 2))
  expect_true(all(out$scale == "time"))
})

test_that("Kaplan-Meier computes RMST at an explicit restriction time", {
  skip_if_not_installed("survival")
  raw <- tibble::tibble(
    time = c(1, 2, 3, 4, 5, 6),
    event = c(1, 0, 1, 1, 0, 1),
    arm = rep(c("A", "B"), 3)
  )
  data <- as_bq_data(raw) |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    add_survival_outcome(os, time, event, 1, "months")
  direct <- survival::survfit(
    survival::Surv(time, event == 1) ~ arm, data = raw
  )
  direct_table <- summary(direct, rmean = 4)$table

  result <- data |>
    plan_kaplan_meier(os, groups = arm, rmst_tau = 4) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- survival_estimates(result)
  rmst <- out[out$estimate_type == "restricted_mean_survival_time", ]
  critical <- stats::qnorm(0.975)

  expect_equal(rmst$estimate, unname(direct_table[, "rmean"]))
  expect_equal(rmst$std_error, unname(direct_table[, "se(rmean)"]))
  expect_equal(
    rmst$conf_low,
    unname(direct_table[, "rmean"] - critical * direct_table[, "se(rmean)"])
  )
  expect_true(all(rmst$restriction_time == 4))
  expect_true(all(rmst$scale == "time"))
})

test_that("quantile and RMST estimands validate their domains", {
  data <- as_bq_data(tibble::tibble(time = 1:3, event = c(1, 0, 1))) |>
    add_survival_outcome(os, time, event, 1, "months")

  expect_error(
    plan_kaplan_meier(data, os, quantiles = c(0, 0.5)),
    class = "bq_error_invalid_survival_plan"
  )
  expect_error(
    plan_kaplan_meier(data, os, rmst_tau = c(2, 3)),
    class = "bq_error_invalid_survival_plan"
  )
})
