test_that("Aalen-Johansen cumulative incidence agrees with survival", {
  skip_if_not_installed("survival")
  raw <- tibble::tibble(
    time = c(1, 2, 3, 4, 2, 3, 5, 6),
    status = c(1, 2, 0, 1, 2, 0, 1, 0),
    arm = factor(rep(c("A", "B"), each = 4))
  )
  data <- as_bq_data(raw) |>
    set_predictor(arm, type = "binary", reference = "A") |>
    add_competing_risk_outcome(pfs, time, status, censor_value = 0, "months")

  result <- data |>
    plan_cumulative_incidence(pfs, groups = arm, times = c(2, 4)) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- survival_estimates(result)

  direct <- survival::survfit(
    survival::Surv(time, factor(status)) ~ arm,
    data = raw, conf.int = 0.95
  )
  direct <- summary(direct, times = c(2, 4), extend = FALSE)
  expected <- as.numeric(direct$pstate[, -1, drop = FALSE])

  expect_equal(out$estimate, expected)
  expect_identical(out$estimate_type, rep("cumulative_incidence", nrow(out)))
  expect_identical(out$cause, rep(c("1", "2"), each = 4))
  expect_identical(out$method, rep("aalen_johansen", nrow(out)))
})

test_that("competing-risk preflight rejects data without observed causes", {
  data <- as_bq_data(tibble::tibble(time = 1:3, status = c(0, 0, 0))) |>
    add_competing_risk_outcome(pfs, time, status, censor_value = 0, "months")
  plan <- data |> plan_cumulative_incidence(pfs) |> validate_plan(data)
  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "event cause")
})

test_that("analysis_plan builder executes cumulative-incidence tasks", {
  skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(
    time = c(1, 2, 3, 4, 2, 3, 5, 6),
    status = c(1, 2, 0, 1, 2, 0, 1, 0),
    arm = factor(rep(c("A", "B"), each = 4L))
  )) |>
    set_predictor(arm, type = "binary", reference = "A") |>
    add_competing_risk_outcome(
      pfs, time, status, censor_value = 0, time_unit = "months"
    )

  result <- data |>
    analysis_plan() |>
    add_cumulative_incidence(pfs, groups = arm, times = c(2, 4)) |>
    validate_plan() |>
    run_analysis(data)

  expect_identical(result$plan$analysis_type, "cumulative_incidence")
  expect_gt(nrow(survival_estimates(result)), 0L)
})
