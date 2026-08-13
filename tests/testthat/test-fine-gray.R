test_that("Fine-Gray regression agrees with cmprsk crr", {
  skip_if_not_installed("cmprsk")
  time <- c(1, 2, 3, 4, 5, 6, 7, 8, 2, 3, 5, 7, 9, 10, 11, 12)
  status <- c(1, 2, 0, 1, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 2)
  arm <- factor(rep(c("A", "B"), each = 8L))
  data <- as_bq_data(tibble::tibble(time, status, arm)) |>
    set_predictor(arm, type = "binary", reference = "A") |>
    add_competing_risk_outcome(
      pfs, time, status, censor_value = 0, time_unit = "months"
    )
  method <- fine_gray_model(cause = 1)
  result <- data |>
    plan_fine_gray(pfs, arm, method = method) |>
    validate_plan(data) |>
    run_analysis(data)
  direct <- cmprsk::crr(
    ftime = time, fstatus = status,
    cov1 = stats::model.matrix(~ arm)[, -1, drop = FALSE],
    failcode = 1, cencode = 0
  )

  expect_equal(estimates(result)$estimate, exp(unname(direct$coef)))
  expect_identical(estimates(result)$effect_measure, "subdistribution_hazard_ratio")
  expect_identical(result$plan$method, "fine_gray_regression")
  expect_true(all(diagnostics(result)$metric == "converged"))
})

test_that("Fine-Gray preflight requires an observed requested cause", {
  data <- as_bq_data(tibble::tibble(
    time = 1:6, status = c(0, 1, 0, 1, 0, 1), x = 1:6
  )) |>
    set_predictor(x, type = "continuous") |>
    add_competing_risk_outcome(
      pfs, time, status, censor_value = 0, time_unit = "months"
    )
  plan <- data |>
    plan_fine_gray(pfs, x, method = fine_gray_model(cause = 2)) |>
    validate_plan(data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "cause")
})
