test_that("robust linear regression agrees with MASS rlm", {
  skip_if_not_installed("MASS")
  data <- as_bq_data(tibble::tibble(
    y = c(1, 2, 3, 4, 5, 30, 7, 8), x = 1:8
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  rules <- analysis_rules(where_continuous() ~ robust_linear_model())
  result <- data |> plan_analysis(y, x, rules = rules) |>
    validate_plan(data) |> run_analysis(data)
  direct <- MASS::rlm(y ~ x, data = data)

  expect_equal(estimates(result)$estimate, unname(stats::coef(direct)))
  expect_identical(result$plan$method, "robust_linear_model")
  expect_identical(result$plan$required_packages, list("MASS"))
})

test_that("quantile regression agrees with quantreg rq", {
  skip_if_not_installed("quantreg")
  data <- as_bq_data(tibble::tibble(
    y = c(1, 3, 2, 5, 4, 8, 7, 9, 11, 10), x = 1:10
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  method <- quantile_model(tau = 0.5, se = "nid")
  result <- data |> plan_analysis(
    y, x, rules = analysis_rules(where_continuous() ~ method)
  ) |> validate_plan(data) |> run_analysis(data)
  direct <- quantreg::rq(y ~ x, data = data, tau = 0.5)

  expect_equal(estimates(result)$estimate, unname(stats::coef(direct)))
  expect_identical(result$plan$effect_measure, "conditional_quantile_difference")
  expect_identical(result$plan$method_object[[1]]$tau, 0.5)
})

test_that("beta regression agrees with betareg and validates outcome domain", {
  skip_if_not_installed("betareg")
  data <- as_bq_data(tibble::tibble(
    y = c(0.12, 0.20, 0.31, 0.42, 0.48, 0.61, 0.72, 0.84), x = 1:8
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  method <- beta_model()
  result <- data |> plan_analysis(
    y, x, rules = analysis_rules(where_continuous() ~ method)
  ) |> validate_plan(data) |> run_analysis(data)
  direct <- betareg::betareg(y ~ x, data = data, link = "logit")

  expect_equal(
    estimates(result)$estimate,
    unname(summary(direct)$coefficients$mean[, "Estimate"])
  )
  invalid <- dplyr::mutate(data, y = c(0, data$y[-1])) |>
    plan_analysis(y, x, rules = analysis_rules(where_continuous() ~ method)) |>
    validate_plan(dplyr::mutate(data, y = c(0, data$y[-1])))
  expect_identical(invalid$status, "invalid")
  expect_match(invalid$reason, "strictly between 0 and 1")
})
