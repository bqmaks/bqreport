test_that("multinomial model agrees with nnet multinom", {
  skip_if_not_installed("nnet")
  raw <- tibble::tibble(
    y = factor(rep(c("control", "mild", "severe"), each = 8),
      levels = c("control", "mild", "severe")),
    x = c(rep(0,6),rep(1,2), rep(0,3),rep(1,5), rep(0,1),rep(1,7))
  )
  data <- as_bq_data(raw) |>
    set_outcome(y, type = "nominal", reference = "control") |>
    set_predictor(x, type = "continuous")
  result <- data |> plan_analysis(y, x) |> validate_plan(data) |> run_analysis(data)
  direct <- nnet::multinom(y ~ x, raw, trace = FALSE, Hess = TRUE)
  out <- estimates(result)

  expect_identical(result$plan$method, "multinomial_logistic_model")
  expect_equal(out$estimate, as.numeric(t(exp(stats::coef(direct)))))
  expect_identical(out$level, rep(c("mild", "severe"), each = 2))
  expect_true(all(out$effect_measure == "multinomial_odds_ratio"))
  expect_identical(tests(result)$test, "likelihood_ratio")
})

test_that("nominal outcome preflight requires at least three factor levels", {
  data <- as_bq_data(tibble::tibble(y = factor(c("a", "b", "a")), x = 1:3)) |>
    set_outcome(y, type = "nominal") |> set_predictor(x, type = "continuous")
  plan <- data |> plan_analysis(y, x) |> validate_plan(data)
  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "at least three")
})
