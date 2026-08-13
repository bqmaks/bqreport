test_that("proportional odds model agrees with MASS polr", {
  skip_if_not_installed("MASS")
  raw <- tibble::tibble(
    y = ordered(rep(c("low", "mid", "high"), each = 6), c("low", "mid", "high")),
    x = c(0,0,0,1,1,1, 0,0,1,1,1,1, 0,1,1,1,1,1)
  )
  data <- as_bq_data(raw) |>
    set_outcome(y, type = "ordinal") |>
    set_predictor(x, type = "continuous")
  result <- data |> plan_analysis(y, x) |> validate_plan(data) |> run_analysis(data)
  direct <- MASS::polr(y ~ x, raw, Hess = TRUE, method = "logistic")
  out <- estimates(result)
  coefficient <- out[out$term == "x", ]

  expect_identical(result$plan$method, "ordinal_logistic_model")
  expect_equal(coefficient$estimate, unname(exp(stats::coef(direct))))
  expect_true(all(out$effect_measure[out$term == "x"] == "proportional_odds_ratio"))
  expect_equal(sum(grepl("^threshold_", diagnostics(result)$metric)), 2)
  expect_identical(tests(result)$test, "likelihood_ratio")
})

test_that("ordinal preflight requires an ordered factor", {
  data <- as_bq_data(tibble::tibble(y = factor(c("a", "b", "c")), x = 1:3)) |>
    set_outcome(y, type = "ordinal") |> set_predictor(x, type = "continuous")
  plan <- data |> plan_analysis(y, x) |> validate_plan(data)
  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "ordered factor")
})
