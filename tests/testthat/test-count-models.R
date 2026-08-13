test_that("Poisson model returns rate ratios and agrees with glm", {
  raw <- tibble::tibble(
    events = c(0, 1, 1, 2, 2, 3, 4, 5),
    arm = factor(rep(c("A", "B"), each = 4))
  )
  data <- as_bq_data(raw) |>
    set_outcome(events, type = "count") |>
    set_predictor(arm, type = "binary", reference = "A")
  result <- data |> plan_analysis(events, arm) |> validate_plan(data) |> run_analysis(data)
  out <- estimates(result)
  direct <- stats::glm(events ~ arm, raw, family = stats::poisson("log"))

  expect_identical(result$plan$method, "poisson_model")
  expect_equal(out$estimate, unname(exp(stats::coef(direct))))
  expect_equal(out$std_error, unname(summary(direct)$coefficients[, "Std. Error"]))
  expect_true(all(out$effect_measure == "rate_ratio"))
  expect_true(all(out$scale == "ratio"))
  expect_true("dispersion_ratio" %in% diagnostics(result)$metric)
})

test_that("Poisson coefficients can remain on the log-rate scale", {
  method <- poisson_model(exponentiate = FALSE)
  expect_identical(method$effect_measure, "log_rate")
  expect_identical(method$scale, "link")
})

test_that("count outcome preflight requires non-negative integers", {
  data <- as_bq_data(tibble::tibble(y = c(0, 1.5, -1), x = 1:3)) |>
    set_outcome(y, type = "count") |>
    set_predictor(x, type = "continuous")
  plan <- data |> plan_analysis(y, x) |> validate_plan(data)
  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "non-negative integer")
})

test_that("negative binomial model agrees with MASS glm.nb", {
  skip_if_not_installed("MASS")
  raw <- tibble::tibble(
    y = c(0, 0, 1, 1, 2, 3, 5, 8, 2, 12, 1, 15),
    x = rep(c(0, 1), each = 6)
  )
  data <- as_bq_data(raw) |>
    set_outcome(y, type = "count") |>
    set_predictor(x, type = "continuous")
  rules <- analysis_rules(where_count() ~ negative_binomial_model())
  result <- data |> plan_analysis(y, x, rules = rules) |>
    validate_plan(data) |> run_analysis(data)
  direct <- MASS::glm.nb(y ~ x, data = raw)
  out <- estimates(result)

  expect_equal(out$estimate, unname(exp(stats::coef(direct))))
  expect_equal(out$std_error, unname(summary(direct)$coefficients[, "Std. Error"]))
  expect_true("theta" %in% diagnostics(result)$metric)
  expect_identical(tests(result)$test, "likelihood_ratio")
})
