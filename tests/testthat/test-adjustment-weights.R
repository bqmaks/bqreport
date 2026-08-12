test_that("plan stores covariates and builds adjusted formula by stable ids", {
  x <- as_bq_data(tibble::tibble(y = 1:6, exposure = 2:7, age = 30:35, sex = rep(c("F", "M"), 3))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(exposure, type = "continuous") |>
    set_predictor(age, type = "continuous") |>
    set_predictor(sex, type = "binary", reference = "F")

  plan <- plan_analysis(x, predictors = exposure, covariates = c(age, sex))

  expect_identical(plan$covariate_ids[[1]], variables(x)$var_id[3:4])
  expect_identical(plan$covariates[[1]], c("age", "sex"))
  expect_identical(deparse(plan$formula[[1]]), "y ~ exposure + age + sex")
})

test_that("set_weight and preflight record IPW diagnostics", {
  x <- as_bq_data(tibble::tibble(y = 1:5, x = 2:6, iptw = c(1, 2, 0.5, 1.5, 1))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_weight(iptw, type = "ipw")

  plan <- validate_plan(plan_analysis(x, weights = iptw), x)
  diagnostics <- plan$weight_diagnostics[[1]]

  expect_identical(plan$weight_id, variables(x)$var_id[[3]])
  expect_identical(plan$weight_type, "ipw")
  expect_identical(plan$variance, "robust")
  expect_equal(diagnostics$effective_n, sum(x$iptw)^2 / sum(x$iptw^2))
  expect_identical(diagnostics$n_zero, 0L)
})

test_that("invalid weights fail preflight", {
  x <- as_bq_data(tibble::tibble(y = 1:4, x = 2:5, w = c(1, -1, NA, 2))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_weight(w, type = "ipw")

  plan <- validate_plan(plan_analysis(x, weights = w), x)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "negative")
  expect_match(plan$reason, "missing")
})

test_that("IPW engine uses robust covariance without changing coefficients", {
  x <- as_bq_data(tibble::tibble(
    y = c(2.0, 2.8, 4.2, 5.1, 5.9, 7.2), x = 1:6,
    age = c(30, 42, 35, 51, 47, 60), w = c(1, 1.5, 0.8, 2, 1.2, 0.7)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_predictor(age, type = "continuous") |>
    set_weight(w, type = "ipw")
  plan <- validate_plan(plan_analysis(x, predictors = x, covariates = age, weights = w), x)

  result <- run_analysis(plan, x)
  direct <- stats::lm(y ~ x + age, data = x, weights = w)
  robust_se <- sqrt(diag(sandwich::vcovHC(direct, type = "HC0")))

  expect_identical(estimates(result)$estimate, unname(stats::coef(direct)))
  expect_equal(estimates(result)$std_error, unname(robust_se))
  expect_true(all(estimates(result)$std_error_scale == "identity"))
})
