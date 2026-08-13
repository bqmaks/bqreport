test_that("penalized Cox regression agrees with glmnet CV fit", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  set.seed(12)
  data <- as_bq_data(tibble::tibble(
    time = stats::rexp(60, rate = 0.1),
    event = stats::rbinom(60, 1, 0.7),
    x1 = stats::rnorm(60), x2 = stats::rnorm(60)
  )) |>
    set_predictor(x1, type = "continuous") |>
    set_predictor(x2, type = "continuous") |>
    add_survival_outcome(
      os, time, event, event_value = 1, time_unit = "months"
    )
  method <- penalized_cox_model(
    alpha = 1, lambda = "lambda.min", nfolds = 5, seed = 77
  )
  result <- data |>
    plan_penalized_cox(os, x1, covariates = x2, method = method) |>
    validate_plan(data) |>
    run_analysis(data)
  x <- stats::model.matrix(~ x1 + x2, data)[, -1, drop = FALSE]
  y <- survival::Surv(data$time, data$event == 1)
  direct <- bqreport:::with_local_seed(77, glmnet::cv.glmnet(
    x, y, family = "cox", alpha = 1, nfolds = 5, cox.ties = "breslow"
  ))

  expect_equal(
    estimates(result)$estimate,
    exp(as.numeric(stats::coef(direct, s = "lambda.min")))
  )
  expect_true(all(is.na(estimates(result)$p_value)))
  expect_true(all(is.na(estimates(result)$conf_low)))
  expect_true("selected_lambda" %in% diagnostics(result)$metric)
})
