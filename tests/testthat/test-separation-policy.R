test_that("separation policy selects glm for overlapping data", {
  skip_if_not_installed("detectseparation")
  x <- as_bq_data(tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 1, 0),
    x = c(0, 0, 1, 1, 2, 2, 3, 3)
  )) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(x, type = "continuous")

  plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_binary() ~ separation_logistic())
  ), x)

  expect_identical(plan$method, "logistic_model")
  expect_identical(plan$engine, "glm")
  expect_false(plan$selection_diagnostics[[1]]$separation)
  expect_match(plan$selection_reason, "finite")
})

test_that("separation policy selects Firth and matches direct backend", {
  skip_if_not_installed("detectseparation")
  skip_if_not_installed("logistf")
  raw <- tibble::tibble(
    y = c(0, 0, 0, 1, 1, 1),
    x = c(0, 1, 2, 3, 4, 5)
  )
  x <- as_bq_data(raw) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(x, type = "continuous")
  plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_binary() ~ separation_logistic())
  ), x)

  result <- run_analysis(plan, x)
  direct <- logistf::logistf(y ~ x, data = raw)
  estimate <- estimates(result)

  expect_identical(plan$method, "firth_logistic")
  expect_identical(plan$engine, "custom_function")
  expect_true(plan$selection_diagnostics[[1]]$separation)
  expect_match(plan$selection_reason, "separation")
  expect_equal(estimate$estimate, exp(unname(direct$coefficients)))
  expect_equal(estimate$conf_low, exp(unname(direct$ci.lower)))
  expect_equal(estimate$conf_high, exp(unname(direct$ci.upper)))
  expect_equal(estimate$p_value, unname(direct$prob))
  expect_identical(unique(estimate$effect_measure), "odds_ratio")
  expect_identical(unique(estimate$scale), "ratio")
})

test_that("Firth method can retain coefficients on the link scale", {
  skip_if_not_installed("logistf")
  x <- as_bq_data(tibble::tibble(
    y = c(0, 0, 0, 1, 1, 1), x = 0:5
  )) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(x, type = "continuous")
  plan <- validate_plan(plan_analysis(
    x, rules = analysis_rules(where_binary() ~ firth_logistic(exponentiate = FALSE))
  ), x)

  estimate <- estimates(run_analysis(plan, x))

  expect_identical(unique(estimate$effect_measure), "log_odds")
  expect_identical(unique(estimate$scale), "link")
})
