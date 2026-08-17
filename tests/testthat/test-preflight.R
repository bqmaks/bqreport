test_that("preflight() accepts a ready continuous summary plan", {
  plan <- plan_summary(labelled_data(), age)
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, age, statistic)

  result <- preflight(plan)

  expect_s3_class(
    result,
    c("bq_preflight_summary", "bq_preflight"),
    exact = TRUE
  )
  expect_identical(
    unclass(result),
    list(
      analysis = "summary",
      ok = TRUE,
      diagnostics = tibble::tibble(
        severity = character(),
        code = character(),
        var_id = character(),
        statistic_id = character(),
        message = character()
      )
    )
  )
})

test_that("preflight() reports missing and unknown analytic types", {
  missing_data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  missing_plan <- plan_summary(missing_data, value)
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  missing_plan <- add_statistic(missing_plan, value, statistic)

  unknown_data <- apply_dictionary(
    missing_data,
    tibble::tibble(name = "value", type = "unknown")
  )
  unknown_plan <- plan_summary(unknown_data, value)
  unknown_plan <- add_statistic(unknown_plan, value, statistic)

  missing_result <- preflight(missing_plan)
  unknown_result <- preflight(unknown_plan)

  expect_false(missing_result$ok)
  expect_identical(missing_result$diagnostics$code, "missing_type")
  expect_identical(missing_result$diagnostics$var_id, "v001")
  expect_false(unknown_result$ok)
  expect_identical(unknown_result$diagnostics$code, "unknown_type")
})

test_that("preflight() checks types of design axes", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2),
    group = c("a", "b")
  ))
  data <- set_type(data, value, continuous())
  plan <- plan_summary(data, value, group = group)
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, value, statistic)

  result <- preflight(plan)

  expect_false(result$ok)
  expect_identical(result$diagnostics$code, "missing_type")
  expect_identical(result$diagnostics$var_id, "v002")
})

test_that("preflight() restricts units and rounding to quantitative variables", {
  data <- set_unit(labelled_data(), sex, "kg")
  data <- set_rounding(data, sex, 1)
  plan <- plan_summary(data, age, group = sex)
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, age, statistic)

  result <- preflight(plan)

  expect_false(result$ok)
  expect_identical(
    result$diagnostics$code,
    c("incompatible_unit", "incompatible_rounding")
  )
  expect_identical(result$diagnostics$var_id, c("v002", "v002"))
})

test_that("preflight() requires a statistic for every summary variable", {
  result <- preflight(plan_summary(labelled_data(), c(age, bmi)))

  expect_false(result$ok)
  expect_identical(
    result$diagnostics$code,
    c("missing_statistic", "missing_statistic")
  )
  expect_identical(result$diagnostics$var_id, c("v001", "v003"))
})

test_that("preflight() reports incompatible continuous statistics", {
  data <- set_type(labelled_data(), sex, binary("m"))
  plan <- plan_summary(data, sex)
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, sex, statistic)

  result <- preflight(plan)

  expect_false(result$ok)
  expect_identical(result$diagnostics$code, "incompatible_statistic")
  expect_identical(result$diagnostics$var_id, "v002")
  expect_identical(result$diagnostics$statistic_id, "s001")
})

test_that("preflight() rejects non-plans and damaged plan structures", {
  expect_error(
    preflight(labelled_data()),
    class = "bq_error_invalid_plan"
  )

  plan <- plan_summary(labelled_data(), age)
  plan$statistics <- plan$statistics[, -1]

  expect_error(preflight(plan), class = "bq_error_invalid_plan")
})
