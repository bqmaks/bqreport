test_that("continuous_descriptives_extended() declares the extended set", {
  skip_if_not_installed("datawizard")

  statistic <- continuous_descriptives_extended()

  expect_s3_class(
    statistic,
    c("bq_continuous_statistic", "bq_statistic"),
    exact = TRUE
  )
  expect_identical(statistic$name, "descriptives_extended")
  expect_identical(
    statistic$components,
    c(
      "mean", "sd", "median", "q1", "q3", "min", "max", "iqr", "mad",
      "skewness", "excess_kurtosis"
    )
  )
  expect_identical(
    unname(statistic$component_scales),
    c(rep("variable", 9), rep("dimensionless", 2))
  )
  expect_identical(
    unname(statistic$component_rounding),
    c(rep(NA_character_, 9), "decimal", "decimal")
  )
  expect_identical(
    unname(statistic$component_digits),
    c(rep(NA_integer_, 9), 2L, 2L)
  )
  expect_identical(statistic$source, "built_in_datawizard")
  expect_identical(statistic$missing, "omit")
})

test_that("extended moments match direct datawizard calculations", {
  skip_if_not_installed("datawizard")
  x <- c(1, 2, NA, 4, 8, 16)
  observed <- x[!is.na(x)]

  result <- continuous_descriptives_extended()$fun(x)
  expected_skewness <- datawizard::skewness(
    observed,
    remove_na = FALSE,
    type = "2",
    verbose = FALSE
  )$Skewness[1L]
  expected_kurtosis <- datawizard::kurtosis(
    observed,
    remove_na = FALSE,
    type = "2",
    verbose = FALSE
  )$Kurtosis[1L]

  expect_equal(result$iqr, result$q3 - result$q1)
  expect_equal(result$mad, stats::mad(observed))
  expect_equal(result$skewness, expected_skewness)
  expect_equal(result$excess_kurtosis, expected_kurtosis)
})

test_that("extended moments do not fall back for small samples", {
  skip_if_not_installed("datawizard")
  statistic <- continuous_descriptives_extended()

  two <- statistic$fun(c(1, 2))
  three <- statistic$fun(c(1, 2, 4))
  four <- statistic$fun(c(1, 2, 4, 8))
  constant <- statistic$fun(rep(1, 4))

  expect_true(is.na(two$skewness))
  expect_true(is.na(two$excess_kurtosis))
  expect_false(is.na(three$skewness))
  expect_true(is.na(three$excess_kurtosis))
  expect_false(is.na(four$excess_kurtosis))
  expect_true(is.na(constant$skewness))
  expect_true(is.na(constant$excess_kurtosis))
})

test_that("continuous_descriptives_extended() passes preflight", {
  skip_if_not_installed("datawizard")
  data <- as_bq_data(tibble::tibble(value = c(1, 2, 4, 8)))
  data <- set_type(data, value, continuous())
  data <- set_rounding(data, value, 1)
  plan <- plan_summary(data) |>
    add_statistic(value, continuous_descriptives_extended())

  checked <- preflight(plan)

  expect_true(checked$ok)
  expect_false(any(checked$diagnostics$code == "missing_component_rounding"))
})
