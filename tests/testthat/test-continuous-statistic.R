test_that("continuous_statistic() records a fixed data frame prototype", {
  fun <- function(x) {
    data.frame(
      centre = if (length(x) == 0L) NA_real_ else mean(x, na.rm = TRUE),
      observed = sum(!is.na(x))
    )
  }
  specification <- continuous_statistic("custom_centre", fun)

  expect_s3_class(
    specification,
    c("bq_continuous_statistic", "bq_statistic"),
    exact = TRUE
  )
  expect_identical(
    unclass(specification),
    list(
      kind = "continuous_statistic",
      name = "custom_centre",
      components = c("centre", "observed"),
      component_types = c(centre = "double", observed = "integer"),
      source = "custom_raw",
      missing = "user",
      fun = fun
    )
  )
})

test_that("continuous_statistic() accepts a tibble prototype", {
  specification <- continuous_statistic(
    "range",
    function(x) tibble::tibble(minimum = NA_real_, maximum = NA_real_)
  )

  expect_identical(specification$components, c("minimum", "maximum"))
  expect_identical(specification$component_types, c(minimum = "double", maximum = "double"))
})

test_that("continuous_statistic() validates its name and function", {
  valid_fun <- function(x) data.frame(value = NA_real_)

  for (name in list(NULL, NA_character_, "", c("a", "b"), 1)) {
    expect_error(
      continuous_statistic(name, valid_fun),
      class = "bq_error_invalid_statistic"
    )
  }
  expect_error(
    continuous_statistic("value", 1),
    class = "bq_error_invalid_statistic"
  )
})

test_that("continuous_statistic() reports failure on its empty prototype input", {
  expect_error(
    continuous_statistic("requires_data", function(x) stop("no observations")),
    class = "bq_error_invalid_statistic"
  )
  expect_error(
    continuous_statistic("requires_data", function(x) stop("no observations")),
    "fails for an empty numeric vector: no observations"
  )
})

test_that("continuous_statistic() requires a one-row data frame", {
  invalid_functions <- list(
    function(x) 1,
    function(x) data.frame(value = numeric()),
    function(x) data.frame(value = c(1, 2))
  )

  for (fun in invalid_functions) {
    expect_error(
      continuous_statistic("invalid", fun),
      class = "bq_error_invalid_statistic"
    )
  }
})

test_that("continuous_statistic() requires unique non-empty component names", {
  duplicated <- function(x) data.frame(a = 1, a = 2, check.names = FALSE)
  unnamed <- function(x) stats::setNames(data.frame(1), "")

  expect_error(
    continuous_statistic("duplicated", duplicated),
    class = "bq_error_invalid_statistic"
  )
  expect_error(
    continuous_statistic("unnamed", unnamed),
    class = "bq_error_invalid_statistic"
  )
})

test_that("continuous_statistic() accepts only plain numeric result columns", {
  invalid_functions <- list(
    function(x) data.frame(value = "text"),
    function(x) data.frame(value = TRUE),
    function(x) data.frame(value = factor("a")),
    function(x) tibble::tibble(value = list(1)),
    function(x) data.frame(value = I(matrix(1, nrow = 1)))
  )

  for (fun in invalid_functions) {
    expect_error(
      continuous_statistic("invalid", fun),
      class = "bq_error_invalid_statistic"
    )
  }
})
