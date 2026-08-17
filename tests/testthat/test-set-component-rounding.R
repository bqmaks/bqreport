test_that("set_component_rounding() records component policies", {
  statistic <- continuous_statistic(
    "summary",
    function(x) {
      data.frame(
        mean = NA_real_,
        observed = NA_integer_,
        cv = NA_real_
      )
    },
    scale = c(
      mean = "variable",
      observed = "count",
      cv = "dimensionless"
    )
  )

  result <- set_component_rounding(
    statistic,
    c("mean", "cv"),
    digits = 3,
    method = "significant"
  )

  expect_identical(
    result$component_rounding,
    c(mean = "significant", observed = NA, cv = "significant")
  )
  expect_identical(
    result$component_digits,
    c(mean = 3L, observed = NA_integer_, cv = 3L)
  )
  expect_true(all(is.na(statistic$component_rounding)))
  expect_true(all(is.na(statistic$component_digits)))
})

test_that("set_component_rounding() validates components", {
  statistic <- continuous_statistic(
    "summary",
    function(x) data.frame(mean = NA_real_, observed = NA_integer_),
    scale = c(mean = "variable", observed = "count")
  )

  for (components in list(NULL, character(), NA_character_, "", c("mean", "mean"))) {
    expect_error(
      set_component_rounding(statistic, components, 1),
      class = "bq_error_invalid_statistic"
    )
  }
  expect_error(
    set_component_rounding(statistic, "missing", 1),
    class = "bq_error_invalid_statistic"
  )
  expect_error(
    set_component_rounding(statistic, "observed", 1),
    class = "bq_error_invalid_rounding"
  )
})

test_that("set_component_rounding() validates method and digits", {
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = NA_real_)
  )

  for (method in list(NA_character_, "fixed", c("decimal", "significant"), 1)) {
    expect_error(
      set_component_rounding(statistic, "mean", 1, method),
      class = "bq_error_invalid_rounding"
    )
  }
  for (digits in list(NA_integer_, -1L, 1.5, c(1L, 2L), "1")) {
    expect_error(
      set_component_rounding(statistic, "mean", digits),
      class = "bq_error_invalid_rounding"
    )
  }
  expect_error(
    set_component_rounding(statistic, "mean", 0, "significant"),
    class = "bq_error_invalid_rounding"
  )
})

test_that("set_component_rounding() requires a statistic specification", {
  expect_error(
    set_component_rounding("mean", "mean", 1),
    class = "bq_error_invalid_statistic"
  )

  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = NA_real_)
  )
  statistic$component_digits <- 1
  expect_error(
    set_component_rounding(statistic, "mean", 1),
    class = "bq_error_invalid_statistic"
  )
})
