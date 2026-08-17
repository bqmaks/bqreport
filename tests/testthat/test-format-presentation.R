formatted_example <- function(rounding = TRUE) {
  data <- as_bq_data(tibble::tibble(value = c(1.2, 3)))
  data <- set_type(data, value, continuous())
  if (rounding) {
    data <- set_rounding(data, value, 2, "decimal")
  }
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = mean(x))
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(
    plan,
    value,
    enumerate_values(display_statistics = TRUE)
  )
  prepare_presentation(run_analysis(plan))
}

test_that("format_presentation() formats estimates and enumerations", {
  presentation <- formatted_example()

  formatted <- format_presentation(presentation, decimal_mark = ",")

  expect_s3_class(
    formatted,
    c("bq_formatted_presentation_summary", "bq_formatted_presentation"),
    exact = TRUE
  )
  expect_identical(formatted$presentation, presentation)
  expect_identical(formatted$formatted_estimates$value, "2,10")
  expect_identical(formatted$formatted_values$value, c("1,20", "3,00"))
  expect_identical(formatted$enumerations$value, "1,20; 3,00")
  expect_identical(formatted$settings$decimal_mark, ",")
  expect_identical(formatted$settings$value_separator, "; ")
})

test_that("format_presentation() supports alternative decimal marks", {
  presentation <- formatted_example()

  for (decimal_mark in c(".", ",", "\u00b7")) {
    formatted <- format_presentation(
      presentation,
      decimal_mark = decimal_mark,
      value_separator = " | "
    )
    expect_identical(
      formatted$formatted_values$value[1L],
      paste0("1", decimal_mark, "20")
    )
    expect_identical(
      formatted$enumerations$value,
      paste0("1", decimal_mark, "20 | 3", decimal_mark, "00")
    )
  }
})

test_that("format_presentation() can trim trailing fractional zeros", {
  presentation <- formatted_example()

  formatted <- format_presentation(
    presentation,
    trim_trailing_zeros = TRUE
  )

  expect_identical(formatted$formatted_estimates$value, "2.1")
  expect_identical(formatted$formatted_values$value, c("1.2", "3"))
  expect_identical(formatted$enumerations$value, "1.2, 3")
})

test_that("format_presentation() formats component scales", {
  data <- as_bq_data(tibble::tibble(value = c(1, 3)))
  data <- set_type(data, value, continuous())
  data <- set_rounding(data, value, 1, "decimal")
  statistic <- continuous_statistic(
    "summary",
    function(x) {
      data.frame(
        mean = mean(x),
        observed = length(x),
        ratio = 12000
      )
    },
    scale = c(
      mean = "variable",
      observed = "count",
      ratio = "dimensionless"
    )
  )
  statistic <- set_component_rounding(
    statistic,
    "ratio",
    3,
    "significant"
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  presentation <- prepare_presentation(run_analysis(plan))

  formatted <- format_presentation(presentation)

  expect_identical(
    formatted$formatted_estimates$value,
    c("2.0", "2", "1.20e+04")
  )

  trimmed <- format_presentation(
    presentation,
    decimal_mark = ",",
    trim_trailing_zeros = TRUE
  )
  expect_identical(
    trimmed$formatted_estimates$value,
    c("2", "2", "1,2e+04")
  )
})

test_that("format_presentation() normalizes negative zero", {
  data <- as_bq_data(tibble::tibble(value = -0.001))
  data <- set_type(data, value, continuous())
  data <- set_rounding(data, value, 2, "decimal")
  statistic <- continuous_statistic(
    "value",
    function(x) data.frame(value = mean(x))
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  presentation <- prepare_presentation(run_analysis(plan))

  formatted <- format_presentation(presentation)

  expect_identical(formatted$formatted_estimates$value, "0.00")
})

test_that("format_presentation() uses only an explicit fallback", {
  presentation <- formatted_example(rounding = FALSE)

  expect_error(
    format_presentation(presentation),
    class = "bq_error_missing_rounding"
  )

  formatted <- format_presentation(
    presentation,
    fallback_digits = 2,
    fallback_method = "decimal"
  )
  expect_identical(formatted$formatted_estimates$value, "2.10")
  expect_identical(formatted$formatted_values$value, c("1.20", "3.00"))
})

test_that("format_presentation() records missing and empty status text", {
  data <- as_bq_data(tibble::tibble(
    value = c(NA_real_, 1),
    stratum = factor(c("A", "B"), levels = c("A", "B", "C"))
  ))
  data <- set_type(data, value, continuous())
  data <- set_type(data, stratum, nominal("A"))
  data <- set_rounding(data, value, 1)
  statistic <- continuous_statistic(
    "mean",
    function(x) {
      data.frame(
        mean = if (length(x) == 0L) NA_real_ else mean(x, na.rm = TRUE)
      )
    }
  )
  plan <- plan_summary(
    data,
    strata = stratum
  )
  plan <- add_statistic(plan, value, statistic)
  presentation <- prepare_presentation(run_analysis(plan))

  formatted <- format_presentation(
    presentation,
    missing = "Missing",
    empty = "Empty"
  )

  expect_identical(
    formatted$display_cells$status_text,
    c("Missing", NA_character_, "Empty")
  )
  expect_identical(
    formatted$formatted_estimates$value,
    c("Missing", "1.0", "Missing")
  )
})

test_that("format_presentation() validates formatting arguments", {
  presentation <- formatted_example()

  for (decimal_mark in list(NA_character_, "", "..", "1", " ", 1)) {
    expect_error(
      format_presentation(presentation, decimal_mark = decimal_mark),
      class = "bq_error_invalid_format"
    )
  }
  for (trim in list(NA, logical(), c(TRUE, FALSE), 1)) {
    expect_error(
      format_presentation(presentation, trim_trailing_zeros = trim),
      class = "bq_error_invalid_format"
    )
  }
  expect_error(
    format_presentation(presentation, fallback_digits = -1),
    class = "bq_error_invalid_rounding"
  )
})

test_that("format_presentation() requires prepared results", {
  expect_error(
    format_presentation(labelled_data()),
    class = "bq_error_invalid_presentation"
  )
})
