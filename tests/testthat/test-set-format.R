test_that("set_unit() records units without changing data", {
  data <- labelled_data()
  result <- set_unit(data, c(age, bmi), "kg")

  expect_registry_aligned(result)
  expect_identical(tibble::as_tibble(result), tibble::as_tibble(data))
  expect_identical(variables_of(result)$unit, c("kg", NA, "kg"))
  expect_identical(variables_of(result)$type, variables_of(data)$type)
})

test_that("set_unit() validates data, unit and selection", {
  data <- labelled_data()

  expect_error(
    set_unit(tibble::tibble(age = 1), age, "years"),
    class = "bq_error_invalid_data"
  )
  for (unit in list(NULL, NA_character_, "", c("kg", "g"), 1)) {
    expect_error(set_unit(data, age, unit), class = "bq_error_invalid_unit")
  }
  expect_error(set_unit(data, missing, "years"), class = "bq_error_invalid_selection")
})

test_that("set_rounding() records decimal and significant policies", {
  data <- labelled_data()
  data <- set_rounding(data, age, digits = 0)
  result <- set_rounding(data, bmi, digits = 3, method = "significant")

  expect_registry_aligned(result)
  expect_identical(tibble::as_tibble(result), tibble::as_tibble(data))
  expect_identical(variables_of(result)$rounding, c("decimal", NA, "significant"))
  expect_identical(variables_of(result)$digits, c(0L, NA, 3L))
})

test_that("set_rounding() validates its method and digit count", {
  data <- labelled_data()

  for (method in list(NA_character_, "fixed", c("decimal", "significant"), 1)) {
    expect_error(
      set_rounding(data, age, 1, method),
      class = "bq_error_invalid_rounding"
    )
  }
  for (digits in list(NA_integer_, -1L, 1.5, c(1L, 2L), "1")) {
    expect_error(
      set_rounding(data, age, digits),
      class = "bq_error_invalid_rounding"
    )
  }
  expect_error(
    set_rounding(data, age, 0, "significant"),
    class = "bq_error_invalid_rounding"
  )
  expect_error(
    set_rounding(data, missing, 1),
    class = "bq_error_invalid_selection"
  )
})
