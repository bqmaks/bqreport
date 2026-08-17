test_that("variables() returns the registry as a plain tibble", {
  data <- labelled_data()
  registry <- variables(data)

  expect_identical(registry, variables_of(data))
  expect_s3_class(registry, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
  expect_identical(
    names(registry),
    c(
      "var_id", "name", "label", "role", "type", "event", "event_source",
      "reference", "type_source", "unit", "rounding", "digits"
    )
  )
})

test_that("variables() refuses anything but a bq_data object", {
  expect_error(variables(tibble::tibble(a = 1)), class = "bq_error_invalid_data")
  expect_error(variables(1:3), "must be a bq_data object, not integer")
})
