test_that("variable_levels() returns the flat level registry", {
  data <- as_bq_data(tibble::tibble(severity = c("low", "high")))
  data <- set_type(data, severity, type_ordinal(c("low", "medium", "high")))

  registry <- variable_levels(data)

  expect_identical(registry, levels_of(data))
  expect_s3_class(registry, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
  expect_identical(names(registry), c("var_id", "value", "position"))
  expect_identical(registry$var_id, rep(variables(data)$var_id, 3))
})

test_that("variable_levels() keeps its schema when no levels are declared", {
  registry <- variable_levels(as_bq_data(tibble::tibble(age = c(40, 55))))

  expect_identical(
    registry,
    tibble::tibble(
      var_id = character(),
      value = character(),
      position = integer()
    )
  )
})

test_that("variable_levels() refuses anything but a bq_data object", {
  expect_error(
    variable_levels(tibble::tibble(x = 1)),
    class = "bq_error_invalid_data"
  )
  expect_error(variable_levels(1:3), "must be a bq_data object, not integer")
})
