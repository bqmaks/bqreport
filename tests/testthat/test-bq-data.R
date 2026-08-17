test_that("as_bq_data() gives every column one blank registry row", {
  data <- as_bq_data(data.frame(age = c(40, 55), sex = c("f", "m")))

  expect_s3_class(data, "bq_data")
  expect_s3_class(data, "tbl_df")
  expect_identical(
    variables_of(data),
    tibble::tibble(
      var_id = c("v001", "v002"),
      name = c("age", "sex"),
      label = NA_character_,
      role = NA_character_,
      type = NA_character_,
      event = NA_character_,
      event_source = NA_character_,
      reference = NA_character_,
      type_source = NA_character_,
      unit = NA_character_,
      rounding = NA_character_,
      digits = NA_integer_
    )
  )
  expect_identical(
    levels_of(data),
    tibble::tibble(
      var_id = character(),
      value = character(),
      position = integer()
    )
  )
  expect_identical(attr(data, "next_var_number"), 3L)
})

test_that("as_bq_data() leaves an existing bq_data untouched", {
  data <- labelled_data()

  expect_identical(as_bq_data(data), data)
})

test_that("as_bq_data() rejects anything that is not a data frame", {
  expect_error(as_bq_data(1:3), class = "bq_error_invalid_data")
  expect_error(as_bq_data(1:3), "must be a data frame, not integer")
})

test_that("as_bq_data() accepts a frame without columns", {
  data <- as_bq_data(data.frame())

  expect_identical(nrow(variables_of(data)), 0L)
  expect_identical(nrow(levels_of(data)), 0L)
  expect_identical(attr(data, "next_var_number"), 1L)
})
