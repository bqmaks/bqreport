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
      type = NA_character_
    )
  )
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
  expect_identical(nrow(variables_of(as_bq_data(data.frame()))), 0L)
})
