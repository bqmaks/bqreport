test_that("the header counts columns by role", {
  header <- tibble::tbl_sum(labelled_data())

  expect_identical(names(header), c("A bq_data", "Variables"))
  # tibble picks the multiplication sign according to the locale, so only the
  # dimensions themselves are asserted.
  expect_match(header[["A bq_data"]], "^3 . 3$")
  expect_identical(header[["Variables"]], "1 group, 1 outcome, 1 predictor")
})

test_that("the header pluralises roles but not the unassigned count", {
  data <- as_bq_data(data.frame(a = 1, b = 2, c = 3, d = 4))

  expect_identical(tibble::tbl_sum(data)[["Variables"]], "4 unassigned")

  variables <- variables_of(data)
  variables$role <- c("outcome", "outcome", "predictor", NA)
  attr(data, "variables") <- variables

  expect_identical(
    tibble::tbl_sum(data)[["Variables"]],
    "2 outcomes, 1 predictor, 1 unassigned"
  )
})

test_that("the header copes with a frame without columns", {
  expect_identical(tibble::tbl_sum(as_bq_data(data.frame()))[["Variables"]], "none")
})

test_that("printing keeps the tibble layout", {
  output <- capture.output(print(labelled_data()))

  expect_match(output[1], "^# A bq_data: 3 . 3$")
  expect_match(output[2], "^# Variables: ")
  # The data itself is still rendered by tibble, one line per row.
  expect_true(any(grepl("^1 +40 ", output)))
})
