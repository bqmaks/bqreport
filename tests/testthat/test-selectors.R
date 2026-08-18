test_that("resolve_variables() returns selected columns with stable identifiers", {
  data <- labelled_data()
  selection <- rlang::quo(c(bmi, age))

  expect_identical(
    resolve_variables(data, selection, "variables"),
    tibble::tibble(
      var_id = c("v003", "v001"),
      name = c("bmi", "age"),
      position = c(3L, 1L)
    )
  )
})

test_that("resolve_variables() follows a rename through its stable identifier", {
  data <- dplyr::rename(labelled_data(), years = age)

  expect_identical(
    resolve_variables(data, rlang::quo(years), "variable")$var_id,
    "v001"
  )
})

test_that("resolve_variables() does not replace canonical names with aliases", {
  data <- labelled_data()

  expect_identical(
    resolve_variables(data, rlang::quo(c(alias = age)), "variable"),
    tibble::tibble(var_id = "v001", name = "age", position = 1L)
  )
  expect_identical(
    variables(set_type(data, c(alias = age), continuous()))$type[1L],
    "continuous"
  )
})

test_that("resolve_variables() enforces selection cardinality", {
  data <- labelled_data()

  expect_error(
    resolve_variables(data, rlang::quo(tidyselect::starts_with("absent")), "outcome"),
    class = "bq_error_invalid_selection"
  )
  expect_error(
    resolve_variables(data, rlang::quo(c(age, bmi)), "outcome", min = 0L, max = 1L),
    "at most 1 column, not 2"
  )
  expect_error(
    resolve_variables(data, rlang::quo(c(age, bmi)), "outcome", min = 1L, max = 1L),
    "exactly one column, not 2"
  )
})

test_that("resolve_variables() types tidyselect errors for the calling argument", {
  data <- labelled_data()

  expect_error(
    resolve_variables(data, rlang::quo(absent), "strata"),
    class = "bq_error_invalid_selection"
  )
  expect_error(
    resolve_variables(data, rlang::quo(absent), "strata"),
    "Cannot select `strata`"
  )
})
