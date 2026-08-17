test_that("set_role() records one role without changing data or other metadata", {
  data <- labelled_data()
  result <- set_role(data, age, "outcome")

  expect_registry_aligned(result)
  expect_identical(tibble::as_tibble(result), tibble::as_tibble(data))
  expect_identical(variables_of(result)$role, c("outcome", "group", "outcome"))
  expect_identical(variables_of(result)$type, variables_of(data)$type)
  expect_identical(variables_of(result)$event, variables_of(data)$event)
  expect_identical(variables_of(result)$event_source, variables_of(data)$event_source)
  expect_identical(variables_of(result)$reference, variables_of(data)$reference)
  expect_identical(variables_of(result)$type_source, variables_of(data)$type_source)
  expect_identical(levels_of(result), levels_of(data))
})

test_that("set_role() applies one role to several tidyselected variables", {
  data <- labelled_data()

  selected <- set_role(data, c(age, bmi), "predictor")
  ranged <- set_role(data, age:bmi, "id")

  expect_identical(variables_of(selected)$role, c("predictor", "group", "predictor"))
  expect_identical(variables_of(ranged)$role, rep("id", 3))
})

test_that("set_role() accepts every role in the fixed dictionary", {
  data <- as_bq_data(data.frame(x = 1))

  for (role in c("outcome", "predictor", "group", "id")) {
    expect_identical(variables_of(set_role(data, x, role))$role, role)
  }
})

test_that("set_role() rejects values outside the fixed role dictionary", {
  data <- as_bq_data(data.frame(x = 1))

  expect_error(set_role(data, x), class = "bq_error_invalid_role")
  for (role in list(NULL, NA_character_, c("outcome", "predictor"), "exposure", 1)) {
    expect_error(set_role(data, x, role), class = "bq_error_invalid_role")
  }
  expect_error(set_role(data, x, "exposure"), "must be one of")
})

test_that("set_role() requires bq_data and a non-empty valid selection", {
  data <- as_bq_data(data.frame(x = 1, y = 2))

  expect_error(
    set_role(tibble::tibble(x = 1), x, "outcome"),
    class = "bq_error_invalid_data"
  )
  expect_error(set_role(data, missing, "outcome"), class = "bq_error_invalid_selection")
  expect_error(
    set_role(data, tidyselect::starts_with("absent"), "outcome"),
    class = "bq_error_invalid_selection"
  )
  expect_error(
    set_role(data, tidyselect::starts_with("absent"), "outcome"),
    "at least one column"
  )
})

test_that("set_outcome() is equivalent to assigning the outcome role", {
  data <- labelled_data()

  expect_identical(
    set_outcome(data, c(age, bmi)),
    set_role(data, c(age, bmi), "outcome")
  )
})

test_that("set_predictor() is equivalent to assigning the predictor role", {
  data <- labelled_data()

  expect_identical(
    set_predictor(data, c(age, bmi)),
    set_role(data, c(age, bmi), "predictor")
  )
})
