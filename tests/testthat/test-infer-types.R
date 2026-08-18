test_that("infer_types() records decisions for every untyped column", {
  data <- as_bq_data(tibble::tibble(
    age = c(40, 55),
    flag = c(0, 1),
    group = c("zeta", "alpha"),
    severity = ordered(
      c("low", "high"),
      levels = c("low", "medium", "high")
    ),
    empty = c(NA, NA)
  ))
  result <- infer_types(data)
  registry <- variables_of(result)

  expect_registry_aligned(result)
  expect_identical(tibble::as_tibble(result), tibble::as_tibble(data))
  expect_identical(
    registry$type,
    c("continuous", "binary", "binary", "ordinal", "unknown")
  )
  expect_identical(registry$type_source, rep("inferred", 5))
  expect_identical(registry$event, c(NA, "1", "zeta", NA, NA))
  expect_identical(registry$event_source, c(NA, "inferred", "default", NA, NA))
  expect_identical(registry$reference, rep(NA_character_, 5))
})

test_that("infer_types() expands an inferred ordinal order", {
  data <- as_bq_data(tibble::tibble(
    severity = ordered("medium", levels = c("low", "medium", "high"))
  ))
  result <- infer_types(data)

  expect_identical(
    levels_of(result),
    tibble::tibble(
      var_id = rep("v001", 3),
      value = c("low", "medium", "high"),
      position = 1:3
    )
  )
})

test_that("infer_types() does not overwrite an existing type decision", {
  data <- as_bq_data(tibble::tibble(group = c("alpha", "zeta"), age = c(40, 55)))
  data <- set_type(data, group, type_nominal("alpha"))
  before <- variables_of(data)[1, ]

  result <- infer_types(data)

  expect_identical(variables_of(result)[1, ], before)
  expect_identical(variables_of(result)$type[2], "continuous")
  expect_identical(variables_of(result)$type_source[2], "inferred")
})

test_that("infer_types() limits inference to the tidyselected columns", {
  data <- as_bq_data(tibble::tibble(age = c(40, 55), flag = c(0, 1)))
  result <- infer_types(data, age)

  expect_identical(variables_of(result)$type, c("continuous", NA))
  expect_identical(variables_of(result)$type_source, c("inferred", NA))
})

test_that("infer_types() passes max_levels to type inference", {
  data <- as_bq_data(tibble::tibble(category = c("a", "b", "c")))

  expect_identical(variables_of(infer_types(data, max_levels = 2L))$type, "unknown")
  expect_identical(variables_of(infer_types(data, max_levels = 3L))$type, "nominal")
})

test_that("infer_types() reports an invalid max_levels as a package error", {
  data <- as_bq_data(tibble::tibble(category = letters[1:3]))

  expect_error(
    infer_types(data, max_levels = NA_integer_),
    class = "bq_error_invalid_inference"
  )
})

test_that("infer_types() validates data and selection", {
  data <- as_bq_data(tibble::tibble(x = 1, y = 2))

  expect_error(
    infer_types(tibble::tibble(x = 1)),
    class = "bq_error_invalid_data"
  )
  expect_error(infer_types(data, missing), class = "bq_error_invalid_selection")
  expect_error(
    infer_types(data, tidyselect::starts_with("absent")),
    class = "bq_error_invalid_selection"
  )
  expect_error(
    infer_types(data, tidyselect::starts_with("absent")),
    "at least one column"
  )
})
