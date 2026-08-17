test_that("set_type() records an explicit type without changing the data", {
  data <- as_bq_data(data.frame(age = c(40, 55, 61)))
  result <- set_type(data, age, continuous())

  expect_registry_aligned(result)
  expect_identical(tibble::as_tibble(result), tibble::as_tibble(data))
  expect_identical(variables_of(result)$type, "continuous")
  expect_identical(variables_of(result)$event, NA_character_)
  expect_identical(variables_of(result)$event_source, NA_character_)
  expect_identical(variables_of(result)$reference, NA_character_)
  expect_identical(variables_of(result)$type_source, "explicit")
  expect_identical(levels_of(result), levels_of(data))
})

test_that("set_type() records binary event and nominal reference", {
  data <- as_bq_data(data.frame(group = factor("case", levels = c("control", "case"))))

  binary_data <- set_type(data, group, binary("case"))
  nominal_data <- set_type(data, group, nominal("control"))

  expect_identical(variables_of(binary_data)$event, "case")
  expect_identical(variables_of(binary_data)$event_source, "explicit")
  expect_identical(variables_of(binary_data)$reference, NA_character_)
  expect_identical(variables_of(nominal_data)$event, NA_character_)
  expect_identical(variables_of(nominal_data)$event_source, NA_character_)
  expect_identical(variables_of(nominal_data)$reference, "control")
})

test_that("set_type() expands ordinal levels into their flat registry", {
  data <- as_bq_data(data.frame(severity = c("low", "high")))
  result <- set_type(data, severity, ordinal(c("low", "medium", "high")))

  expect_registry_aligned(result)
  expect_identical(
    levels_of(result),
    tibble::tibble(
      var_id = rep("v001", 3),
      value = c("low", "medium", "high"),
      position = 1:3
    )
  )
})

test_that("set_type() replaces stale levels of the selected variable only", {
  data <- labelled_data()
  result <- set_type(data, sex, nominal("f"))

  expect_registry_aligned(result)
  expect_identical(nrow(levels_of(result)), 0L)
  expect_identical(variables_of(result)$type[2], "nominal")
  expect_identical(variables_of(result)$event[2], NA_character_)
  expect_identical(variables_of(result)$event_source[2], NA_character_)
  expect_identical(variables_of(result)$reference[2], "f")
})

test_that("set_type() uses declared factor levels when validating categories", {
  data <- as_bq_data(data.frame(group = factor("case", levels = c("control", "case"))))

  expect_no_error(set_type(data, group, nominal("control")))
  expect_no_error(set_type(data, group, binary("control")))
})

test_that("set_type() rejects metadata that is incompatible with the column", {
  data <- as_bq_data(data.frame(
    group = c("case", "control"),
    severity = c("low", "high")
  ))

  expect_error(
    set_type(data, group, binary("absent")),
    class = "bq_error_type_mismatch"
  )
  expect_error(set_type(data, group, binary("absent")), "variable `group`")
  expect_error(
    set_type(data, group, nominal("absent")),
    class = "bq_error_type_mismatch"
  )
  expect_error(
    set_type(data, severity, ordinal(c("low", "medium", "severe"))),
    class = "bq_error_type_mismatch"
  )
  expect_error(
    set_type(data, severity, ordinal(c("low", "medium", "severe"))),
    'level "high"'
  )
})

test_that("set_type() validates its data, specification and selection", {
  data <- as_bq_data(data.frame(age = 40, bmi = 22))

  expect_error(
    set_type(tibble::tibble(age = 40), age, continuous()),
    class = "bq_error_invalid_data"
  )
  expect_error(set_type(data, age, "continuous"), class = "bq_error_invalid_type_spec")
  expect_error(set_type(data, age), class = "bq_error_invalid_type_spec")
  unknown <- structure(
    list(
      type = "unknown",
      event = NA_character_,
      reference = NA_character_,
      levels = character()
    ),
    class = "bq_type"
  )
  expect_error(set_type(data, age, unknown), class = "bq_error_invalid_type_spec")
  expect_error(set_type(data, missing, continuous()), class = "bq_error_invalid_selection")
  expect_error(set_type(data, c(age, bmi), continuous()), "exactly one column")
  expect_error(set_type(data, tidyselect::everything(), continuous()), "not 2")
})
