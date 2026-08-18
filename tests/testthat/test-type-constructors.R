test_that("type_continuous() declares a continuous type without category metadata", {
  specification <- type_continuous()

  expect_s3_class(specification, "bq_type", exact = TRUE)
  expect_identical(
    unclass(specification),
    list(
      type = "continuous",
      event = NA_character_,
      reference = NA_character_,
      levels = character()
    )
  )
})

test_that("type_count() declares a count type without category metadata", {
  specification <- type_count()

  expect_s3_class(specification, "bq_type", exact = TRUE)
  expect_identical(
    unclass(specification),
    list(
      type = "count",
      event = NA_character_,
      reference = NA_character_,
      levels = character()
    )
  )
})

test_that("type_binary() declares its event in the common registry representation", {
  expect_identical(
    unclass(type_binary(1)),
    list(
      type = "binary",
      event = "1",
      reference = NA_character_,
      levels = character()
    )
  )
  expect_identical(type_binary(TRUE)$event, "TRUE")
  expect_identical(type_binary("case")$event, "case")
  expect_s3_class(type_binary(1), "bq_type", exact = TRUE)
})

test_that("type_binary() requires one non-missing atomic event", {
  expect_error(type_binary(), class = "bq_error_invalid_type_spec")
  expect_error(type_binary(), "`event` is required")

  for (event in list(NULL, NA, c("case", "control"), list("case"))) {
    expect_error(type_binary(event), class = "bq_error_invalid_type_spec")
  }
  expect_error(type_binary(NA), "one non-missing atomic value")
})

test_that("type_ordinal() preserves the declared order in the common representation", {
  specification <- type_ordinal(c("low", "medium", "high"))

  expect_s3_class(specification, "bq_type", exact = TRUE)
  expect_identical(
    unclass(specification),
    list(
      type = "ordinal",
      event = NA_character_,
      reference = NA_character_,
      levels = c("low", "medium", "high")
    )
  )
  expect_identical(type_ordinal(1:3)$levels, c("1", "2", "3"))
})

test_that("type_ordinal() requires at least three distinct non-missing levels", {
  expect_error(type_ordinal(), class = "bq_error_invalid_type_spec")
  expect_error(type_ordinal(), "`levels` is required")

  for (levels in list(NULL, list("low", "medium", "high"), matrix(1:4, 2))) {
    expect_error(type_ordinal(levels), class = "bq_error_invalid_type_spec")
  }
  expect_error(type_ordinal(c("low", "high")), "at least three")
  expect_error(type_ordinal(c("low", NA, "high")), "non-missing")
  expect_error(type_ordinal(c("low", "high", "high")), "must not contain duplicates")
})

test_that("type_nominal() declares its reference in the common representation", {
  expect_identical(
    unclass(type_nominal("control")),
    list(
      type = "nominal",
      event = NA_character_,
      reference = "control",
      levels = character()
    )
  )
  expect_identical(type_nominal(0)$reference, "0")
  expect_s3_class(type_nominal("control"), "bq_type", exact = TRUE)
})

test_that("type_nominal() requires one non-missing atomic reference", {
  expect_error(type_nominal(), class = "bq_error_invalid_type_spec")
  expect_error(type_nominal(), "`reference` is required")

  for (reference in list(NULL, NA, c("a", "b"), list("a"))) {
    expect_error(type_nominal(reference), class = "bq_error_invalid_type_spec")
  }
  expect_error(type_nominal(NA), "one non-missing atomic value")
})
