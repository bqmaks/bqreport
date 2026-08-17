test_that("set_summary_format() records named formats in order", {
  data <- labelled_data()
  formats <- c(
    "Mean (SD)" = "{mean} ({sd})",
    "Median (Q1; Q3)" = "{median} ({q1}; {q3})"
  )

  result <- set_summary_format(data, c(age, bmi), formats)

  expect_registry_aligned(result)
  expect_identical(tibble::as_tibble(result), tibble::as_tibble(data))
  expect_identical(
    summary_formats(result),
    tibble::tibble(
      var_id = rep(c("v001", "v003"), each = 2),
      format_name = rep(names(formats), 2),
      template = rep(unname(formats), 2),
      position = rep(1:2, 2)
    )
  )
})

test_that("set_summary_format() supports unnamed formats and replacement", {
  data <- set_summary_format(
    labelled_data(),
    age,
    c("{mean} ({sd})", "{median} ({q1}; {q3})")
  )

  result <- set_summary_format(data, age, "{mean}")

  expect_registry_aligned(result)
  expect_identical(
    summary_formats(result),
    tibble::tibble(
      var_id = "v001",
      format_name = NA_character_,
      template = "{mean}",
      position = 1L
    )
  )
})

test_that("summary formats follow their variable through dplyr", {
  data <- set_summary_format(
    labelled_data(),
    age,
    c("Mean (SD)" = "{mean} ({sd})")
  )

  renamed <- dplyr::rename(data, years = age)
  selected <- dplyr::select(data, bmi, age)
  rewritten <- dplyr::mutate(data, age = age + 1)
  dropped <- dplyr::select(data, -age)

  expect_identical(summary_formats(renamed), summary_formats(data))
  expect_identical(summary_formats(selected), summary_formats(data))
  expect_identical(summary_formats(rewritten), summary_formats(data))
  expect_identical(summary_formats(dropped), new_summary_format_registry())
})

test_that("summary formats survive every metadata-preserving path", {
  data <- set_summary_format(
    labelled_data(),
    age,
    c("Mean (SD)" = "{mean} ({sd})")
  )

  row_sliced <- dplyr::filter(data, age > 40)
  replaced <- data
  replaced$age <- replaced$age + 1
  dropped <- data
  dropped[["age"]] <- NULL
  plain <- tibble::as_tibble(data)

  expect_identical(summary_formats(row_sliced), summary_formats(data))
  expect_identical(summary_formats(replaced), summary_formats(data))
  expect_identical(summary_formats(dropped), new_summary_format_registry())
  expect_null(attr(plain, "summary_formats"))
})

test_that("set_summary_format() validates formats", {
  data <- labelled_data()

  for (formats in list(
    character(),
    NA_character_,
    "",
    c(good = "{mean}", "{sd}"),
    c(same = "{mean}", same = "{sd}"),
    "mean",
    "{mean",
    "mean}",
    "{}"
  )) {
    expect_error(
      set_summary_format(data, age, formats),
      class = "bq_error_invalid_summary_format"
    )
  }
})

test_that("summary format accessors require bq_data", {
  expect_error(
    set_summary_format(tibble::tibble(age = 1), age, "{mean}"),
    class = "bq_error_invalid_data"
  )
  expect_error(
    summary_formats(tibble::tibble(age = 1)),
    class = "bq_error_invalid_data"
  )
})
