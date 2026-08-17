test_that("enumerate_values() declares a display rule", {
  rule <- enumerate_values()

  expect_s3_class(
    rule,
    c("bq_enumerate_values", "bq_display_rule"),
    exact = TRUE
  )
  expect_identical(
    unclass(rule),
    list(
      kind = "enumerate_values",
      max_n = 2L,
      display_statistics = FALSE
    )
  )
})

test_that("enumerate_values() stores whole numeric thresholds as integers", {
  expect_identical(enumerate_values(3)$max_n, 3L)
})

test_that("enumerate_values() can retain statistics in presentation", {
  rule <- enumerate_values(display_statistics = TRUE)

  expect_true(rule$display_statistics)
})

test_that("enumerate_values() requires one positive whole number", {
  invalid_values <- list(
    NULL,
    numeric(),
    NA_real_,
    NaN,
    Inf,
    0,
    -1,
    1.5,
    .Machine$integer.max + 1,
    c(1, 2),
    TRUE,
    "2"
  )

  for (max_n in invalid_values) {
    expect_error(
      enumerate_values(max_n),
      class = "bq_error_invalid_display_rule"
    )
  }
})

test_that("enumerate_values() requires a logical display choice", {
  for (display_statistics in list(NA, logical(), c(TRUE, FALSE), 1, "yes")) {
    expect_error(
      enumerate_values(display_statistics = display_statistics),
      class = "bq_error_invalid_display_rule"
    )
  }
})
