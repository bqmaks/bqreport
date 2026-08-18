test_that("weighted_relative_effect() has the declared group orientation", {
  expect_identical(
    weighted_relative_effect(3, 1, x_weights = 1, y_weights = 1),
    1
  )
  expect_identical(
    weighted_relative_effect(1, 3, x_weights = 1, y_weights = 1),
    0
  )
  expect_identical(
    weighted_relative_effect(2, 2, x_weights = 1, y_weights = 1),
    0.5
  )
})

test_that("weighted_relative_effect() matches the unweighted pairwise definition", {
  x <- c(1, 2, 2, 5)
  y <- c(0, 2, 3)
  pairwise <- outer(x, y, function(x_value, y_value) {
    as.double(x_value > y_value) + 0.5 * as.double(x_value == y_value)
  })

  expect_equal(
    weighted_relative_effect(x, y, rep(1, length(x)), rep(1, length(y))),
    mean(pairwise),
    tolerance = 1e-15
  )
})

test_that("weighted_relative_effect() applies weights within both samples", {
  result <- weighted_relative_effect(
    x = c(1, 3),
    y = c(0, 2),
    x_weights = c(1, 3),
    y_weights = c(2, 1)
  )

  expect_equal(result, 11 / 12, tolerance = 1e-15)
})

test_that("weighted_relative_effect() is invariant to weight scale", {
  x <- c(1, 4, 6)
  y <- c(2, 3)
  expected <- weighted_relative_effect(x, y, c(1, 2, 3), c(4, 5))

  expect_equal(
    weighted_relative_effect(x, y, c(10, 20, 30), c(0.4, 0.5)),
    expected,
    tolerance = 1e-15
  )
})

test_that("weighted_relative_effect() permits zero-weight observations", {
  expect_identical(
    weighted_relative_effect(
      x = c(1, 10), y = c(2, 20),
      x_weights = c(0, 1), y_weights = c(1, 0)
    ),
    1
  )
})

test_that("weighted_relative_effect() validates samples and weights", {
  expect_error(
    weighted_relative_effect(numeric(), 1, numeric(), 1),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    weighted_relative_effect(c(1, NA), 1, c(1, 1), 1),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    weighted_relative_effect(c(1, 2), 1, 1, 1),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    weighted_relative_effect(c(1, 2), 1, c(0, 0), 1),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    weighted_relative_effect(c(1, 2), 1, c(1, -1), 1),
    class = "bq_error_invalid_analysis_input"
  )
})
