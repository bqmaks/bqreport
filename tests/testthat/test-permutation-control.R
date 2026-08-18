test_that("permutation_control() declares the default permutation policy", {
  control <- permutation_control()

  expect_s3_class(control, "bq_permutation_control", exact = TRUE)
  expect_identical(
    unclass(control),
    list(
      sampling = "random",
      iterations = 10000L,
      p_method = "plusone",
      seed = NULL
    )
  )
})

test_that("permutation_control() stores an explicit policy", {
  control <- permutation_control(iterations = 20000, seed = 2026)

  expect_identical(
    unclass(control),
    list(
      sampling = "random",
      iterations = 20000L,
      p_method = "plusone",
      seed = 2026L
    )
  )
})

test_that("permutation_control() requires a positive whole iteration count", {
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
    c(1000, 2000),
    TRUE,
    "10000"
  )

  for (iterations in invalid_values) {
    expect_error(
      permutation_control(iterations = iterations),
      class = "bq_error_invalid_permutation_control"
    )
  }
})

test_that("permutation_control() requires plus-one random p-values", {
  invalid_values <- list(
    NULL,
    character(),
    NA_character_,
    c("plusone", "exact"),
    "exact",
    "auto",
    TRUE
  )

  for (p_method in invalid_values) {
    expect_error(
      permutation_control(p_method = p_method),
      class = "bq_error_invalid_permutation_control"
    )
  }
})

test_that("permutation_control() validates an optional seed", {
  expect_null(permutation_control(seed = NULL)$seed)
  expect_identical(permutation_control(seed = 0)$seed, 0L)

  invalid_values <- list(
    numeric(),
    NA_real_,
    NaN,
    Inf,
    -1,
    1.5,
    .Machine$integer.max + 1,
    c(1, 2),
    TRUE,
    "2026"
  )

  for (seed in invalid_values) {
    expect_error(
      permutation_control(seed = seed),
      class = "bq_error_invalid_permutation_control"
    )
  }
})
