test_that("bootstrap_control() declares the default bootstrap policy", {
  control <- bootstrap_control()

  expect_s3_class(control, "bq_bootstrap_control", exact = TRUE)
  expect_identical(
    unclass(control),
    list(
      method = "ordinary",
      engine = "boot",
      iterations = 2000L,
      conf_type = "bca",
      seed = NULL,
      weight_type = NULL
    )
  )
})

test_that("bootstrap_control() stores an explicit policy", {
  control <- bootstrap_control(
    iterations = 5000,
    conf_type = "percentile",
    seed = 2026
  )

  expect_identical(
    unclass(control),
    list(
      method = "ordinary",
      engine = "boot",
      iterations = 5000L,
      conf_type = "percentile",
      seed = 2026L,
      weight_type = NULL
    )
  )
  expect_identical(bootstrap_control(conf_type = "basic")$conf_type, "basic")
})

test_that("bootstrap_control() declares fractional exponential weights", {
  implicit <- bootstrap_control(method = "fractional")
  explicit <- bootstrap_control(
    method = "fractional", weight_type = "exponential"
  )

  expect_identical(implicit, explicit)
  expect_identical(
    unclass(implicit),
    list(
      method = "fractional",
      engine = "fwb",
      iterations = 2000L,
      conf_type = "bca",
      seed = NULL,
      weight_type = "exponential"
    )
  )
})

test_that("bootstrap_control() validates method and weight type together", {
  for (method in list(NULL, character(), NA_character_, "bayesian", TRUE)) {
    expect_error(
      bootstrap_control(method = method),
      class = "bq_error_invalid_bootstrap_control"
    )
  }
  expect_error(
    bootstrap_control(weight_type = "exponential"),
    class = "bq_error_invalid_bootstrap_control"
  )
  for (weight_type in list("exp", "pois", "multinom", "mammen", TRUE)) {
    expect_error(
      bootstrap_control(method = "fractional", weight_type = weight_type),
      class = "bq_error_invalid_bootstrap_control"
    )
  }
})

test_that("bootstrap_control() requires a positive whole iteration count", {
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
    "2000"
  )

  for (iterations in invalid_values) {
    expect_error(
      bootstrap_control(iterations = iterations),
      class = "bq_error_invalid_bootstrap_control"
    )
  }
})

test_that("bootstrap_control() accepts only declared interval methods", {
  invalid_values <- list(
    NULL,
    character(),
    NA_character_,
    c("bca", "basic"),
    "perc",
    "normal",
    TRUE
  )

  for (conf_type in invalid_values) {
    expect_error(
      bootstrap_control(conf_type = conf_type),
      class = "bq_error_invalid_bootstrap_control"
    )
  }
})

test_that("bootstrap_control() validates an optional seed", {
  expect_null(bootstrap_control(seed = NULL)$seed)
  expect_identical(bootstrap_control(seed = 0)$seed, 0L)

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
      bootstrap_control(seed = seed),
      class = "bq_error_invalid_bootstrap_control"
    )
  }
})
