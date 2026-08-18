brunner_munzel_input <- function(outcome, group, reference_value) {
  group <- factor(group, levels = unique(group))
  list(
    data = tibble::tibble(
      .row_id = seq_along(outcome), .outcome = outcome, .group = group
    ),
    context = list(
      analysis_id = "a001", test_id = "t001", outcome_var_id = "v001",
      group_var_id = "v002", strata_var_id = NA_character_,
      reference_value = reference_value,
      group_levels = tibble::tibble(
        var_id = rep("v002", 2L), value = levels(group), position = 1:2
      )
    )
  )
}

test_that("brunner_munzel_test() returns an inspectable analytic function", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  analysis <- brunner_munzel_test(inference = "logit", conf_level = 0.9)

  expect_s3_class(
    analysis,
    c("bq_brunner_munzel_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "brunner_munzel_test", hypothesis = "two_sided",
      margin_lower = NA_real_, margin_upper = NA_real_,
      benefit = NA_character_, inference = "logit", permutation = NULL,
      bootstrap = NULL, conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$suggested_dependencies,
    "TOSTER (>= 0.9.0)"
  )
})

test_that("brunner_munzel_test() validates its declaration", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  for (value in list(NULL, "less", NA_character_, TRUE)) {
    expect_error(
      brunner_munzel_test(hypothesis = value),
      class = "bq_error_invalid_analysis_function"
    )
  }
  expect_error(
    brunner_munzel_test(hypothesis = "equivalence", margin = 0.5),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    brunner_munzel_test(hypothesis = "noninferiority", margin = 0.1),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    brunner_munzel_test(inference = "permutation"),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("brunner_munzel_test() matches TOSTER asymptotic inference", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- brunner_munzel_input(
    c(7, 8, 9, 10, 11, 1, 2, 4, 5, 6, NA),
    c(rep("treatment", 5L), rep("control", 6L)),
    "control"
  )
  result <- brunner_munzel_test()(input$data, input$context)
  direct <- suppressMessages(TOSTER::brunner_munzel(
    c(7, 8, 9, 10, 11), c(1, 2, 4, 5, 6), test_method = "t"
  ))

  expect_equal(result$tests$estimate, unname(direct$estimate), tolerance = 1e-12)
  expect_equal(result$tests$statistic, unname(direct$statistic), tolerance = 1e-12)
  expect_equal(result$tests$df, unname(direct$parameter), tolerance = 1e-12)
  expect_equal(result$tests$p_value, direct$p.value, tolerance = 1e-12)
  expect_equal(result$tests$conf_low, direct$conf.int[1L], tolerance = 1e-12)
  expect_equal(result$sample_flow$n_missing, c(0L, 1L))
})

test_that("brunner_munzel_test() orients directional inference by benefit", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- brunner_munzel_input(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    rep(c("treatment", "control"), each = 5L), "control"
  )
  result <- brunner_munzel_test(
    hypothesis = "superiority", margin = 0.05, benefit = "lower"
  )(input$data, input$context)
  direct <- suppressMessages(TOSTER::brunner_munzel(
    6:10, 1:5, alternative = "greater", mu = 0.55, test_method = "t"
  ))

  expect_equal(result$tests$raw_estimate, 0, tolerance = 1e-15)
  expect_equal(result$tests$benefit_estimate, 1, tolerance = 1e-15)
  expect_equal(result$tests$estimate, unname(direct$estimate), tolerance = 1e-12)
  expect_equal(result$tests$p_value, direct$p.value, tolerance = 1e-12)
})

test_that("brunner_munzel_test() uses a TOST interval for equivalence", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- brunner_munzel_input(
    c(2, 3, 4, 5, 6, 1, 3, 4, 6, 7),
    rep(c("new", "reference"), each = 5L), "reference"
  )
  result <- brunner_munzel_test(
    hypothesis = "equivalence", margin = 0.2
  )(input$data, input$context)

  expect_identical(result$tests$margin_lower, -0.2)
  expect_identical(result$tests$margin_upper, 0.2)
  expect_equal(result$tests$interval_conf_level, 0.9, tolerance = 1e-15)
})

test_that("brunner_munzel_test() rejects boundary logit inference", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- brunner_munzel_input(
    c(6:10, 1:5), rep(c("high", "low"), each = 5L), "low"
  )

  expect_error(
    brunner_munzel_test(inference = "logit")(input$data, input$context),
    "exactly zero or one",
    class = "bq_error_analysis_runtime"
  )
})

test_that("brunner_munzel_test() performs declared random permutations", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- brunner_munzel_input(
    c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    rep(c("new", "reference"), each = 5L), "reference"
  )
  control <- permutation_control(iterations = 99L, seed = 2026L)
  analysis <- brunner_munzel_test(
    inference = "permutation", permutation = control
  )

  set.seed(81)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)
  expect_identical(.Random.seed, state_before)
  set.seed(2026)
  direct <- suppressMessages(TOSTER::brunner_munzel(
    c(3, 5, 7, 8, 10), c(1, 2, 4, 6, 9), test_method = "perm",
    R = 99L, p_method = "plusone"
  ))

  expect_equal(result$tests$p_value, direct$p.value, tolerance = 1e-12)
  expect_true(is.na(result$tests$df))
  expect_identical(result$tests$permutation_sampling, "random")
  expect_identical(result$tests$permutation_p_method, "plusone")
  expect_identical(result$tests$permutation_iterations_requested, 99L)
  expect_identical(result$tests$permutation_iterations_performed, 99L)
  expect_identical(result$tests$permutation_seed, 2026L)
})

test_that("brunner_munzel_test() prevents automatic exact enumeration", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- brunner_munzel_input(
    c(1, 2, 3, 4, 5, 6), rep(c("a", "b"), each = 3L), "b"
  )
  analysis <- brunner_munzel_test(
    inference = "permutation",
    permutation = permutation_control(iterations = 20L, seed = 1L)
  )

  expect_error(
    analysis(input$data, input$context),
    "exact enumeration is not part",
    class = "bq_error_analysis_runtime"
  )
})

test_that("brunner_munzel_test() applies an ordinary bootstrap interval", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  skip_if_not_installed("boot")
  input <- brunner_munzel_input(
    c(2, 3, 5, 7, 8, 10, 1, 4, 6, 9, 11, 12),
    rep(c("new", "reference"), each = 6L), "reference"
  )
  control <- bootstrap_control(
    method = "ordinary", iterations = 299L,
    conf_type = "percentile", seed = 2027L
  )
  analysis <- brunner_munzel_test(bootstrap = control, conf_level = 0.9)

  set.seed(91)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)

  expect_identical(.Random.seed, state_before)
  expect_true(is.finite(result$tests$std_error))
  expect_true(result$tests$conf_low <= result$tests$conf_high)
  expect_identical(result$tests$interval_conf_level, 0.9)
  expect_identical(result$tests$ci_method, "bootstrap_percentile")
  expect_false(result$tests$ci_clamped)
  expect_identical(result$tests$bootstrap_method, "ordinary")
  expect_identical(result$tests$bootstrap_engine, "boot")
  expect_true(is.na(result$tests$bootstrap_weight_type))
  expect_identical(result$tests$bootstrap_iterations_requested, 299L)
  expect_identical(result$tests$bootstrap_iterations_valid, 299L)
  expect_identical(result$tests$bootstrap_seed, 2027L)
  expect_identical(
    analysis(input$data, input$context)$tests,
    result$tests
  )
})

test_that("brunner_munzel_test() applies a fractional weighted bootstrap", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  skip_if_not_installed("fwb")
  input <- brunner_munzel_input(
    c(2, 3, 5, 7, 8, 10, 1, 4, 6, 9, 11, 12),
    rep(c("new", "reference"), each = 6L), "reference"
  )
  control <- bootstrap_control(
    method = "fractional", iterations = 299L,
    conf_type = "percentile", seed = 2028L
  )
  analysis <- brunner_munzel_test(bootstrap = control, conf_level = 0.9)

  set.seed(92)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)

  expect_identical(.Random.seed, state_before)
  expect_true(is.finite(result$tests$std_error))
  expect_true(result$tests$conf_low <= result$tests$conf_high)
  expect_identical(result$tests$interval_conf_level, 0.9)
  expect_identical(
    result$tests$ci_method,
    "fractional_bootstrap_percentile"
  )
  expect_identical(result$tests$bootstrap_method, "fractional")
  expect_identical(result$tests$bootstrap_engine, "fwb")
  expect_identical(result$tests$bootstrap_weight_type, "exponential")
  expect_identical(result$tests$bootstrap_iterations_requested, 299L)
  expect_identical(result$tests$bootstrap_iterations_valid, 299L)
  expect_identical(result$tests$bootstrap_seed, 2028L)
  expect_identical(
    analysis(input$data, input$context)$tests,
    result$tests
  )
})
