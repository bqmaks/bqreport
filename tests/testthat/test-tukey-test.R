test_that("tukey_test() returns an inspectable post hoc provider", {
  analysis <- tukey_test(conf_level = 0.9)

  expect_true(is.function(analysis))
  expect_s3_class(
    analysis,
    c("bq_tukey_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "tukey_test",
      family = "pairwise",
      estimand = "mean_difference",
      p_adjust_method = "tukey_single_step",
      conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$supplied_results,
    "comparison_family"
  )
})

test_that("tukey_test() validates its confidence level", {
  for (value in list(NULL, NA_real_, NaN, Inf, 0, 1, c(0.9, 0.95), "0.95")) {
    expect_error(
      tukey_test(value),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

test_that("tukey_test() matches stats::TukeyHSD", {
  outcome <- c(2, 3, 5, 6, 8, 9, 4, 7, 10, 12)
  group <- factor(
    c(rep("control", 3L), rep("dose-low", 3L), rep("dose-high", 4L)),
    levels = c("control", "dose-low", "dose-high")
  )
  result <- run_comparison(
    tukey_test(conf_level = 0.9), outcome, group
  )
  direct_data <- data.frame(outcome = outcome, group = group)
  direct_fit <- stats::aov(outcome ~ group, data = direct_data)
  direct <- stats::TukeyHSD(
    direct_fit, "group", conf.level = 0.9
  )[["group"]]

  expect_named(
    result,
    c("comparisons", "sample_flow")
  )
  expect_equal(
    result$comparisons$estimate, unname(direct[, "diff"]), tolerance = 1e-12
  )
  residual_variance <- stats::deviance(direct_fit) /
    stats::df.residual(direct_fit)
  group_n <- table(group)
  pairs <- utils::combn(levels(group), 2L)
  expected_std_error <- sqrt(
    residual_variance *
      (1 / as.double(group_n[pairs[1L, ]]) +
        1 / as.double(group_n[pairs[2L, ]]))
  )
  expect_equal(
    result$comparisons$std_error,
    expected_std_error,
    tolerance = 1e-12
  )
  expect_equal(
    result$comparisons$conf_low, unname(direct[, "lwr"]), tolerance = 1e-12
  )
  expect_equal(
    result$comparisons$conf_high, unname(direct[, "upr"]), tolerance = 1e-12
  )
  expect_equal(
    result$comparisons$p_value, unname(direct[, "p adj"]), tolerance = 1e-12
  )
  expect_identical(
    result$comparisons$reference_value,
    c("control", "control", "dose-low")
  )
  expect_identical(
    result$comparisons$comparison_value,
    c("dose-low", "dose-high", "dose-high")
  )
  expect_identical(result$comparisons$family_size, rep(3L, 3L))
  expect_true(all(result$comparisons$conf_low <= result$comparisons$conf_high))
  expect_identical(
    result$comparisons$interval_scope,
    rep("familywise_simultaneous", 3L)
  )
})

test_that("tukey_test() preserves external RNG state", {
  set.seed(91)
  state_before <- .Random.seed

  run_comparison(
    tukey_test(),
    outcome = c(1, 2, 4, 3, 5, 8),
    group = rep(c("a", "b", "c"), each = 2L)
  )

  expect_identical(.Random.seed, state_before)
})
