test_that("brunner_munzel_family() records its family and inference", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  analysis <- brunner_munzel_family(
    comparisons = "reference",
    reference = "control",
    inference = "logit",
    p_adjust = "bonferroni",
    conf_level = 0.9
  )

  expect_s3_class(
    analysis,
    c("bq_brunner_munzel_family", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "brunner_munzel_family",
      family = "reference",
      reference = "control",
      inference = "logit",
      p_adjust_method = "bonferroni",
      conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$supplied_results,
    c("comparison_family", "pairwise_effect_size")
  )
})

test_that("brunner_munzel_family() validates constructor decisions", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  expect_error(
    brunner_munzel_family(comparisons = "reference"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    brunner_munzel_family(
      comparisons = "pairwise", reference = "control"
    ),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    brunner_munzel_family(inference = "permutation"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    brunner_munzel_family(p_adjust = "unknown"),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("brunner_munzel_family() computes each declared family", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  outcome <- c(1, 3, 5, 8, 2, 4, 7, 10, 6, 9, 11, 13)
  group <- factor(
    rep(c("control", "low", "high"), each = 4L),
    levels = c("control", "low", "high")
  )
  declarations <- list(
    list(family = "pairwise", reference = NULL),
    list(family = "reference", reference = "control"),
    list(family = "consecutive", reference = NULL)
  )

  for (declaration in declarations) {
    result <- run_comparison(
      brunner_munzel_family(
        comparisons = declaration$family,
        reference = declaration$reference,
        inference = "asymptotic",
        p_adjust = "holm",
        conf_level = 0.9
      ),
      outcome,
      group
    )
    pairs <- compile_comparison_family(
      levels(group), declaration$family, declaration$reference
    )
    direct <- lapply(seq_len(nrow(pairs)), function(position) {
      suppressMessages(TOSTER::brunner_munzel(
        outcome[group == pairs$comparison_value[[position]]],
        outcome[group == pairs$reference_value[[position]]],
        test_method = "t",
        alpha = 0.1
      ))
    })
    relative_effect <- vapply(
      direct, function(test) unname(test$estimate), double(1)
    )
    raw_p <- vapply(direct, function(test) test$p.value, double(1))

    expect_equal(
      result$comparisons$estimate,
      relative_effect,
      tolerance = 1e-12
    )
    direct_std_error <- vapply(
      direct, function(test) unname(test$stderr), double(1)
    )
    expect_equal(
      result$comparisons$std_error,
      direct_std_error,
      tolerance = 1e-12
    )
    expect_equal(
      result$comparisons$effect_size,
      2 * relative_effect - 1,
      tolerance = 1e-12
    )
    expect_equal(
      result$comparisons$effect_std_error,
      2 * direct_std_error,
      tolerance = 1e-12
    )
    direct_conf_low <- vapply(
      direct, function(test) unname(test$conf.int[[1L]]), double(1)
    )
    direct_conf_high <- vapply(
      direct, function(test) unname(test$conf.int[[2L]]), double(1)
    )
    expect_equal(
      result$comparisons$effect_conf_low,
      2 * direct_conf_low - 1,
      tolerance = 1e-12
    )
    expect_equal(
      result$comparisons$effect_conf_high,
      2 * direct_conf_high - 1,
      tolerance = 1e-12
    )
    expect_identical(
      result$comparisons$effect_size_type,
      rep("cliffs_delta", nrow(pairs))
    )
    expect_identical(
      result$comparisons$effect_size_method,
      rep("linear_transform_of_relative_effect", nrow(pairs))
    )
    expect_identical(
      result$comparisons$effect_interval_scope,
      rep("individual_unadjusted", nrow(pairs))
    )
    expect_equal(result$comparisons$p_value_raw, raw_p, tolerance = 1e-12)
    expect_equal(
      result$comparisons$p_value,
      stats::p.adjust(raw_p, method = "holm"),
      tolerance = 1e-12
    )
    expect_identical(result$comparisons$reference_value, pairs$reference_value)
    expect_identical(result$comparisons$comparison_value, pairs$comparison_value)
    expect_named(result, c("comparisons", "sample_flow"))
    expect_identical(result$sample_flow$n_used, rep(4L, 3L))
  }
})

test_that("brunner_munzel_family() uses lexical character order", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  result <- run_comparison(
    brunner_munzel_family(comparisons = "consecutive"),
    outcome = c(1, 5, 9, 2, 4, 8, 3, 6, 7),
    group = rep(c("zeta", "alpha", "middle"), each = 3L)
  )

  expect_identical(
    result$comparisons$reference_value,
    c("alpha", "middle")
  )
  expect_identical(
    result$comparisons$comparison_value,
    c("middle", "zeta")
  )
})
