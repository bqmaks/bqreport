test_that("mann_whitney_family() records the family policy", {
  analysis <- mann_whitney_family(
    comparisons = "reference",
    reference = "control",
    exact = FALSE,
    continuity_correction = FALSE,
    p_adjust = "bonferroni",
    conf_level = 0.9
  )

  expect_s3_class(
    analysis,
    c("bq_mann_whitney_family", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "mann_whitney_family",
      family = "reference",
      reference = "control",
      exact = FALSE,
      continuity_correction = FALSE,
      p_adjust_method = "bonferroni",
      conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$supplied_results,
    c("comparison_family", "pairwise_effect_size")
  )
  expect_identical(
    attr(analysis, "capabilities")$suggested_dependencies,
    character()
  )
})

test_that("mann_whitney_family() validates constructor decisions", {
  expect_error(
    mann_whitney_family(comparisons = "reference"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_family(exact = "sometimes"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_family(p_adjust = "unknown"),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("mann_whitney_family() adjusts only the declared family", {
  outcome <- c(1, 3, 5, 7, 2, 4, 8, 10, 6, 9, 11, 13)
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
      mann_whitney_family(
        comparisons = declaration$family,
        reference = declaration$reference,
        exact = FALSE,
        continuity_correction = FALSE,
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
      stats::wilcox.test(
        outcome[group == pairs$comparison_value[[position]]],
        outcome[group == pairs$reference_value[[position]]],
        exact = FALSE,
        correct = FALSE,
        conf.int = TRUE,
        conf.level = 0.9
      )
    })
    raw_p <- vapply(direct, function(test) test$p.value, double(1))

    expect_equal(
      result$comparisons$estimate,
      vapply(direct, function(test) unname(test$estimate), double(1)),
      tolerance = 1e-12
    )
    expect_equal(result$comparisons$p_value_raw, raw_p, tolerance = 1e-12)
    expect_equal(
      result$comparisons$p_value,
      stats::p.adjust(raw_p, method = "holm"),
      tolerance = 1e-12
    )
    expect_equal(
      result$comparisons$effect_size,
      vapply(seq_len(nrow(pairs)), function(position) {
        comparison_n <- sum(group == pairs$comparison_value[[position]])
        reference_n <- sum(group == pairs$reference_value[[position]])
        2 * unname(direct[[position]]$statistic) /
          (comparison_n * reference_n) - 1
      }, double(1)),
      tolerance = 1e-12
    )
    expect_identical(
      result$comparisons$effect_size_type,
      rep("cliffs_delta", nrow(pairs))
    )
    expect_true(all(is.na(result$comparisons$effect_std_error)))
    expect_true(all(is.na(result$comparisons$effect_conf_low)))
    expect_true(all(is.na(result$comparisons$effect_conf_high)))
    expect_true(all(is.na(result$comparisons$effect_conf_level)))
    expect_true(all(is.na(result$comparisons$effect_interval_scope)))
    expect_true(all(is.na(result$comparisons$effect_ci_method)))
    expect_identical(result$comparisons$family, rep(declaration$family, nrow(pairs)))
    expect_identical(result$comparisons$exact_requested, rep("asymptotic", nrow(pairs)))
    expect_identical(result$comparisons$exact_used, rep(FALSE, nrow(pairs)))
    expect_named(result, c("comparisons", "sample_flow"))
  }
})

test_that("mann_whitney_family() orients Cliff's delta with ties", {
  comparison <- c(1, 2, 2, 4)
  reference <- c(2, 2, 3, 5)
  outcome <- c(reference, comparison)
  group <- factor(
    rep(c("reference", "comparison"), each = 4L),
    levels = c("reference", "comparison")
  )
  result <- run_comparison(
    mann_whitney_family(comparisons = "consecutive", exact = FALSE),
    outcome,
    group
  )
  pairwise_sign <- outer(comparison, reference, function(x, y) sign(x - y))

  expect_equal(
    result$comparisons$effect_size,
    mean(pairwise_sign),
    tolerance = 1e-12
  )
  expect_identical(
    result$comparisons$direction,
    "comparison_minus_reference"
  )
})
