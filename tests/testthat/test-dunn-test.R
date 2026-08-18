test_that("dunn_test() records its engine and family", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  analysis <- dunn_test(
    comparisons = "reference",
    reference = "control",
    p_adjust = "bonferroni"
  )

  expect_s3_class(
    analysis,
    c("bq_dunn_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "dunn_test",
      family = "reference",
      reference = "control",
      p_adjust_method = "bonferroni"
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$suggested_dependencies,
    "PMCMRplus (>= 1.9.12)"
  )
})

test_that("dunn_test() validates constructor decisions", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  expect_error(
    dunn_test(comparisons = "reference"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    dunn_test(p_adjust = "unknown"),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("dunn_test() selects and adjusts each declared family", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  outcome <- c(1, 3, 5, 7, 2, 4, 8, 10, 6, 9, 11, 13)
  group <- factor(
    rep(c("control", "low", "high"), each = 4L),
    levels = c("control", "low", "high")
  )
  engine <- PMCMRplus::kwAllPairsDunnTest(
    outcome, group, p.adjust.method = "none"
  )
  declarations <- list(
    list(family = "pairwise", reference = NULL),
    list(family = "reference", reference = "low"),
    list(family = "consecutive", reference = NULL)
  )

  for (declaration in declarations) {
    result <- run_comparison(
      dunn_test(
        comparisons = declaration$family,
        reference = declaration$reference,
        p_adjust = "holm"
      ),
      outcome,
      group
    )
    pairs <- compile_comparison_family(
      levels(group), declaration$family, declaration$reference
    )
    direct_statistic <- direct_p <- numeric(nrow(pairs))
    for (position in seq_len(nrow(pairs))) {
      reference_position <- match(pairs$reference_value[[position]], levels(group))
      comparison_position <- match(pairs$comparison_value[[position]], levels(group))
      later <- levels(group)[max(reference_position, comparison_position)]
      earlier <- levels(group)[min(reference_position, comparison_position)]
      direct_statistic[[position]] <- engine$statistic[later, earlier] *
        if (comparison_position > reference_position) 1 else -1
      direct_p[[position]] <- engine$p.value[later, earlier]
    }

    expect_equal(result$comparisons$statistic, direct_statistic, tolerance = 1e-12)
    expect_equal(result$comparisons$p_value, direct_p, tolerance = 1e-12)
    expect_equal(
      result$comparisons$p_value_adjusted,
      stats::p.adjust(direct_p, method = "holm"),
      tolerance = 1e-12
    )
    expect_identical(result$comparisons$family, rep(declaration$family, nrow(pairs)))
    expect_identical(
      result$comparisons$engine,
      rep("PMCMRplus::kwAllPairsDunnTest", nrow(pairs))
    )
    expect_named(result, c("analysis", "specification", "comparisons", "sample_flow"))
  }
})
