test_that("games_howell_test() declares its fixed post hoc family", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  analysis <- games_howell_test()

  expect_s3_class(
    analysis,
    c("bq_games_howell_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "games_howell_test",
      family = "pairwise",
      estimand = "mean_difference",
      p_adjust_method = "games_howell_studentized_range"
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$supplied_results,
    "comparison_family"
  )
})

test_that("games_howell_test() matches PMCMRplus", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  outcome <- c(2, 3, 4, 5, 8, 12, 20, 4, 7, 9, 11, 14, 18, 25, 30)
  group <- factor(
    c(rep("control", 4L), rep("low", 3L), rep("high", 8L)),
    levels = c("control", "low", "high")
  )
  result <- run_comparison(games_howell_test(), outcome, group)
  direct <- PMCMRplus::gamesHowellTest(outcome, group)
  pairs <- compile_comparison_family(levels(group), "pairwise")
  direct_statistic <- vapply(seq_len(nrow(pairs)), function(position) {
    direct$statistic[
      pairs$comparison_value[[position]],
      pairs$reference_value[[position]]
    ]
  }, double(1))
  direct_p <- vapply(seq_len(nrow(pairs)), function(position) {
    direct$p.value[
      pairs$comparison_value[[position]],
      pairs$reference_value[[position]]
    ]
  }, double(1))

  expect_equal(result$comparisons$statistic, direct_statistic, tolerance = 1e-12)
  expect_equal(result$comparisons$p_value_adjusted, direct_p, tolerance = 1e-12)
  expect_identical(
    result$comparisons$p_adjust_method,
    rep("games_howell_studentized_range", 3L)
  )
  expect_identical(result$comparisons$variance_assumption, rep("unequal", 3L))
  expect_named(result, c("analysis", "specification", "comparisons", "sample_flow"))
})
