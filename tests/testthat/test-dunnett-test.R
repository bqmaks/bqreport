test_that("dunnett_test() records its fixed reference family", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  analysis <- dunnett_test(reference = "control")

  expect_s3_class(
    analysis,
    c("bq_dunnett_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "dunnett_test",
      family = "reference",
      reference = "control",
      estimand = "mean_difference",
      p_adjust_method = "dunnett_single_step"
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$supplied_results,
    "comparison_family"
  )
})

test_that("dunnett_test() validates its reference", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  expect_error(
    dunnett_test(NA_character_),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("dunnett_test() matches PMCMRplus with an arbitrary reference", {
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  outcome <- c(2, 3, 5, 6, 8, 9, 4, 7, 10, 12, 13, 15)
  group <- factor(
    rep(c("low", "control", "high"), each = 4L),
    levels = c("low", "control", "high")
  )
  result <- run_comparison(
    dunnett_test(reference = "control"), outcome, group
  )
  engine_group <- factor(
    as.character(group),
    levels = c("control", "low", "high")
  )
  direct <- PMCMRplus::dunnettTest(
    outcome, engine_group, alternative = "two.sided"
  )

  expect_identical(result$comparisons$reference_value, c("control", "control"))
  expect_identical(result$comparisons$comparison_value, c("low", "high"))
  expect_equal(
    result$comparisons$statistic,
    c(direct$statistic["low", "control"], direct$statistic["high", "control"]),
    tolerance = 1e-12
  )
  expect_equal(
    result$comparisons$p_value,
    c(direct$p.value["low", "control"], direct$p.value["high", "control"]),
    tolerance = 1e-12
  )
  expect_identical(
    result$comparisons$p_adjust_method,
    rep("dunnett_single_step", 2L)
  )
  expect_named(result, c("comparisons", "sample_flow"))
})
