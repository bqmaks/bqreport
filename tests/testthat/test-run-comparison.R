test_that("run_comparison() runs two-group analyses from vectors", {
  outcome <- c(8, 10, 12, 1, 2, 6)
  group <- rep(c("new", "control"), each = 3L)

  t_result <- run_comparison(
    t_test(), outcome, group, reference = "control"
  )
  wilcox_result <- run_comparison(
    mann_whitney_test(), outcome, group, reference = "control"
  )

  expect_named(t_result, c("tests", "estimates", "sample_flow"))
  expect_equal(
    t_result$tests$statistic,
    unname(stats::t.test(outcome[group == "new"], outcome[group == "control"])$statistic),
    tolerance = 1e-12
  )
  expect_equal(
    wilcox_result$tests$p_value,
    stats::wilcox.test(
      outcome[group == "new"], outcome[group == "control"],
      exact = TRUE, correct = TRUE, conf.int = TRUE
    )$p.value,
    tolerance = 1e-12
  )
  expect_identical(t_result$tests$outcome_var_id, ".outcome")
  expect_identical(t_result$sample_flow$group_value, c("new", "control"))
})

test_that("run_comparison() reads named data-frame columns", {
  data <- data.frame(
    response = c(1, 2, 4, 3, 5, 8),
    arm = rep(c("a", "b", "c"), each = 2L)
  )

  kw_result <- run_comparison(
    kruskal_wallis_test(), "response", "arm", data = data
  )
  anova_result <- run_comparison(
    oneway_anova(effect_size = "none"),
    "response", "arm", data = data
  )

  expect_equal(
    kw_result$tests$p_value,
    stats::kruskal.test(response ~ arm, data = data)$p.value,
    tolerance = 1e-12
  )
  expect_equal(
    anova_result$tests$p_value,
    stats::anova(stats::lm(response ~ arm, data = data))[["Pr(>F)"]][1L],
    tolerance = 1e-12
  )
  expect_identical(kw_result$tests$outcome_var_id, "response")
  expect_identical(kw_result$sample_flow$group_value, c("a", "b", "c"))
})

test_that("run_comparison() respects declared factor and ordinal order", {
  outcome <- ordered(
    c("high", "low", "medium", "low"),
    levels = c("low", "medium", "high")
  )
  group <- factor(c("new", "control", "new", "control"),
    levels = c("control", "new")
  )

  result <- run_comparison(
    mann_whitney_test(), outcome, group, reference = "control"
  )

  expect_identical(result$sample_flow$group_value, c("control", "new"))
  expect_identical(result$sample_flow$n_used, c(2L, 2L))
})

test_that("run_comparison() validates its public inputs", {
  expect_error(
    run_comparison(mean, 1:4, rep(c("a", "b"), each = 2L)),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    run_comparison(t_test(), 1:3, c("a", "b"), reference = "a"),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    run_comparison(t_test(), 1:4, rep(c("a", "b"), each = 2L)),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    run_comparison(
      kruskal_wallis_test(), 1:4, rep(c("a", "b"), each = 2L),
      reference = "a"
    ),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    run_comparison(
      oneway_anova(effect_size = "none"),
      "missing", "arm", data = data.frame(value = 1:2, arm = c("a", "b"))
    ),
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("run_comparison() supports Brunner-Munzel without package data objects", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")

  result <- run_comparison(
    brunner_munzel_test(),
    c(8, 10, 12, 1, 2, 6),
    rep(c("new", "control"), each = 3L),
    reference = "control"
  )

  expect_named(result, c("tests", "estimates", "sample_flow"))
  expect_identical(result$tests$test, "brunner_munzel")
})
