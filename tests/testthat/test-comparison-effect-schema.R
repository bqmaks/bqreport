test_that("comparison providers share one effect-size schema", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  skip_if_not_installed("PMCMRplus", minimum_version = "1.9.12")
  outcome <- c(2, 3, 5, 6, 8, 9, 4, 7, 10, 12, 13, 15)
  group <- factor(
    rep(c("control", "low", "high"), each = 4L),
    levels = c("control", "low", "high")
  )
  providers <- list(
    t_family(),
    mann_whitney_family(exact = FALSE),
    brunner_munzel_family(),
    dunn_test(),
    tukey_test(),
    dunnett_test(reference = "control"),
    games_howell_test()
  )
  effect_columns <- c(
    "effect_size_type", "effect_size", "effect_std_error",
    "effect_size_method", "effect_size_correction", "effect_conf_low",
    "effect_conf_high", "effect_conf_level", "effect_interval_scope",
    "effect_ci_method", "effect_ci_clamped"
  )
  comparison_names <- lapply(providers, function(provider) {
    result <- run_comparison(provider, outcome, group)
    expect_identical(names(result$comparisons)[13L], "std_error")
    expect_identical(
      names(result$comparisons)[14:24],
      effect_columns
    )
    names(result$comparisons)
  })

  for (position in seq_along(comparison_names)[-1L]) {
    expect_identical(comparison_names[[position]], comparison_names[[1L]])
  }
})
