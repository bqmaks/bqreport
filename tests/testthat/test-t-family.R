test_that("t_family() records the declared test family", {
  analysis <- t_family(
    comparisons = "reference",
    reference = "control",
    var_equal = TRUE,
    p_adjust = "bonferroni",
    conf_level = 0.9
  )

  expect_s3_class(
    analysis,
    c("bq_t_family", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "t_family",
      family = "reference",
      reference = "control",
      var_equal = TRUE,
      effect_size = "none",
      p_adjust_method = "bonferroni",
      conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities")$supplied_results,
    "comparison_family"
  )
})

test_that("t_family() validates constructor decisions", {
  expect_error(
    t_family(comparisons = "reference"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    t_family(comparisons = "pairwise", reference = "control"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    t_family(p_adjust = "unknown"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    t_family(conf_level = 1),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    t_family(effect_size = "cohen_d"),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("t_family() declares requested pairwise effect sizes", {
  skip_if_not_installed("effectsize")

  for (effect_size in c("cohens_d", "hedges_g")) {
    analysis <- t_family(effect_size = effect_size)

    expect_identical(
      attr(analysis, "specification")$effect_size,
      effect_size
    )
    expect_identical(
      attr(analysis, "capabilities")$supplied_results,
      c("comparison_family", "pairwise_effect_size")
    )
    expect_identical(
      attr(analysis, "capabilities")$suggested_dependencies,
      "effectsize"
    )
  }
})

test_that("t_family() computes and adjusts only the declared family", {
  outcome <- c(2, 3, 5, 6, 8, 9, 4, 7, 10, 12, 13, 15)
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
    analysis <- t_family(
      comparisons = declaration$family,
      reference = declaration$reference,
      var_equal = FALSE,
      p_adjust = "holm",
      conf_level = 0.9
    )
    result <- run_comparison(analysis, outcome, group)
    pairs <- compile_comparison_family(
      levels(group), declaration$family, declaration$reference
    )
    direct <- lapply(seq_len(nrow(pairs)), function(position) {
      stats::t.test(
        outcome[group == pairs$comparison_value[[position]]],
        outcome[group == pairs$reference_value[[position]]],
        var.equal = FALSE,
        conf.level = 0.9
      )
    })
    raw_p <- vapply(direct, function(test) test$p.value, double(1))

    expect_equal(
      result$comparisons$estimate,
      vapply(direct, function(test) {
        unname(test$estimate[[1L]] - test$estimate[[2L]])
      }, double(1)),
      tolerance = 1e-12
    )
    expect_equal(
      result$comparisons$std_error,
      vapply(direct, function(test) unname(test$stderr), double(1)),
      tolerance = 1e-12
    )
    expect_equal(result$comparisons$p_value_raw, raw_p, tolerance = 1e-12)
    expect_equal(
      result$comparisons$p_value,
      stats::p.adjust(raw_p, method = "holm"),
      tolerance = 1e-12
    )
    expect_identical(result$comparisons$family, rep(declaration$family, nrow(pairs)))
    expect_identical(
      result$comparisons$interval_scope,
      rep("individual_unadjusted", nrow(pairs))
    )
    expect_named(result, c("comparisons", "sample_flow"))
  }
})

test_that("t_family() requires two observations per compared group", {
  analysis <- t_family(comparisons = "reference", reference = "a")

  expect_error(
    run_comparison(
      analysis,
      outcome = c(1, 2, 3, 4, 5),
      group = c("a", "a", "b", "b", "c")
    ),
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("t_family() supplies variance-compatible standardized effects", {
  skip_if_not_installed("effectsize")
  outcome <- c(2, 3, 5, 6, 8, 9, 4, 7, 10, 12, 13, 15)
  group <- factor(
    rep(c("control", "low", "high"), each = 4L),
    levels = c("control", "low", "high")
  )
  settings <- list(
    list(effect_size = "cohens_d", var_equal = FALSE),
    list(effect_size = "hedges_g", var_equal = TRUE)
  )

  for (setting in settings) {
    result <- run_comparison(
      t_family(
        var_equal = setting$var_equal,
        effect_size = setting$effect_size,
        conf_level = 0.9
      ),
      outcome,
      group
    )
    pairs <- compile_comparison_family(levels(group), "pairwise", NULL)
    direct <- lapply(seq_len(nrow(pairs)), function(position) {
      effect_function <- if (setting$effect_size == "cohens_d") {
        effectsize::cohens_d
      } else {
        effectsize::hedges_g
      }
      effect_function(
        outcome[group == pairs$comparison_value[[position]]],
        outcome[group == pairs$reference_value[[position]]],
        pooled_sd = setting$var_equal,
        ci = 0.9,
        verbose = FALSE
      )
    })
    effect_column <- if (setting$effect_size == "cohens_d") {
      "Cohens_d"
    } else {
      "Hedges_g"
    }

    expect_equal(
      result$comparisons$effect_size,
      vapply(direct, function(value) value[[effect_column]], double(1)),
      tolerance = 1e-12
    )
    expect_equal(
      result$comparisons$effect_conf_low,
      vapply(direct, function(value) value$CI_low, double(1)),
      tolerance = 1e-12
    )
    expect_equal(
      result$comparisons$effect_conf_high,
      vapply(direct, function(value) value$CI_high, double(1)),
      tolerance = 1e-12
    )
    expect_identical(
      result$comparisons$effect_size_type,
      rep(setting$effect_size, nrow(pairs))
    )
    expect_identical(
      result$comparisons$effect_size_method,
      rep(
        if (setting$var_equal) "pooled_sd" else "unpooled_sd",
        nrow(pairs)
      )
    )
    expect_identical(
      result$comparisons$effect_size_correction,
      rep(
        if (setting$effect_size == "hedges_g") {
          "small_sample_bias"
        } else {
          "none"
        },
        nrow(pairs)
      )
    )
    expect_true(all(is.na(result$comparisons$effect_std_error)))
    expect_identical(
      result$comparisons$effect_interval_scope,
      rep("individual_unadjusted", nrow(pairs))
    )
    expect_identical(
      result$comparisons$effect_ci_method,
      rep("noncentral_t", nrow(pairs))
    )
  }
})
