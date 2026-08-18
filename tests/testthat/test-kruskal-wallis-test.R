kruskal_wallis_input <- function(outcome, group, estimate_id = NA_character_) {
  group <- factor(group, levels = unique(group))
  list(
    data = tibble::tibble(
      .row_id = seq_along(outcome), .outcome = outcome, .group = group
    ),
    context = list(
      analysis_id = "a001", test_id = "t001", estimate_id = estimate_id,
      outcome_var_id = "v001", group_var_id = "v002",
      strata_var_id = NA_character_,
      group_levels = tibble::tibble(
        var_id = rep("v002", nlevels(group)), value = levels(group),
        position = seq_len(nlevels(group))
      )
    )
  )
}

test_that("kruskal_wallis_test() returns an inspectable analytic function", {
  analysis <- kruskal_wallis_test(effect_size = "none", conf_level = 0.9)
  expect_true(is.function(analysis))
  expect_s3_class(
    analysis,
    c("bq_kruskal_wallis_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "kruskal_wallis_test", effect_size = "none",
      inference = "analytical", permutation = NULL, conf_level = 0.9,
      bootstrap = NULL
    )
  )
  capabilities <- attr(analysis, "capabilities")
  expect_identical(capabilities$outcome_types, c("continuous", "ordinal"))
  expect_identical(capabilities$group_min_levels, 2L)
  expect_identical(capabilities$group_max_levels, NA_integer_)
  expect_identical(capabilities$suggested_dependencies, character())
  expect_identical(names(formals(analysis)), c("data", "context"))
})

test_that("kruskal_wallis_test() validates its arguments", {
  for (value in list(NULL, NA_character_, "", "epsilon_squared", 1)) {
    expect_error(
      kruskal_wallis_test(effect_size = value),
      class = "bq_error_invalid_analysis_function"
    )
  }
  for (value in list(NULL, NA_real_, NaN, Inf, 0, 1, c(0.9, 0.95), "0.95")) {
    expect_error(
      kruskal_wallis_test(conf_level = value),
      class = "bq_error_invalid_analysis_function"
    )
  }
  for (value in list(NULL, NA_character_, "", "exact", 1)) {
    expect_error(
      kruskal_wallis_test(inference = value),
      class = "bq_error_invalid_analysis_function"
    )
  }
  expect_error(
    kruskal_wallis_test(inference = "permutation"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    kruskal_wallis_test(permutation = permutation_control()),
    class = "bq_error_invalid_analysis_function"
  )
  for (value in list(TRUE, list(), structure(list(), class = "bq_bootstrap_control"))) {
    expect_error(
      kruskal_wallis_test(
        effect_size = "rank_epsilon_squared", bootstrap = value
      ),
      class = "bq_error_invalid_analysis_function"
    )
  }
  expect_error(
    kruskal_wallis_test(bootstrap = bootstrap_control(seed = 1)),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    kruskal_wallis_test(
      effect_size = "rank_epsilon_squared",
      bootstrap = bootstrap_control(method = "fractional")
    ),
    "does not support fractional weighted bootstrap",
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("kruskal_wallis_test() performs a reproducible random permutation test", {
  input <- kruskal_wallis_input(
    c(1, 2, 4, 5, 3, 7, 8, 10, 6, 9, 11, 12),
    rep(c("a", "b", "c"), each = 4L)
  )
  control <- permutation_control(iterations = 99L, seed = 2038L)
  analysis <- kruskal_wallis_test(
    inference = "permutation", permutation = control
  )

  set.seed(81)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)
  expect_identical(.Random.seed, state_before)

  observed <- unname(as.double(stats::kruskal.test(
    input$data$.outcome, input$data$.group
  )$statistic))
  set.seed(2038)
  exceedances <- sum(replicate(99L, {
    permuted_group <- sample(input$data$.group, replace = FALSE)
    unname(as.double(stats::kruskal.test(
      input$data$.outcome, permuted_group
    )$statistic)) >= observed
  }))

  expect_identical(result$tests$test, "kruskal_wallis")
  expect_equal(result$tests$statistic, observed, tolerance = 1e-12)
  expect_true(is.na(result$tests$df))
  expect_equal(result$tests$p_value, (exceedances + 1) / 100)
  expect_identical(result$tests$inference, "permutation")
  expect_identical(result$tests$permutation_sampling, "random")
  expect_identical(result$tests$permutation_p_method, "plusone")
  expect_identical(result$tests$permutation_iterations_requested, 99L)
  expect_identical(result$tests$permutation_iterations_performed, 99L)
  expect_identical(result$tests$permutation_seed, 2038L)
  expect_identical(
    analysis(input$data, input$context)$tests,
    result$tests
  )
})

test_that("kruskal_wallis_test() does not silently enumerate exact permutations", {
  input <- kruskal_wallis_input(c(1, 2, 3, 4), c("a", "a", "b", "b"))
  analysis <- kruskal_wallis_test(
    inference = "permutation",
    permutation = permutation_control(iterations = 6L, seed = 1L)
  )

  expect_error(
    analysis(input$data, input$context),
    "exact enumeration is not part",
    class = "bq_error_analysis_runtime"
  )
})

test_that("kruskal_wallis_test() matches stats::kruskal.test", {
  input <- kruskal_wallis_input(
    c(1, 2, 4, 5, NA, 8, 9, 11),
    c("a", "a", "b", "b", "b", "c", "c", "c")
  )
  original <- input$data
  result <- kruskal_wallis_test()(input$data, input$context)
  used <- !is.na(input$data$.outcome)
  direct <- stats::kruskal.test(
    input$data$.outcome[used], input$data$.group[used]
  )

  expect_named(result, c("tests", "estimates", "sample_flow"))
  expect_equal(result$tests$statistic, unname(direct$statistic), tolerance = 1e-12)
  expect_equal(result$tests$df, unname(direct$parameter), tolerance = 1e-12)
  expect_equal(result$tests$p_value, direct$p.value, tolerance = 1e-12)
  expect_equal(result$sample_flow$n_total, c(2L, 3L, 3L))
  expect_equal(result$sample_flow$n_missing, c(0L, 1L, 0L))
  expect_equal(result$sample_flow$n_used, c(2L, 2L, 3L))
  expect_identical(input$data, original)
})

test_that("kruskal_wallis_test() rejects a non-finite omnibus result", {
  input <- kruskal_wallis_input(rep(1, 6), rep(c("a", "b"), each = 3L))

  expect_error(
    kruskal_wallis_test()(input$data, input$context),
    "finite omnibus test",
    class = "bq_error_analysis_runtime"
  )
})

test_that("kruskal_wallis_test() computes rank epsilon squared without an implicit bootstrap", {
  skip_if_not_installed("effectsize")
  input <- kruskal_wallis_input(
    c(1, 2, 3, 4, 6, 7, 8, 10, 11),
    rep(c("a", "b", "c"), each = 3L),
    estimate_id = "e001"
  )
  result <- kruskal_wallis_test(
    effect_size = "rank_epsilon_squared"
  )(input$data, input$context)
  direct <- effectsize::rank_epsilon_squared(
    input$data$.outcome, input$data$.group, ci = NULL, verbose = FALSE
  )

  expect_equal(
    result$estimates$estimate,
    direct$rank_epsilon_squared,
    tolerance = 1e-12
  )
  expect_identical(result$estimates$estimator, "kruskal_wallis_H/(n-1)")
  expect_identical(result$estimates$ci_method, "not_computed")
  expect_true(is.na(result$estimates$conf_low))
  expect_true(is.na(result$estimates$conf_high))
  expect_true(is.na(result$estimates$conf_level))
  expect_true(is.na(result$estimates$bootstrap_engine))
})

test_that("kruskal_wallis_test() bootstraps rank epsilon squared through boot", {
  skip_if_not_installed("boot")
  input <- kruskal_wallis_input(
    c(1, 2, 3, 4, 6, 7, 8, 10, 11, 12, 13, 15),
    rep(c("a", "b", "c"), each = 4L),
    estimate_id = "e001"
  )
  control <- bootstrap_control(
    iterations = 499L,
    conf_type = "percentile",
    seed = 2026L
  )
  analysis <- kruskal_wallis_test(
    effect_size = "rank_epsilon_squared",
    conf_level = 0.9,
    bootstrap = control
  )

  set.seed(81)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)

  expect_identical(.Random.seed, state_before)
  expect_true(is.finite(result$estimates$std_error))
  expect_true(result$estimates$conf_low <= result$estimates$conf_high)
  expect_identical(result$estimates$conf_level, 0.9)
  expect_identical(result$estimates$ci_method, "bootstrap_percentile")
  expect_identical(result$estimates$bootstrap_engine, "boot::boot")
  expect_identical(result$estimates$bootstrap_iterations_requested, 499L)
  expect_true(result$estimates$bootstrap_iterations_valid <= 499L)
  expect_true(result$estimates$bootstrap_iterations_valid >= 2L)
  expect_identical(result$estimates$bootstrap_seed, 2026L)
  expect_identical(
    analysis(input$data, input$context)$estimates,
    result$estimates
  )
})

test_that("kruskal_wallis_test() rejects empty observed groups and invalid context", {
  input <- kruskal_wallis_input(c(1, 2, NA), c("a", "a", "b"))
  expect_error(
    kruskal_wallis_test()(input$data, input$context),
    "no observed outcome values",
    class = "bq_error_invalid_analysis_input"
  )

  input <- kruskal_wallis_input(c(1, 2, 3, 4), c("a", "a", "b", "b"))
  input$context$strata_var_id <- "v003"
  expect_error(
    kruskal_wallis_test()(input$data, input$context),
    class = "bq_error_invalid_analysis_input"
  )
})
