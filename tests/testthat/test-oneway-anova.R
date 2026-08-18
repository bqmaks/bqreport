test_that("oneway_anova() returns an inspectable analytic function", {
  analysis <- oneway_anova(
    var_equal = TRUE,
    effect_size = "eta_squared",
    conf_level = 0.9
  )

  expect_true(is.function(analysis))
  expect_s3_class(
    analysis,
    c("bq_oneway_anova", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "oneway_anova",
      var_equal = TRUE,
      effect_size = "eta_squared",
      inference = "analytical",
      permutation = NULL,
      conf_level = 0.9,
      bootstrap = NULL
    )
  )
  expect_identical(
    attr(analysis, "capabilities"),
    list(
      outcome_types = "continuous",
      outcomes_per_analysis = 1L,
      requires_group = TRUE,
      group_min_levels = 2L,
      group_max_levels = NA_integer_,
      max_strata = 0L,
      supports_covariates = FALSE,
      supports_weights = FALSE,
      supports_clusters = FALSE,
      supports_matched_sets = FALSE,
      provides_fits = FALSE,
      supplied_results = c("omnibus_test", "effect_size"),
      supplied_extractors = character(),
      suggested_dependencies = "effectsize"
    )
  )
  expect_identical(names(formals(analysis)), c("data", "context"))
})

test_that("oneway_anova() records its defaults", {
  analysis <- oneway_anova()

  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "oneway_anova",
      var_equal = TRUE,
      effect_size = "omega_squared",
      inference = "analytical",
      permutation = NULL,
      conf_level = 0.95,
      bootstrap = NULL
    )
  )
})

test_that("oneway_anova() declares only requested results and dependencies", {
  analysis <- oneway_anova(effect_size = "none")
  capabilities <- attr(analysis, "capabilities")

  expect_identical(capabilities$supplied_results, "omnibus_test")
  expect_identical(capabilities$suggested_dependencies, character())
})

test_that("oneway_anova() validates var_equal", {
  invalid <- list(NULL, NA, 1, "yes", c(TRUE, FALSE))

  for (var_equal in invalid) {
    expect_error(
      oneway_anova(var_equal = var_equal),
      class = "bq_error_invalid_analysis_function"
    )
  }

  expect_false(attr(oneway_anova(var_equal = FALSE), "specification")$var_equal)
})

test_that("oneway_anova() validates effect_size", {
  invalid <- list(NULL, NA_character_, "", "cohen_d", c("none", "eta_squared"), 1)

  for (effect_size in invalid) {
    expect_error(
      oneway_anova(effect_size = effect_size),
      class = "bq_error_invalid_analysis_function"
    )
  }

  for (effect_size in c("none", "eta_squared", "omega_squared")) {
    analysis <- oneway_anova(effect_size = effect_size)
    expect_identical(attr(analysis, "specification")$effect_size, effect_size)
  }
})

test_that("oneway_anova() validates conf_level", {
  invalid <- list(NULL, NA_real_, NaN, Inf, -0.1, 0, 1, 1.1, c(0.9, 0.95), "0.95")

  for (conf_level in invalid) {
    expect_error(
      oneway_anova(conf_level = conf_level),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

test_that("oneway_anova() validates permutation inference declarations", {
  for (value in list(NULL, NA_character_, "", "exact", 1)) {
    expect_error(
      oneway_anova(inference = value),
      class = "bq_error_invalid_analysis_function"
    )
  }
  expect_error(
    oneway_anova(inference = "permutation"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    oneway_anova(permutation = permutation_control()),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("oneway_anova() validates bootstrap declarations", {
  for (value in list(TRUE, list(), structure(list(), class = "bq_bootstrap_control"))) {
    expect_error(
      oneway_anova(bootstrap = value),
      class = "bq_error_invalid_analysis_function"
    )
  }
  expect_error(
    oneway_anova(
      effect_size = "none", bootstrap = bootstrap_control(seed = 1L)
    ),
    class = "bq_error_invalid_analysis_function"
  )
})

oneway_anova_input <- function(outcome, group, estimate_id = NA_character_) {
  group <- factor(group, levels = unique(group))
  list(
    data = tibble::tibble(
      .row_id = seq_along(outcome),
      .outcome = outcome,
      .group = group
    ),
    context = list(
      analysis_id = "a001",
      test_id = "t001",
      estimate_id = estimate_id,
      outcome_var_id = "v001",
      group_var_id = "v002",
      strata_var_id = NA_character_,
      group_levels = tibble::tibble(
        var_id = rep("v002", nlevels(group)),
        value = levels(group),
        position = seq_len(nlevels(group))
      )
    )
  )
}

test_that("oneway_anova() returns only its omnibus result", {
  input <- oneway_anova_input(
    outcome = c(2, 3, 5, 6, 9, 10),
    group = rep(c("control", "treatment", "other"), each = 2L)
  )
  original_data <- input$data
  analysis <- oneway_anova(effect_size = "none")
  result <- analysis(input$data, input$context)
  direct_fit <- stats::lm(.outcome ~ .group, data = input$data)
  direct <- stats::anova(direct_fit)

  expect_named(
    result,
    c("tests", "estimates", "sample_flow")
  )
  expect_identical(result$tests$test, "oneway_anova")
  expect_equal(result$tests$statistic, direct[["F value"]][1L], tolerance = 1e-12)
  expect_equal(result$tests$df1, direct[["Df"]][1L], tolerance = 1e-12)
  expect_equal(result$tests$df2, direct[["Df"]][2L], tolerance = 1e-12)
  expect_equal(result$tests$p_value, direct[["Pr(>F)"]][1L], tolerance = 1e-12)
  expect_identical(result$tests$variance_assumption, "equal")
  expect_identical(
    result$estimates,
    tibble::tibble(
      estimate_id = character(),
      analysis_id = character(),
      outcome_var_id = character(),
      estimand = character(),
      estimate = double(),
      std_error = double(),
      conf_low = double(),
      conf_high = double(),
      conf_level = double(),
      estimator = character(),
      ci_method = character(),
      bootstrap_method = character(),
      bootstrap_engine = character(),
      bootstrap_weight_type = character(),
      bootstrap_iterations_requested = integer(),
      bootstrap_iterations_valid = integer(),
      bootstrap_seed = integer()
    )
  )
  expect_identical(
    result$sample_flow,
    tibble::tibble(
      analysis_id = rep("a001", 3L),
      outcome_var_id = rep("v001", 3L),
      group_value = c("control", "treatment", "other"),
      n_total = rep(2L, 3L),
      n_missing = rep(0L, 3L),
      n_used = rep(2L, 3L)
    )
  )
  expect_identical(input$data, original_data)
})

test_that("oneway_anova() reports its missing outcome policy", {
  input <- oneway_anova_input(
    outcome = c(2, NA, 5, 6, 9, NA, 11),
    group = c("control", "control", "treatment", "treatment", "other", "other", "other")
  )
  result <- oneway_anova(effect_size = "none")(input$data, input$context)

  expect_identical(result$sample_flow$n_total, c(2L, 2L, 3L))
  expect_identical(result$sample_flow$n_missing, c(1L, 0L, 1L))
  expect_identical(result$sample_flow$n_used, c(1L, 2L, 2L))
  expect_identical(sum(result$sample_flow$n_used), 5L)
})

test_that("oneway_anova() validates prepared data and context", {
  input <- oneway_anova_input(
    outcome = c(2, 3, 5, 6),
    group = rep(c("control", "treatment"), each = 2L)
  )
  analysis <- oneway_anova(effect_size = "none")

  invalid_data <- as.data.frame(input$data)
  expect_error(
    analysis(invalid_data, input$context),
    class = "bq_error_invalid_analysis_input"
  )

  missing_group <- input$data
  missing_group$.group[1L] <- NA
  expect_error(
    analysis(missing_group, input$context),
    "without missing values",
    class = "bq_error_invalid_analysis_input"
  )

  invalid_context <- input$context
  invalid_context$group_levels$value <- rev(invalid_context$group_levels$value)
  expect_error(
    analysis(input$data, invalid_context),
    "group_levels",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("oneway_anova() rejects unestimable group data", {
  input <- oneway_anova_input(
    outcome = c(2, 3, NA, NA),
    group = rep(c("control", "treatment"), each = 2L)
  )
  analysis <- oneway_anova(effect_size = "none")

  expect_error(
    analysis(input$data, input$context),
    "no observed outcome values",
    class = "bq_error_invalid_analysis_input"
  )

  input <- oneway_anova_input(
    outcome = c(2, 3),
    group = c("control", "treatment")
  )
  expect_error(
    analysis(input$data, input$context),
    "too few observed outcome values",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("oneway_anova() computes classical effect sizes", {
  skip_if_not_installed("effectsize")
  input <- oneway_anova_input(
    outcome = c(2, 3, 5, 6, 9, 10),
    group = rep(c("control", "treatment", "other"), each = 2L),
    estimate_id = "e001"
  )
  analysis <- oneway_anova(effect_size = "eta_squared")
  result <- analysis(input$data, input$context)
  direct_test <- stats::lm(.outcome ~ .group, data = input$data)
  direct_effect <- effectsize::eta_squared(
    direct_test, ci = NULL, verbose = FALSE
  )

  expect_equal(result$estimates$estimate, direct_effect$Eta2, tolerance = 1e-12)
  expect_identical(result$estimates$estimand, "eta_squared")
  expect_identical(
    result$estimates$estimator,
    "effectsize::eta_squared_classical_f"
  )
  expect_identical(result$estimates$ci_method, "not_computed")
})

test_that("oneway_anova() computes Welch tests and approximate effect sizes", {
  skip_if_not_installed("effectsize")
  input <- oneway_anova_input(
    outcome = c(1, 2, 3, 4, 6, 8, 10, 15, 20),
    group = rep(c("a", "b", "c"), each = 3L),
    estimate_id = "e001"
  )
  result <- oneway_anova(
    var_equal = FALSE, effect_size = "omega_squared"
  )(input$data, input$context)
  direct_test <- stats::oneway.test(
    .outcome ~ .group, data = input$data, var.equal = FALSE
  )
  direct_effect <- effectsize::omega_squared(
    direct_test, ci = NULL, verbose = FALSE
  )

  expect_identical(result$tests$test, "welch_anova")
  expect_equal(result$tests$statistic, unname(direct_test$statistic), tolerance = 1e-12)
  expect_equal(result$tests$df1, unname(direct_test$parameter[[1L]]), tolerance = 1e-12)
  expect_equal(result$tests$df2, unname(direct_test$parameter[[2L]]), tolerance = 1e-12)
  expect_equal(result$tests$p_value, direct_test$p.value, tolerance = 1e-12)
  expect_identical(result$tests$variance_assumption, "unequal")
  expect_equal(result$estimates$estimate, direct_effect$Omega2, tolerance = 1e-12)
  expect_identical(
    result$estimates$estimator,
    "effectsize::omega_squared_welch_f_approximation"
  )
})

test_that("oneway_anova() permutes the selected F statistic reproducibly", {
  input <- oneway_anova_input(
    outcome = c(1, 2, 4, 5, 3, 7, 8, 10, 6, 9, 11, 15),
    group = rep(c("a", "b", "c"), each = 4L)
  )

  for (var_equal in c(TRUE, FALSE)) {
    control <- permutation_control(iterations = 99L, seed = 2039L)
    analysis <- oneway_anova(
      var_equal = var_equal, effect_size = "none",
      inference = "permutation", permutation = control
    )

    set.seed(81)
    state_before <- .Random.seed
    result <- analysis(input$data, input$context)
    expect_identical(.Random.seed, state_before)

    statistic <- function(group) {
      test_data <- data.frame(
        outcome = input$data$.outcome,
        group = group
      )
      if (var_equal) {
        fit <- stats::lm(outcome ~ group, data = test_data)
        unname(as.double(stats::anova(fit)[["F value"]][1L]))
      } else {
        unname(as.double(stats::oneway.test(
          outcome ~ group, data = test_data, var.equal = FALSE
        )$statistic))
      }
    }
    observed <- statistic(input$data$.group)
    set.seed(2039)
    exceedances <- sum(replicate(99L, {
      statistic(sample(input$data$.group, replace = FALSE)) >= observed
    }))

    expect_identical(
      result$tests$test,
      if (var_equal) {
        "oneway_anova_permutation"
      } else "welch_anova_permutation"
    )
    expect_equal(result$tests$statistic, observed, tolerance = 1e-12)
    expect_equal(result$tests$p_value, (exceedances + 1) / 100)
    expect_identical(result$tests$inference, "permutation")
    expect_identical(result$tests$permutation_sampling, "random")
    expect_identical(result$tests$permutation_p_method, "plusone")
    expect_identical(result$tests$permutation_iterations_requested, 99L)
    expect_identical(result$tests$permutation_iterations_performed, 99L)
    expect_identical(result$tests$permutation_seed, 2039L)
    expect_identical(
      analysis(input$data, input$context)$tests,
      result$tests
    )
  }
})

test_that("oneway_anova() does not silently enumerate exact permutations", {
  input <- oneway_anova_input(c(1, 2, 3, 4), c("a", "a", "b", "b"))
  analysis <- oneway_anova(
    effect_size = "none", inference = "permutation",
    permutation = permutation_control(iterations = 6L, seed = 1L)
  )

  expect_error(
    analysis(input$data, input$context),
    "exact enumeration is not part",
    class = "bq_error_analysis_runtime"
  )
})

test_that("oneway_anova() rejects non-finite F tests", {
  input <- oneway_anova_input(rep(1, 6), rep(c("a", "b"), each = 3L))

  for (var_equal in c(TRUE, FALSE)) {
    expect_error(
      oneway_anova(var_equal = var_equal, effect_size = "none")(
        input$data,
        input$context
      ),
      class = "bq_error_analysis_runtime"
    )
  }
})

test_that("oneway_anova() bootstraps both effect sizes for both F tests", {
  skip_if_not_installed("boot")
  input <- oneway_anova_input(
    outcome = c(1, 2, 4, 7, 3, 5, 8, 13, 6, 9, 14, 22),
    group = rep(c("a", "b", "c"), each = 4L),
    estimate_id = "e001"
  )

  for (var_equal in c(TRUE, FALSE)) {
    for (effect_size in c("eta_squared", "omega_squared")) {
      analysis <- oneway_anova(
        var_equal = var_equal, effect_size = effect_size, conf_level = 0.9,
        bootstrap = bootstrap_control(
          iterations = 199L, conf_type = "percentile", seed = 2040L
        )
      )
      set.seed(81)
      state_before <- .Random.seed
      result <- analysis(input$data, input$context)

      expect_identical(.Random.seed, state_before)
      expect_true(is.finite(result$estimates$std_error))
      expect_true(result$estimates$conf_low <= result$estimates$conf_high)
      expect_identical(result$estimates$conf_level, 0.9)
      expect_identical(result$estimates$ci_method, "bootstrap_percentile")
      expect_identical(result$estimates$bootstrap_method, "ordinary")
      expect_identical(result$estimates$bootstrap_engine, "boot")
      expect_true(is.na(result$estimates$bootstrap_weight_type))
      expect_identical(result$estimates$bootstrap_iterations_requested, 199L)
      expect_true(result$estimates$bootstrap_iterations_valid >= 2L)
      expect_true(result$estimates$bootstrap_iterations_valid <= 199L)
      expect_identical(result$estimates$bootstrap_seed, 2040L)
      expect_identical(
        analysis(input$data, input$context)$estimates,
        result$estimates
      )
    }
  }
})

test_that("oneway_anova() supports fractional weighted bootstrap", {
  skip_if_not_installed("fwb")
  input <- oneway_anova_input(
    outcome = c(1, 2, 4, 7, 3, 5, 8, 13, 6, 9, 14, 22),
    group = rep(c("a", "b", "c"), each = 4L),
    estimate_id = "e001"
  )

  for (var_equal in c(TRUE, FALSE)) {
    analysis <- oneway_anova(
      var_equal = var_equal, effect_size = "omega_squared", conf_level = 0.9,
      bootstrap = bootstrap_control(
        method = "fractional", iterations = 199L,
        conf_type = "percentile", seed = 2041L
      )
    )
    set.seed(81)
    state_before <- .Random.seed
    result <- analysis(input$data, input$context)

    expect_identical(.Random.seed, state_before)
    expect_true(is.finite(result$estimates$std_error))
    expect_true(result$estimates$conf_low <= result$estimates$conf_high)
    expect_identical(
      result$estimates$ci_method, "fractional_bootstrap_percentile"
    )
    expect_identical(result$estimates$bootstrap_method, "fractional")
    expect_identical(result$estimates$bootstrap_engine, "fwb")
    expect_identical(result$estimates$bootstrap_weight_type, "exponential")
    expect_identical(result$estimates$bootstrap_iterations_requested, 199L)
    expect_identical(result$estimates$bootstrap_iterations_valid, 199L)
    expect_identical(result$estimates$bootstrap_seed, 2041L)
    expect_identical(
      analysis(input$data, input$context)$estimates,
      result$estimates
    )
  }
})

test_that("oneway_anova() combines permutation inference and bootstrap", {
  skip_if_not_installed("boot")
  input <- oneway_anova_input(
    outcome = c(1, 2, 4, 7, 3, 5, 8, 13, 6, 9, 14, 22),
    group = rep(c("a", "b", "c"), each = 4L),
    estimate_id = "e001"
  )
  analysis <- oneway_anova(
    var_equal = FALSE, effect_size = "eta_squared",
    inference = "permutation",
    permutation = permutation_control(iterations = 49L, seed = 2042L),
    bootstrap = bootstrap_control(
      iterations = 99L, conf_type = "percentile", seed = 2043L
    )
  )

  set.seed(81)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)

  expect_identical(.Random.seed, state_before)
  expect_identical(result$tests$test, "welch_anova_permutation")
  expect_identical(result$tests$permutation_seed, 2042L)
  expect_identical(result$estimates$bootstrap_seed, 2043L)
  expect_true(is.finite(result$tests$p_value))
  expect_true(is.finite(result$estimates$std_error))
})
