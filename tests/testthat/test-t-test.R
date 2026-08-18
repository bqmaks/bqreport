test_that("t_test() returns an inspectable analytic function", {
  analysis <- t_test(var_equal = TRUE, conf_level = 0.9)

  expect_true(is.function(analysis))
  expect_s3_class(
    analysis,
    c("bq_t_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "t_test",
      var_equal = TRUE,
      hypothesis = "two_sided",
      margin_lower = NA_real_,
      margin_upper = NA_real_,
      benefit = NA_character_,
      effect_size = "none",
      inference = "analytical",
      permutation = NULL,
      bootstrap = NULL,
      conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities"),
    list(
      outcome_types = "continuous",
      group_min_levels = 2L,
      group_max_levels = 2L,
      supplied_results = "test",
      suggested_dependencies = character()
    )
  )
  expect_identical(names(formals(analysis)), c("data", "context"))
})

test_that("t_test() records its defaults", {
  analysis <- t_test()

  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "t_test",
      var_equal = FALSE,
      hypothesis = "two_sided",
      margin_lower = NA_real_,
      margin_upper = NA_real_,
      benefit = NA_character_,
      effect_size = "none",
      inference = "analytical",
      permutation = NULL,
      bootstrap = NULL,
      conf_level = 0.95
    )
  )
})

test_that("t_test() records equivalence margins", {
  symmetric <- t_test(hypothesis = "equivalence", margin = 2)
  asymmetric <- t_test(
    hypothesis = "equivalence",
    margin = c(lower = -1, upper = 2)
  )

  expect_identical(
    attr(symmetric, "specification")[c("margin_lower", "margin_upper")],
    list(margin_lower = -2, margin_upper = 2)
  )
  expect_identical(
    attr(asymmetric, "specification")[c("margin_lower", "margin_upper")],
    list(margin_lower = -1, margin_upper = 2)
  )
})

test_that("t_test() records directional hypotheses", {
  noninferiority <- t_test(
    hypothesis = "noninferiority",
    margin = 3,
    benefit = "higher"
  )
  superiority <- t_test(
    hypothesis = "superiority",
    margin = 0,
    benefit = "lower"
  )

  expect_identical(
    attr(noninferiority, "specification")[c("margin_lower", "benefit")],
    list(margin_lower = -3, benefit = "higher")
  )
  expect_identical(
    attr(superiority, "specification")[c("margin_lower", "benefit")],
    list(margin_lower = 0, benefit = "lower")
  )
})

test_that("t_test() validates hypothesis", {
  invalid <- list(NULL, NA_character_, "different", c("two_sided", "superiority"), 1)

  for (hypothesis in invalid) {
    expect_error(
      t_test(hypothesis = hypothesis),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

test_that("t_test() validates margins for each hypothesis", {
  invalid_two_sided <- list(0, 1, c(lower = -1, upper = 1))
  for (margin in invalid_two_sided) {
    expect_error(
      t_test(margin = margin),
      class = "bq_error_invalid_analysis_function"
    )
  }

  invalid_equivalence <- list(NULL, 0, -1, c(-1, 1), c(upper = 1, lower = -1), c(lower = 0, upper = 1))
  for (margin in invalid_equivalence) {
    expect_error(
      t_test(hypothesis = "equivalence", margin = margin),
      class = "bq_error_invalid_analysis_function"
    )
  }

  for (margin in list(NULL, 0, -1, Inf, c(1, 2))) {
    expect_error(
      t_test(hypothesis = "noninferiority", margin = margin, benefit = "higher"),
      class = "bq_error_invalid_analysis_function"
    )
  }

  for (margin in list(NULL, -1, Inf, c(0, 1))) {
    expect_error(
      t_test(hypothesis = "superiority", margin = margin, benefit = "higher"),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

test_that("t_test() validates benefit direction", {
  for (benefit in list(NULL, NA_character_, "both", c("higher", "lower"), 1)) {
    expect_error(
      t_test(hypothesis = "superiority", margin = 0, benefit = benefit),
      class = "bq_error_invalid_analysis_function"
    )
  }

  expect_error(
    t_test(benefit = "higher"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    t_test(hypothesis = "equivalence", margin = 1, benefit = "higher"),
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("t_test() declares requested effect sizes and dependencies", {
  for (effect_size in c("cohens_d", "hedges_g")) {
    analysis <- t_test(effect_size = effect_size)
    capabilities <- attr(analysis, "capabilities")

    expect_identical(capabilities$supplied_results, c("test", "effect_size"))
    expect_identical(capabilities$suggested_dependencies, "effectsize")
  }
})

test_that("t_test() declares the selected bootstrap engine dependency", {
  skip_if_not_installed("fwb")

  analysis <- t_test(
    bootstrap = bootstrap_control(method = "fractional")
  )

  expect_identical(
    attr(analysis, "capabilities")$suggested_dependencies,
    "fwb"
  )
})

test_that("t_test() validates var_equal", {
  invalid <- list(NULL, NA, 1, "yes", c(TRUE, FALSE))

  for (var_equal in invalid) {
    expect_error(
      t_test(var_equal = var_equal),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

test_that("t_test() accepts only its implemented effect-size policies", {
  invalid <- list(NULL, NA_character_, "", "cohen_d", c("none", "none"), 1)

  for (effect_size in invalid) {
    expect_error(
      t_test(effect_size = effect_size),
      class = "bq_error_invalid_analysis_function"
    )
  }

  for (effect_size in c("none", "cohens_d", "hedges_g")) {
    expect_identical(
      attr(t_test(effect_size = effect_size), "specification")$effect_size,
      effect_size
    )
  }
})

test_that("t_test() validates conf_level", {
  invalid <- list(NULL, NA_real_, NaN, Inf, -0.1, 0, 1, 1.1, c(0.9, 0.95), "0.95")

  for (conf_level in invalid) {
    expect_error(
      t_test(conf_level = conf_level),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

t_test_input <- function(outcome, group, reference_value, estimate_id = NA_character_) {
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
      reference_value = reference_value,
      group_levels = tibble::tibble(
        var_id = rep("v002", nlevels(group)),
        value = levels(group),
        position = seq_len(nlevels(group))
      )
    )
  )
}

test_that("t_test() executes Welch test in comparison-minus-reference direction", {
  input <- t_test_input(
    outcome = c(8, 10, 12, 1, 2, 6, 7),
    group = c(rep("treatment", 3L), rep("control", 4L)),
    reference_value = "control"
  )
  original_data <- input$data
  result <- t_test()(input$data, input$context)
  direct <- stats::t.test(
    c(8, 10, 12),
    c(1, 2, 6, 7),
    var.equal = FALSE,
    conf.level = 0.95
  )

  expect_named(result, c("tests", "estimates", "sample_flow"))
  expect_identical(
    result$tests,
    tibble::tibble(
      test_id = "t001",
      analysis_id = "a001",
      outcome_var_id = "v001",
      test = "welch_t",
      reference_value = "control",
      comparison_value = "treatment",
      hypothesis = "two_sided",
      benefit = NA_character_,
      margin_lower = NA_real_,
      margin_upper = NA_real_,
      raw_estimate = mean(c(8, 10, 12)) - mean(c(1, 2, 6, 7)),
      benefit_estimate = NA_real_,
      estimate = mean(c(8, 10, 12)) - mean(c(1, 2, 6, 7)),
      std_error = unname(as.double(direct$stderr)),
      statistic = unname(as.double(direct$statistic)),
      statistic_lower = NA_real_,
      statistic_upper = NA_real_,
      df = unname(as.double(direct$parameter)),
      conf_low = unname(as.double(direct$conf.int[1L])),
      conf_high = unname(as.double(direct$conf.int[2L])),
      p_value = unname(as.double(direct$p.value)),
      p_lower = NA_real_,
      p_upper = NA_real_,
      requested_conf_level = 0.95,
      interval_conf_level = 0.95,
      ci_method = "t_distribution",
      bootstrap_method = NA_character_,
      bootstrap_engine = NA_character_,
      bootstrap_weight_type = NA_character_,
      bootstrap_iterations_requested = NA_integer_,
      bootstrap_iterations_valid = NA_integer_,
      bootstrap_seed = NA_integer_,
      inference = "analytical",
      permutation_sampling = NA_character_,
      permutation_p_method = NA_character_,
      permutation_iterations_requested = NA_integer_,
      permutation_iterations_performed = NA_integer_,
      permutation_seed = NA_integer_
    )
  )
  expect_identical(
    result$estimates,
    tibble::tibble(
      estimate_id = character(),
      analysis_id = character(),
      outcome_var_id = character(),
      estimand = character(),
      reference_value = character(),
      comparison_value = character(),
      standardizer = character(),
      estimate = double(),
      std_error = double(),
      conf_low = double(),
      conf_high = double(),
      ci_method = character(),
      bootstrap_method = character(),
      bootstrap_engine = character(),
      bootstrap_weight_type = character(),
      bootstrap_iterations_requested = integer(),
      bootstrap_iterations_valid = integer(),
      bootstrap_seed = integer()
    )
  )
  expect_identical(input$data, original_data)
})

test_that("t_test() executes Student test and reports missing outcomes", {
  input <- t_test_input(
    outcome = c(1, 2, NA, 4, 6, 8),
    group = rep(c("control", "treatment"), each = 3L),
    reference_value = "control"
  )
  result <- t_test(var_equal = TRUE, conf_level = 0.9)(
    input$data,
    input$context
  )
  direct <- stats::t.test(
    c(4, 6, 8),
    c(1, 2),
    var.equal = TRUE,
    conf.level = 0.9
  )

  expect_identical(result$tests$test, "student_t")
  expect_identical(result$tests$statistic, unname(as.double(direct$statistic)))
  expect_identical(result$tests$df, unname(as.double(direct$parameter)))
  expect_identical(result$tests$conf_low, unname(as.double(direct$conf.int[1L])))
  expect_identical(result$tests$conf_high, unname(as.double(direct$conf.int[2L])))
  expect_identical(result$sample_flow$n_total, c(3L, 3L))
  expect_identical(result$sample_flow$n_missing, c(1L, 0L))
  expect_identical(result$sample_flow$n_used, c(2L, 3L))
})

test_that("t_test() validates prepared data, context and reference", {
  input <- t_test_input(
    outcome = c(1, 2, 4, 6),
    group = rep(c("control", "treatment"), each = 2L),
    reference_value = "control"
  )
  analysis <- t_test()

  expect_error(
    analysis(as.data.frame(input$data), input$context),
    class = "bq_error_invalid_analysis_input"
  )

  invalid_context <- input$context
  invalid_context$reference_value <- "unknown"
  expect_error(
    analysis(input$data, invalid_context),
    "reference_value",
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

test_that("t_test() rejects unsupported group structure and sample size", {
  input <- t_test_input(
    outcome = c(1, 2, 3, 4, 5, 6),
    group = rep(c("a", "b", "c"), each = 2L),
    reference_value = "a"
  )

  expect_error(
    t_test()(input$data, input$context),
    "exactly 2",
    class = "bq_error_invalid_analysis_input"
  )

  input <- t_test_input(
    outcome = c(1, 2, NA),
    group = c("control", "control", "treatment"),
    reference_value = "control"
  )
  expect_error(
    t_test()(input$data, input$context),
    "has no observed outcome values",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("t_test() reports runtime failures without fallback", {
  input <- t_test_input(
    outcome = rep(1, 4L),
    group = rep(c("control", "treatment"), each = 2L),
    reference_value = "control"
  )

  expect_error(
    t_test()(input$data, input$context),
    "essentially constant",
    class = "bq_error_analysis_runtime"
  )
})

test_that("t_test() executes superiority on the declared benefit scale", {
  input <- t_test_input(
    outcome = c(2, 3, 4, 7, 8, 9),
    group = rep(c("treatment", "control"), each = 3L),
    reference_value = "control"
  )
  result <- t_test(
    hypothesis = "superiority",
    margin = 1,
    benefit = "lower"
  )(input$data, input$context)
  direct <- stats::t.test(
    -c(2, 3, 4),
    -c(7, 8, 9),
    alternative = "greater",
    mu = 1,
    var.equal = FALSE,
    conf.level = 0.95
  )

  expect_identical(result$tests$raw_estimate, -5)
  expect_identical(result$tests$benefit_estimate, 5)
  expect_identical(result$tests$estimate, 5)
  expect_identical(result$tests$statistic, unname(as.double(direct$statistic)))
  expect_identical(result$tests$p_value, unname(as.double(direct$p.value)))
  expect_identical(result$tests$conf_low, unname(as.double(direct$conf.int[1L])))
  expect_identical(result$tests$conf_high, Inf)
})

test_that("t_test() executes noninferiority against the negative margin", {
  input <- t_test_input(
    outcome = c(5, 6, 7, 7, 8, 9),
    group = rep(c("treatment", "control"), each = 3L),
    reference_value = "control"
  )
  result <- t_test(
    var_equal = TRUE,
    hypothesis = "noninferiority",
    margin = 3,
    benefit = "higher"
  )(input$data, input$context)
  direct <- stats::t.test(
    c(5, 6, 7),
    c(7, 8, 9),
    alternative = "greater",
    mu = -3,
    var.equal = TRUE,
    conf.level = 0.95
  )

  expect_identical(result$tests$margin_lower, -3)
  expect_identical(result$tests$benefit_estimate, -2)
  expect_identical(result$tests$p_lower, unname(as.double(direct$p.value)))
  expect_identical(result$tests$p_upper, NA_real_)
  expect_identical(result$tests$p_value, unname(as.double(direct$p.value)))
})

test_that("t_test() executes equivalence as two one-sided tests", {
  input <- t_test_input(
    outcome = c(5, 6, 7, 5.5, 6.5, 7.5),
    group = rep(c("treatment", "control"), each = 3L),
    reference_value = "control"
  )
  result <- t_test(
    hypothesis = "equivalence",
    margin = c(lower = -2, upper = 1),
    conf_level = 0.95
  )(input$data, input$context)
  lower <- stats::t.test(
    c(5, 6, 7), c(5.5, 6.5, 7.5),
    alternative = "greater", mu = -2, var.equal = FALSE, conf.level = 0.95
  )
  upper <- stats::t.test(
    c(5, 6, 7), c(5.5, 6.5, 7.5),
    alternative = "less", mu = 1, var.equal = FALSE, conf.level = 0.95
  )
  interval <- stats::t.test(
    c(5, 6, 7), c(5.5, 6.5, 7.5),
    var.equal = FALSE, conf.level = 0.9
  )

  expect_identical(result$tests$statistic, NA_real_)
  expect_identical(
    result$tests$statistic_lower,
    unname(as.double(lower$statistic))
  )
  expect_identical(
    result$tests$statistic_upper,
    unname(as.double(upper$statistic))
  )
  expect_identical(result$tests$p_lower, unname(as.double(lower$p.value)))
  expect_identical(result$tests$p_upper, unname(as.double(upper$p.value)))
  expect_identical(
    result$tests$p_value,
    max(unname(as.double(lower$p.value)), unname(as.double(upper$p.value)))
  )
  expect_identical(result$tests$conf_low, unname(as.double(interval$conf.int[1L])))
  expect_identical(result$tests$conf_high, unname(as.double(interval$conf.int[2L])))
  expect_identical(result$tests$requested_conf_level, 0.95)
  expect_equal(result$tests$interval_conf_level, 0.9, tolerance = 1e-15)
})

test_that("t_test() requires equivalence confidence above one half", {
  expect_error(
    t_test(hypothesis = "equivalence", margin = 1, conf_level = 0.5),
    "greater than 0.5",
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("t_test() supplies Cohen's d with variance-compatible standardization", {
  input <- t_test_input(
    outcome = c(8, 10, 12, 1, 2, 6, 7),
    group = c(rep("treatment", 3L), rep("control", 4L)),
    reference_value = "control",
    estimate_id = "e001"
  )
  result <- t_test(effect_size = "cohens_d")(
    input$data,
    input$context
  )
  direct <- effectsize::cohens_d(
    c(8, 10, 12),
    c(1, 2, 6, 7),
    pooled_sd = FALSE,
    ci = 0.95,
    verbose = FALSE
  )

  expect_identical(result$estimates$estimand, "cohens_d")
  expect_identical(result$estimates$standardizer, "unpooled_sd")
  expect_identical(result$estimates$estimate, unname(as.double(direct$Cohens_d)))
  expect_identical(result$estimates$conf_low, unname(as.double(direct$CI_low)))
  expect_identical(result$estimates$conf_high, unname(as.double(direct$CI_high)))
})

test_that("t_test() supplies Hedges' g with pooled standardization for Student test", {
  input <- t_test_input(
    outcome = c(8, 10, 12, 1, 2, 6, 7),
    group = c(rep("treatment", 3L), rep("control", 4L)),
    reference_value = "control",
    estimate_id = "e001"
  )
  result <- t_test(var_equal = TRUE, effect_size = "hedges_g")(
    input$data,
    input$context
  )
  direct <- effectsize::hedges_g(
    c(8, 10, 12),
    c(1, 2, 6, 7),
    pooled_sd = TRUE,
    ci = 0.95,
    verbose = FALSE
  )

  expect_identical(result$estimates$estimand, "hedges_g")
  expect_identical(result$estimates$standardizer, "pooled_sd")
  expect_identical(result$estimates$estimate, unname(as.double(direct$Hedges_g)))
  expect_identical(result$estimates$conf_low, unname(as.double(direct$CI_low)))
  expect_identical(result$estimates$conf_high, unname(as.double(direct$CI_high)))
})

test_that("t_test() requires an estimate ID exactly when effect size is requested", {
  input <- t_test_input(
    outcome = c(1, 2, 4, 6),
    group = rep(c("control", "treatment"), each = 2L),
    reference_value = "control"
  )

  expect_error(
    t_test(effect_size = "cohens_d")(input$data, input$context),
    "estimate_id",
    class = "bq_error_invalid_analysis_input"
  )

  input$context$estimate_id <- "e001"
  expect_error(
    t_test()(input$data, input$context),
    "estimate_id",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("t_test() performs a declared studentized permutation test", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- t_test_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )
  control <- permutation_control(iterations = 99L, seed = 2030L)
  analysis <- t_test(inference = "permutation", permutation = control)

  set.seed(80)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)
  expect_identical(.Random.seed, state_before)
  set.seed(2030)
  direct <- suppressMessages(TOSTER::perm_t_test(
    c(3, 5, 7, 8, 10), c(1, 2, 4, 6, 9),
    var.equal = FALSE, R = 99L, p_method = "plusone", keep_perm = FALSE
  ))

  expect_equal(result$tests$p_value, direct$p.value, tolerance = 1e-12)
  expect_equal(result$tests$statistic, unname(direct$statistic), tolerance = 1e-12)
  expect_identical(result$tests$test, "welch_t")
  expect_identical(result$tests$inference, "permutation")
  expect_identical(result$tests$permutation_sampling, "random")
  expect_identical(result$tests$permutation_p_method, "plusone")
  expect_identical(result$tests$permutation_iterations_requested, 99L)
  expect_identical(result$tests$permutation_iterations_performed, 99L)
  expect_identical(result$tests$permutation_seed, 2030L)
})

test_that("t_test() validates permutation declaration and prevents exact mode", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  expect_error(
    t_test(inference = "permutation"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    t_test(permutation = permutation_control(seed = 1L)),
    class = "bq_error_invalid_analysis_function"
  )

  input <- t_test_input(
    outcome = 1:6, group = rep(c("a", "b"), each = 3L),
    reference_value = "b"
  )
  analysis <- t_test(
    inference = "permutation",
    permutation = permutation_control(iterations = 20L, seed = 1L)
  )
  expect_error(
    analysis(input$data, input$context),
    "exact enumeration is not part",
    class = "bq_error_analysis_runtime"
  )
})

test_that("t_test() applies ordinary bootstrap to the mean difference", {
  skip_if_not_installed("boot")
  input <- t_test_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )
  control <- bootstrap_control(
    method = "ordinary", iterations = 299L,
    conf_type = "percentile", seed = 2031L
  )
  analysis <- t_test(bootstrap = control, conf_level = 0.9)

  set.seed(79)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)

  expect_identical(.Random.seed, state_before)
  expect_true(is.finite(result$tests$std_error))
  expect_true(result$tests$conf_low <= result$tests$conf_high)
  expect_identical(result$tests$ci_method, "bootstrap_percentile")
  expect_identical(result$tests$bootstrap_method, "ordinary")
  expect_identical(result$tests$bootstrap_engine, "boot")
  expect_true(is.na(result$tests$bootstrap_weight_type))
  expect_identical(result$tests$bootstrap_iterations_requested, 299L)
  expect_identical(result$tests$bootstrap_iterations_valid, 299L)
  expect_identical(result$tests$bootstrap_seed, 2031L)
  expect_identical(
    analysis(input$data, input$context)$tests,
    result$tests
  )
})

test_that("t_test() bootstraps its declared standardized effect", {
  skip_if_not_installed("boot")
  skip_if_not_installed("effectsize")
  input <- t_test_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference", estimate_id = "e001"
  )
  control <- bootstrap_control(
    iterations = 299L, conf_type = "percentile", seed = 2032L
  )
  result <- t_test(
    var_equal = TRUE, effect_size = "hedges_g", bootstrap = control
  )(input$data, input$context)

  expect_true(is.finite(result$estimates$std_error))
  expect_true(result$estimates$conf_low <= result$estimates$conf_high)
  expect_identical(result$estimates$ci_method, "bootstrap_percentile")
  expect_identical(result$estimates$bootstrap_method, "ordinary")
  expect_identical(result$estimates$bootstrap_engine, "boot")
  expect_identical(result$estimates$bootstrap_iterations_valid, 299L)
  expect_identical(result$estimates$bootstrap_seed, 2032L)
})

test_that("t_test() applies fractional weighted bootstrap", {
  skip_if_not_installed("fwb")
  skip_if_not_installed("effectsize")
  input <- t_test_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference", estimate_id = "e001"
  )
  control <- bootstrap_control(
    method = "fractional", iterations = 299L,
    conf_type = "percentile", seed = 2033L
  )
  analysis <- t_test(effect_size = "cohens_d", bootstrap = control)

  set.seed(78)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)

  expect_identical(.Random.seed, state_before)
  expect_true(is.finite(result$tests$std_error))
  expect_identical(result$tests$ci_method, "fractional_bootstrap_percentile")
  expect_identical(result$tests$bootstrap_method, "fractional")
  expect_identical(result$tests$bootstrap_engine, "fwb")
  expect_identical(result$tests$bootstrap_weight_type, "exponential")
  expect_identical(result$tests$bootstrap_iterations_valid, 299L)
  expect_true(is.finite(result$estimates$std_error))
  expect_identical(
    result$estimates$ci_method,
    "fractional_bootstrap_percentile"
  )
  expect_identical(result$estimates$bootstrap_weight_type, "exponential")
  expect_identical(
    analysis(input$data, input$context)$tests,
    result$tests
  )
})

test_that("t_test() isolates each resampling stage by its own seed", {
  skip_if_not_installed("boot")
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- t_test_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )

  seed_combinations <- list(
    c(permutation = 2034L, bootstrap = 2035L),
    c(permutation = 2034L, bootstrap = NA_integer_),
    c(permutation = NA_integer_, bootstrap = 2035L),
    c(permutation = NA_integer_, bootstrap = NA_integer_)
  )
  preserves_stream <- c(TRUE, FALSE, FALSE, FALSE)

  for (combination in seq_along(seed_combinations)) {
    seeds <- seed_combinations[[combination]]
    permutation_seed <- if (is.na(seeds[["permutation"]])) {
      NULL
    } else {
      seeds[["permutation"]]
    }
    bootstrap_seed <- if (is.na(seeds[["bootstrap"]])) {
      NULL
    } else {
      seeds[["bootstrap"]]
    }
    analysis <- t_test(
      inference = "permutation",
      permutation = permutation_control(
        iterations = 19L,
        seed = permutation_seed
      ),
      bootstrap = bootstrap_control(
        iterations = 49L,
        conf_type = "percentile",
        seed = bootstrap_seed
      )
    )

    set.seed(84)
    state_before <- .Random.seed
    analysis(input$data, input$context)

    expect_identical(
      identical(.Random.seed, state_before),
      preserves_stream[combination]
    )
  }
})
