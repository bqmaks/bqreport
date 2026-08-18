test_that("mann_whitney_test() returns an inspectable analytic function", {
  analysis <- mann_whitney_test(
    exact = FALSE,
    continuity_correction = FALSE,
    conf_level = 0.9
  )

  expect_true(is.function(analysis))
  expect_s3_class(
    analysis,
    c("bq_mann_whitney_test", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "mann_whitney_test",
      exact = FALSE,
      continuity_correction = FALSE,
      hypothesis = "two_sided",
      margin_lower = NA_real_,
      margin_upper = NA_real_,
      benefit = NA_character_,
      inference = "analytical",
      permutation = NULL,
      bootstrap = NULL,
      conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities"),
    list(
      outcome_types = c("continuous", "ordinal"),
      outcomes_per_analysis = 1L,
      requires_group = TRUE,
      group_min_levels = 2L,
      group_max_levels = 2L,
      max_strata = 0L,
      supports_covariates = FALSE,
      supports_weights = FALSE,
      supports_clusters = FALSE,
      supports_matched_sets = FALSE,
      provides_fits = FALSE,
      supplied_results = "test",
      supplied_extractors = character(),
      suggested_dependencies = character()
    )
  )
  expect_identical(names(formals(analysis)), c("data", "context"))
})

test_that("mann_whitney_test() records its defaults", {
  analysis <- mann_whitney_test()

  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "mann_whitney_test",
      exact = "auto",
      continuity_correction = TRUE,
      hypothesis = "two_sided",
      margin_lower = NA_real_,
      margin_upper = NA_real_,
      benefit = NA_character_,
      inference = "analytical",
      permutation = NULL,
      bootstrap = NULL,
      conf_level = 0.95
    )
  )
})

test_that("mann_whitney_test() validates exact", {
  invalid <- list(NULL, NA, "exact", 1, c(TRUE, FALSE), c("auto", "auto"))

  for (exact in invalid) {
    expect_error(
      mann_whitney_test(exact = exact),
      class = "bq_error_invalid_analysis_function"
    )
  }

  for (exact in list("auto", TRUE, FALSE)) {
    expect_identical(
      attr(mann_whitney_test(exact = exact), "specification")$exact,
      exact
    )
  }
})

test_that("mann_whitney_test() validates continuity correction", {
  invalid <- list(NULL, NA, 1, "yes", c(TRUE, FALSE))

  for (continuity_correction in invalid) {
    expect_error(
      mann_whitney_test(continuity_correction = continuity_correction),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

test_that("mann_whitney_test() validates conf_level", {
  invalid <- list(NULL, NA_real_, NaN, Inf, -0.1, 0, 1, 1.1, c(0.9, 0.95), "0.95")

  for (conf_level in invalid) {
    expect_error(
      mann_whitney_test(conf_level = conf_level),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

mann_whitney_input <- function(outcome, group, reference_value) {
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

test_that("mann_whitney_test() executes an exact comparison in declared direction", {
  input <- mann_whitney_input(
    outcome = c(8, 10, 12, 1, 2, 6, 7),
    group = c(rep("treatment", 3L), rep("control", 4L)),
    reference_value = "control"
  )
  original_data <- input$data
  result <- mann_whitney_test()(input$data, input$context)
  direct <- stats::wilcox.test(
    c(8, 10, 12),
    c(1, 2, 6, 7),
    exact = TRUE,
    correct = TRUE,
    conf.int = TRUE,
    conf.level = 0.95
  )

  expect_named(result, c("tests", "estimates", "sample_flow"))
  expect_identical(result$tests$statistic, unname(as.double(direct$statistic)))
  expect_identical(result$tests$p_value, unname(as.double(direct$p.value)))
  expect_identical(result$tests$estimate, unname(as.double(direct$estimate)))
  expect_identical(result$tests$conf_low, unname(as.double(direct$conf.int[1L])))
  expect_identical(result$tests$conf_high, unname(as.double(direct$conf.int[2L])))
  expect_identical(
    result$tests$interval_conf_level,
    unname(as.double(attr(direct$conf.int, "conf.level")))
  )
  expect_identical(result$tests$exact_requested, "auto")
  expect_identical(result$tests$exact_used, TRUE)
  expect_identical(result$tests$has_ties, FALSE)
  expect_identical(result$tests$continuity_correction, FALSE)
  expect_identical(input$data, original_data)
})

test_that("mann_whitney_test() validates hypothesis margins and benefit", {
  for (hypothesis in list("unknown", NA_character_, c("two_sided", "superiority"))) {
    expect_error(
      mann_whitney_test(
        hypothesis = hypothesis,
        inference = "permutation",
        permutation = permutation_control(iterations = 9L)
      ),
      class = "bq_error_invalid_analysis_function"
    )
  }
  expect_error(
    mann_whitney_test(margin = 1),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_test(hypothesis = "equivalence", margin = 0),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_test(hypothesis = "noninferiority", margin = 1),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_test(
      hypothesis = "superiority", margin = 0, benefit = "unknown"
    ),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_test(
      hypothesis = "equivalence", margin = 1, conf_level = 0.5
    ),
    class = "bq_error_invalid_analysis_function"
  )

  analysis <- mann_whitney_test(
    hypothesis = "equivalence",
    margin = c(lower = -1, upper = 2)
  )
  specification <- attr(analysis, "specification")
  expect_identical(specification$margin_lower, -1)
  expect_identical(specification$margin_upper, 2)
})

test_that("mann_whitney_test() executes superiority on the location-shift scale", {
  input <- mann_whitney_input(
    outcome = c(8, 10, 12, 1, 2, 6, 7),
    group = c(rep("treatment", 3L), rep("control", 4L)),
    reference_value = "control"
  )
  result <- mann_whitney_test(
    exact = TRUE,
    hypothesis = "superiority",
    margin = 1,
    benefit = "higher"
  )(input$data, input$context)
  direct <- stats::wilcox.test(
    c(8, 10, 12), c(1, 2, 6, 7), alternative = "greater", mu = 1,
    exact = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.95
  )

  expect_identical(result$tests$p_value, unname(as.double(direct$p.value)))
  expect_identical(result$tests$estimate, unname(as.double(direct$estimate)))
  expect_identical(result$tests$raw_estimate, result$tests$estimate)
  expect_identical(result$tests$benefit_estimate, result$tests$estimate)
  expect_identical(result$tests$margin_lower, 1)
})

test_that("mann_whitney_test() orients lower-benefit noninferiority", {
  comparison <- c(1, 2, 3, 4)
  reference <- c(5, 6, 7, 8)
  input <- mann_whitney_input(
    outcome = c(comparison, reference),
    group = rep(c("treatment", "control"), each = 4L),
    reference_value = "control"
  )
  result <- mann_whitney_test(
    exact = TRUE,
    hypothesis = "noninferiority",
    margin = 1,
    benefit = "lower"
  )(input$data, input$context)
  direct <- stats::wilcox.test(
    -comparison, -reference, alternative = "greater", mu = -1,
    exact = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.95
  )

  expect_identical(result$tests$p_value, unname(as.double(direct$p.value)))
  expect_identical(
    result$tests$benefit_estimate,
    unname(as.double(direct$estimate))
  )
  expect_identical(result$tests$raw_estimate, -result$tests$benefit_estimate)
})

test_that("mann_whitney_test() executes equivalence as two one-sided tests", {
  comparison <- c(4, 5, 6, 7, 8)
  reference <- c(3, 4, 5, 6, 7)
  input <- mann_whitney_input(
    outcome = c(comparison, reference),
    group = rep(c("treatment", "control"), each = 5L),
    reference_value = "control"
  )
  result <- mann_whitney_test(
    exact = FALSE,
    continuity_correction = FALSE,
    hypothesis = "equivalence",
    margin = 3,
    conf_level = 0.95
  )(input$data, input$context)
  lower <- stats::wilcox.test(
    comparison, reference, alternative = "greater", mu = -3,
    exact = FALSE, correct = FALSE, conf.int = TRUE, conf.level = 0.95
  )
  upper <- stats::wilcox.test(
    comparison, reference, alternative = "less", mu = 3,
    exact = FALSE, correct = FALSE, conf.int = TRUE, conf.level = 0.95
  )
  interval <- stats::wilcox.test(
    comparison, reference, exact = FALSE, correct = FALSE, conf.int = TRUE,
    conf.level = 0.9
  )

  expect_identical(result$tests$p_lower, unname(as.double(lower$p.value)))
  expect_identical(result$tests$p_upper, unname(as.double(upper$p.value)))
  expect_identical(
    result$tests$p_value,
    max(unname(as.double(lower$p.value)), unname(as.double(upper$p.value)))
  )
  expect_identical(result$tests$statistic_lower, unname(as.double(lower$statistic)))
  expect_identical(result$tests$statistic_upper, unname(as.double(upper$statistic)))
  expect_equal(result$tests$conf_low, unname(as.double(interval$conf.int[1L])))
  expect_equal(result$tests$conf_high, unname(as.double(interval$conf.int[2L])))
  expect_equal(result$tests$interval_conf_level, 0.9)
  expect_false(result$tests$exact_used)
})

test_that("mann_whitney_test() resolves auto to asymptotic for ties", {
  input <- mann_whitney_input(
    outcome = c(3, 4, 4, 1, 2, 4, NA),
    group = c(rep("treatment", 3L), rep("control", 4L)),
    reference_value = "control"
  )
  result <- mann_whitney_test()(input$data, input$context)
  direct <- stats::wilcox.test(
    c(3, 4, 4),
    c(1, 2, 4),
    exact = FALSE,
    correct = TRUE,
    conf.int = TRUE,
    conf.level = 0.95
  )

  expect_identical(result$tests$statistic, unname(as.double(direct$statistic)))
  expect_identical(result$tests$p_value, unname(as.double(direct$p.value)))
  expect_identical(result$tests$exact_used, FALSE)
  expect_identical(result$tests$has_ties, TRUE)
  expect_identical(result$tests$continuity_correction, TRUE)
  expect_identical(result$sample_flow$n_total, c(3L, 4L))
  expect_identical(result$sample_flow$n_missing, c(0L, 1L))
  expect_identical(result$sample_flow$n_used, c(3L, 3L))
})

test_that("mann_whitney_test() rejects ties when exact is required", {
  input <- mann_whitney_input(
    outcome = c(3, 4, 4, 1, 2, 4),
    group = rep(c("treatment", "control"), each = 3L),
    reference_value = "control"
  )

  expect_error(
    mann_whitney_test(exact = TRUE)(input$data, input$context),
    "contain ties",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("mann_whitney_test() validates prepared input and group design", {
  input <- mann_whitney_input(
    outcome = c(1, 2, 4, 6),
    group = rep(c("control", "treatment"), each = 2L),
    reference_value = "control"
  )
  analysis <- mann_whitney_test()

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

  input <- mann_whitney_input(
    outcome = c(1, 2, 3, 4, 5, 6),
    group = rep(c("a", "b", "c"), each = 2L),
    reference_value = "a"
  )
  expect_error(
    analysis(input$data, input$context),
    "exactly two",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("mann_whitney_test() requires observations in both groups", {
  input <- mann_whitney_input(
    outcome = c(1, 2, NA, NA),
    group = rep(c("control", "treatment"), each = 2L),
    reference_value = "control"
  )

  expect_error(
    mann_whitney_test()(input$data, input$context),
    "observed outcome in both groups",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("mann_whitney_test() performs declared randomization inference", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- mann_whitney_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )
  control <- permutation_control(iterations = 99L, seed = 2034L)
  analysis <- mann_whitney_test(
    inference = "permutation", permutation = control
  )

  set.seed(77)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)
  expect_identical(.Random.seed, state_before)
  set.seed(2034)
  direct <- suppressMessages(TOSTER::hodges_lehmann(
    c(3, 5, 7, 8, 10), c(1, 2, 4, 6, 9),
    R = 99L, p_method = "plusone", keep_perm = FALSE
  ))

  expect_equal(result$tests$estimate, unname(direct$estimate), tolerance = 1e-12)
  expect_equal(result$tests$statistic, unname(direct$statistic), tolerance = 1e-12)
  expect_equal(result$tests$p_value, direct$p.value, tolerance = 1e-12)
  expect_identical(result$tests$test, "hodges_lehmann_permutation_test")
  expect_identical(result$tests$inference, "permutation")
  expect_true(is.na(result$tests$exact_requested))
  expect_true(is.na(result$tests$exact_used))
  expect_true(is.na(result$tests$continuity_correction))
  expect_identical(result$tests$permutation_sampling, "random")
  expect_identical(result$tests$permutation_p_method, "plusone")
  expect_identical(result$tests$permutation_iterations_requested, 99L)
  expect_identical(result$tests$permutation_iterations_performed, 99L)
  expect_identical(result$tests$permutation_seed, 2034L)
})

test_that("mann_whitney_test() supports directional permutation nulls", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- mann_whitney_input(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )
  result <- mann_whitney_test(
    hypothesis = "superiority", margin = 1, benefit = "lower",
    inference = "permutation",
    permutation = permutation_control(iterations = 99L, seed = 2035L)
  )(input$data, input$context)

  expect_identical(result$tests$raw_estimate, -5)
  expect_identical(result$tests$benefit_estimate, 5)
  expect_identical(result$tests$estimate, 5)
  expect_identical(result$tests$p_lower, result$tests$p_value)
})

test_that("mann_whitney_test() rejects unsupported permutation declarations", {
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  expect_error(
    mann_whitney_test(inference = "permutation"),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_test(permutation = permutation_control(seed = 1L)),
    class = "bq_error_invalid_analysis_function"
  )
  expect_error(
    mann_whitney_test(
      hypothesis = "equivalence", margin = 1,
      inference = "permutation", permutation = permutation_control(seed = 1L)
    ),
    "not valid for equivalence",
    class = "bq_error_invalid_analysis_function"
  )

  input <- mann_whitney_input(
    outcome = 1:6, group = rep(c("a", "b"), each = 3L),
    reference_value = "b"
  )
  analysis <- mann_whitney_test(
    inference = "permutation",
    permutation = permutation_control(iterations = 20L, seed = 1L)
  )
  expect_error(
    analysis(input$data, input$context),
    "exact enumeration is not part",
    class = "bq_error_analysis_runtime"
  )
})

test_that("mann_whitney_test() bootstraps the Hodges-Lehmann shift", {
  skip_if_not_installed("boot")
  input <- mann_whitney_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )
  control <- bootstrap_control(
    method = "ordinary", iterations = 299L,
    conf_type = "percentile", seed = 2036L
  )
  analysis <- mann_whitney_test(bootstrap = control, conf_level = 0.9)

  set.seed(76)
  state_before <- .Random.seed
  result <- analysis(input$data, input$context)

  expect_identical(.Random.seed, state_before)
  expect_true(is.finite(result$tests$std_error))
  expect_true(result$tests$conf_low <= result$tests$conf_high)
  expect_identical(result$tests$interval_conf_level, 0.9)
  expect_identical(result$tests$ci_method, "bootstrap_percentile")
  expect_identical(result$tests$bootstrap_method, "ordinary")
  expect_identical(result$tests$bootstrap_engine, "boot")
  expect_identical(result$tests$bootstrap_iterations_requested, 299L)
  expect_identical(result$tests$bootstrap_iterations_valid, 299L)
  expect_identical(result$tests$bootstrap_seed, 2036L)
  expect_identical(
    analysis(input$data, input$context)$tests,
    result$tests
  )
})

test_that("mann_whitney_test() bootstraps on the directional benefit scale", {
  skip_if_not_installed("boot")
  input <- mann_whitney_input(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )
  result <- mann_whitney_test(
    hypothesis = "superiority", margin = 1, benefit = "lower",
    bootstrap = bootstrap_control(
      iterations = 299L, conf_type = "percentile", seed = 2037L
    )
  )(input$data, input$context)

  expect_identical(result$tests$raw_estimate, -5)
  expect_identical(result$tests$benefit_estimate, 5)
  expect_true(result$tests$conf_low > 0)
})

test_that("mann_whitney_test() rejects fractional bootstrap", {
  expect_error(
    mann_whitney_test(
      bootstrap = bootstrap_control(method = "fractional")
    ),
    "fractional bootstrap is not supported",
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("mann_whitney_test() isolates each resampling stage by its own seed", {
  skip_if_not_installed("boot")
  skip_if_not_installed("TOSTER", minimum_version = "0.9.0")
  input <- mann_whitney_input(
    outcome = c(3, 5, 7, 8, 10, 1, 2, 4, 6, 9),
    group = rep(c("new", "reference"), each = 5L),
    reference_value = "reference"
  )
  seed_combinations <- list(
    c(permutation = 2038L, bootstrap = 2039L),
    c(permutation = 2038L, bootstrap = NA_integer_),
    c(permutation = NA_integer_, bootstrap = 2039L),
    c(permutation = NA_integer_, bootstrap = NA_integer_)
  )
  preserves_stream <- c(TRUE, FALSE, FALSE, FALSE)

  for (index in seq_along(seed_combinations)) {
    seeds <- seed_combinations[[index]]
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
    analysis <- mann_whitney_test(
      inference = "permutation",
      permutation = permutation_control(
        iterations = 19L, seed = permutation_seed
      ),
      bootstrap = bootstrap_control(
        iterations = 49L, conf_type = "percentile", seed = bootstrap_seed
      )
    )

    set.seed(85)
    state_before <- .Random.seed
    analysis(input$data, input$context)

    expect_identical(
      identical(.Random.seed, state_before),
      preserves_stream[[index]]
    )
  }
})
