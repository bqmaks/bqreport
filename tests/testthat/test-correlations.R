test_that("correlation method constructors declare estimands and CI", {
  expect_identical(pearson_correlation()$id, "pearson")
  expect_identical(pearson_correlation()$ci_method, "fisher_z")
  expect_identical(spearman_correlation()$ci_method, "fisher_z_bonett_wright")
  expect_identical(kendall_correlation()$ci_method, "normal_approximation")
})

test_that("Spearman confidence intervals use the Bonett-Wright standard error", {
  data <- as_bq_data(tibble::tibble(
    x = c(1, 4, 2, 8, 5, 9, 3, 7, 6, 10),
    y = c(2, 1, 5, 4, 8, 7, 3, 9, 6, 10)
  ))
  output <- data |>
    plan_correlations(x, with = y, method = spearman_correlation()) |>
    validate_plan(data) |>
    run_analysis(data) |>
    correlations()
  estimate <- stats::cor(data$x, data$y, method = "spearman")
  std_error <- sqrt((1 + estimate^2 / 2) / (nrow(data) - 3))
  critical <- stats::qnorm(0.975)

  expect_equal(output$estimate, estimate)
  expect_equal(output$std_error, std_error)
  expect_equal(output$conf_low, tanh(atanh(estimate) - critical * std_error))
  expect_equal(output$conf_high, tanh(atanh(estimate) + critical * std_error))
  expect_identical(output$std_error_scale, "fisher_z_bonett_wright")
})

test_that("plan_correlations compiles unique inspectable pairs", {
  data <- as_bq_data(tibble::tibble(a = 1:5, b = 2:6, c = 5:1))
  plan <- plan_correlations(data, c(a, b, c))

  expect_s3_class(plan, "analysis_plan")
  expect_equal(nrow(plan), 3L)
  expect_setequal(
    paste(plan$variable_x, plan$variable_y), c("a b", "a c", "b c")
  )
  expect_true(all(plan$analysis_type == "correlation"))
  expect_true(all(plan$method == "pearson"))
})

test_that("rectangular correlation selection removes self and duplicate pairs", {
  data <- as_bq_data(tibble::tibble(a = 1:5, b = 2:6, c = 5:1))
  plan <- plan_correlations(data, c(a, b), with = c(b, c))

  expect_setequal(
    paste(plan$variable_x, plan$variable_y), c("a b", "a c", "b c")
  )
})

test_that("Pearson correlations agree with cor.test and expose Fisher scale SE", {
  data <- as_bq_data(tibble::tibble(
    x = c(1, 2, 3, 4, 5, 6), y = c(1, 3, 2, 5, 4, 7)
  ))
  direct <- stats::cor.test(data$x, data$y, method = "pearson", conf.level = 0.9)
  result <- data |>
    plan_correlations(x, with = y, confidence_level = 0.9) |>
    validate_plan(data) |>
    run_analysis(data)
  output <- correlations(result)

  expect_equal(output$estimate, unname(direct$estimate))
  expect_equal(output$conf_low, direct$conf.int[[1]])
  expect_equal(output$conf_high, direct$conf.int[[2]])
  expect_equal(output$p_value, direct$p.value)
  expect_equal(output$std_error, 1 / sqrt(output$n - 3))
  expect_identical(output$std_error_scale, "fisher_z")
})

test_that("correlation transformations are applied before estimation", {
  raw <- tibble::tibble(x = c(1, 2, 4, 8, 16), y = c(1, 2, 3, 4, 5))
  data <- as_bq_data(raw) |>
    set_transformation(x, log2_transform())
  result <- data |>
    plan_correlations(x, with = y) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_equal(correlations(result)$estimate, stats::cor(log2(raw$x), raw$y))
  expect_identical(correlations(result)$transformation_x, "log2")
})

test_that("pairwise and complete missing policies have explicit sample sizes", {
  data <- as_bq_data(tibble::tibble(
    a = 1:6, b = c(1, 2, 3, 4, NA, 6), c = c(1, 2, 3, 4, 5, NA)
  ))
  pairwise <- data |> plan_correlations(c(a, b, c), missing = "pairwise") |>
    validate_plan(data) |> run_analysis(data) |> correlations()
  complete <- data |> plan_correlations(c(a, b, c), missing = "complete") |>
    validate_plan(data) |> run_analysis(data) |> correlations()

  expect_setequal(pairwise$n, c(4L, 5L))
  expect_true(all(complete$n == 4L))
  expect_true(all(complete$missing_policy == "complete"))
})

test_that("correlation p-values are adjusted over the declared family", {
  data <- as_bq_data(tibble::tibble(a = 1:8, b = 2:9, c = c(8:2, 4)))
  result <- data |>
    plan_correlations(c(a, b, c), adjust = "holm") |>
    validate_plan(data) |>
    run_analysis(data)
  output <- correlations(result)

  expect_equal(output$p_adjusted, stats::p.adjust(output$p_value, "holm"))
  expect_true(all(output$adjust_method == "holm"))
})

test_that("correlation preflight rejects insufficient and constant pairs", {
  data <- as_bq_data(tibble::tibble(a = c(1, NA, NA), b = c(2, 2, 2)))
  plan <- validate_plan(plan_correlations(data, a, with = b), data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "complete|variation", ignore.case = TRUE)
})

test_that("partial Pearson correlations agree with residualized variables", {
  set.seed(91)
  raw <- tibble::tibble(
    age = rnorm(80), x = rnorm(80), y = rnorm(80)
  )
  raw$x <- raw$x + 0.8 * raw$age
  raw$y <- raw$y + 0.6 * raw$age + 0.4 * raw$x
  data <- as_bq_data(raw)
  result <- data |>
    plan_correlations(x, with = y, adjust_for = age) |>
    validate_plan(data) |>
    run_analysis(data)
  output <- correlations(result)
  residual_x <- stats::residuals(stats::lm(x ~ age, data = raw))
  residual_y <- stats::residuals(stats::lm(y ~ age, data = raw))
  expected <- stats::cor(residual_x, residual_y)

  expect_equal(output$estimate, expected)
  expect_identical(output$estimand, "partial_correlation")
  expect_identical(output$adjustment_variables[[1]], "age")
  expect_identical(output$n_adjustment, 1L)
  expect_equal(output$df, output$n - 3)
  expect_equal(output$std_error, 1 / sqrt(output$n - 4))
})

test_that("partial Spearman correlation residualizes ranks", {
  raw <- tibble::tibble(
    x = c(1, 4, 2, 8, 5, 9), y = c(2, 1, 5, 4, 8, 7), z = 1:6
  )
  data <- as_bq_data(raw)
  result <- data |>
    plan_correlations(
      x, with = y, adjust_for = z, method = spearman_correlation()
    ) |>
    validate_plan(data) |>
    run_analysis(data)
  rx <- stats::residuals(stats::lm(rank(x) ~ rank(z), data = raw))
  ry <- stats::residuals(stats::lm(rank(y) ~ rank(z), data = raw))

  expect_equal(correlations(result)$estimate, stats::cor(rx, ry))
})

test_that("partial correlation preflight validates method and residual df", {
  data <- as_bq_data(tibble::tibble(x = 1:5, y = 2:6, z1 = 1:5, z2 = 5:1))
  expect_error(
    plan_correlations(
      data, x, with = y, adjust_for = z1, method = kendall_correlation()
    ),
    class = "bq_error_invalid_correlation"
  )
  plan <- data |>
    plan_correlations(x, with = y, adjust_for = c(z1, z2)) |>
    validate_plan(data)
  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "degrees of freedom|complete", ignore.case = TRUE)
})

test_that("correlations compile and execute independently by strata", {
  data <- as_bq_data(tibble::tibble(
    x = rep(1:6, 2),
    y = c(1:6, 6:1),
    arm = rep(c("A", "B"), each = 6)
  ))
  plan <- plan_correlations(data, x, with = y, strata = arm, adjust = "holm")

  expect_equal(nrow(plan), 2L)
  expect_setequal(plan$stratum_label, c("arm=A", "arm=B"))
  expect_length(unique(plan$correlation_family_id), 2L)

  result <- plan |> validate_plan(data) |> run_analysis(data)
  output <- correlations(result)
  expect_setequal(output$stratum_label, c("arm=A", "arm=B"))
  expect_equal(output$estimate[output$stratum_label == "arm=A"], 1)
  expect_equal(output$estimate[output$stratum_label == "arm=B"], -1)
  expect_true(all(output$n == 6L))
  expect_true(all(output$p_adjusted == output$p_value))
})

test_that("correlation strata use stable ids after rename", {
  data <- as_bq_data(tibble::tibble(
    x = 1:8, y = 2:9, site = rep(c("A", "B"), each = 4)
  ))
  plan <- plan_correlations(data, x, with = y, strata = site)
  renamed <- dplyr::rename(data, centre = site)
  validated <- validate_plan(plan, renamed)

  expect_true(all(validated$status == "ready"))
  expect_true(all(validated$strata[[1]] == "centre"))
})

test_that("correlation strata with insufficient observations fail locally", {
  data <- as_bq_data(tibble::tibble(
    x = 1:7, y = 2:8, arm = c(rep("A", 4), rep("B", 3))
  ))
  plan <- plan_correlations(data, x, with = y, strata = arm) |>
    validate_plan(data)

  expect_identical(plan$status[plan$stratum_label == "arm=A"], "ready")
  expect_identical(plan$status[plan$stratum_label == "arm=B"], "invalid")
  expect_match(
    plan$reason[plan$stratum_label == "arm=B"], "complete", ignore.case = TRUE
  )
})

test_that("Pearson correlations expose an omnibus interaction test across strata", {
  data <- as_bq_data(tibble::tibble(
    x = rep(1:8, 3),
    y = c(1, 3, 2, 5, 4, 7, 6, 8, 8, 6, 7, 4, 5, 2, 3, 1,
      1, 2, 4, 3, 6, 5, 8, 7),
    arm = rep(c("A", "B", "C"), each = 8)
  ))
  result <- data |>
    plan_correlations(x, with = y, strata = arm, interaction_test = TRUE) |>
    validate_plan(data) |>
    run_analysis(data)

  observed <- correlations(result)
  z <- atanh(observed$estimate)
  variance <- 1 / (observed$n - 3)
  weighted_mean <- sum(z / variance) / sum(1 / variance)
  expected_q <- sum((z - weighted_mean)^2 / variance)
  interaction <- tests(result)[tests(result)$test == "correlation_interaction", ]

  expect_equal(nrow(interaction), 1L)
  expect_equal(interaction$statistic, expected_q)
  expect_equal(interaction$df, 2)
  expect_equal(interaction$p_value, stats::pchisq(expected_q, 2, lower.tail = FALSE))
})

test_that("correlation interaction returns pairwise Fisher z contrasts", {
  data <- as_bq_data(tibble::tibble(
    x = rep(1:8, 2),
    y = c(1, 3, 2, 5, 4, 7, 6, 8, 8, 6, 7, 4, 5, 2, 3, 1),
    arm = rep(c("A", "B"), each = 8)
  ))
  result <- data |>
    plan_correlations(
      x, with = y, strata = arm, interaction_test = TRUE,
      confidence_level = 0.9, adjust = "holm"
    ) |>
    validate_plan(data) |>
    run_analysis(data)
  output <- correlations(result)
  contrast <- contrasts(result)
  expected <- diff(atanh(output$estimate))
  expected_se <- sqrt(sum(1 / (output$n - 3)))

  expect_equal(nrow(contrast), 1L)
  expect_equal(abs(contrast$estimate), abs(expected))
  expect_equal(contrast$std_error, expected_se)
  expect_identical(contrast$std_error_scale, "fisher_z")
  expect_identical(contrast$effect_measure, "difference_in_fisher_z")
  expect_true(contrast$conf_low < contrast$estimate)
  expect_true(contrast$conf_high > contrast$estimate)
  expect_equal(contrast$p_adjusted, contrast$p_value)
})

test_that("correlation interaction contract requires strata and Pearson method", {
  data <- as_bq_data(tibble::tibble(x = 1:8, y = 2:9, arm = rep(c("A", "B"), 4)))

  expect_error(
    plan_correlations(data, x, with = y, interaction_test = TRUE),
    class = "bq_error_invalid_correlation"
  )
  expect_error(
    plan_correlations(
      data, x, with = y, strata = arm, interaction_test = TRUE,
      method = spearman_correlation()
    ),
    class = "bq_error_invalid_correlation"
  )
})

test_that("custom correlation methods use a validated public contract", {
  custom <- correlation_method(
    id = "scaled_pearson", effect_measure = "scaled_pearson_correlation",
    ci_method = "custom_normal", supports_partial = FALSE,
    supports_interaction = FALSE,
    compute = function(context) {
      estimate <- stats::cor(context$x, context$y) / 2
      correlation_output(
        estimate = estimate, std_error = 0.1,
        std_error_scale = "custom", conf_low = estimate - 0.2,
        conf_high = estimate + 0.2, statistic = estimate / 0.1,
        df = length(context$x) - 2, p_value = 0.25
      )
    }
  )
  data <- as_bq_data(tibble::tibble(x = 1:8, y = c(1, 3, 2, 5, 4, 7, 6, 8)))
  plan <- plan_correlations(data, x, with = y, method = custom)
  result <- plan |> validate_plan(data) |> run_analysis(data)

  expect_equal(correlations(result)$estimate, stats::cor(data$x, data$y) / 2)
  expect_identical(correlations(result)$method, "scaled_pearson")
  expect_identical(
    correlations(result)$effect_measure, "scaled_pearson_correlation"
  )
  expect_identical(result$provenance$function_id, "scaled_pearson")
  expect_false(is.na(result$provenance$function_hash))
})

test_that("correlation capabilities are enforced before execution", {
  method <- correlation_method(
    id = "simple", effect_measure = "simple_correlation", ci_method = "none",
    supports_partial = FALSE, supports_interaction = FALSE,
    compute = function(context) correlation_output(
      stats::cor(context$x, context$y), NA_real_, "none",
      NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
    )
  )
  data <- as_bq_data(tibble::tibble(
    x = 1:8, y = 2:9, z = 3:10, arm = rep(c("A", "B"), 4)
  ))

  expect_error(
    plan_correlations(data, x, with = y, adjust_for = z, method = method),
    class = "bq_error_invalid_correlation"
  )
  expect_error(
    plan_correlations(
      data, x, with = y, strata = arm, interaction_test = TRUE, method = method
    ),
    class = "bq_error_invalid_correlation"
  )
})

test_that("malformed custom correlation output is collected as an issue", {
  bad <- correlation_method(
    id = "bad", effect_measure = "bad_correlation", ci_method = "none",
    compute = function(context) tibble::tibble(estimate = 0)
  )
  data <- as_bq_data(tibble::tibble(x = 1:8, y = 2:9))
  result <- data |>
    plan_correlations(x, with = y, method = bad) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_equal(nrow(correlations(result)), 0L)
  expect_true(any(
    issues(result)$condition_class == "bq_error_invalid_correlation_output"
  ))
})

test_that("custom correlation comparators control stratified interaction output", {
  comparator <- correlation_comparator(
    id = "fixed_comparator", methods = "pearson",
    compare = function(context) correlation_comparison_output(
      statistic = 4, df = 1, p_value = 0.0455,
      contrasts = tibble::tibble(
        numerator = context$estimates$stratum_label[[1]],
        denominator = context$estimates$stratum_label[[2]],
        estimate = 0.5, std_error = 0.2, std_error_scale = "custom",
        conf_low = 0.1, conf_high = 0.9, p_value = 0.02,
        effect_measure = "custom_difference", scale = "custom"
      )
    )
  )
  data <- as_bq_data(tibble::tibble(
    x = rep(1:8, 2), y = c(1:8, 8:1), arm = rep(c("A", "B"), each = 8)
  ))
  result <- data |>
    plan_correlations(
      x, with = y, strata = arm, interaction_test = TRUE,
      comparator = comparator, adjust = "holm"
    ) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_equal(tests(result)$statistic, 4)
  expect_identical(tests(result)$method, "fixed_comparator")
  expect_equal(contrasts(result)$estimate, 0.5)
  expect_identical(contrasts(result)$effect_measure, "custom_difference")
  expect_equal(contrasts(result)$p_adjusted, 0.02)
  expect_true(all(result$provenance$correlation_comparator_id == "fixed_comparator"))
})

test_that("correlation comparator validates compatible method and output", {
  comparator <- correlation_comparator(
    id = "pearson_only", methods = "pearson",
    compare = function(context) tibble::tibble()
  )
  data <- as_bq_data(tibble::tibble(
    x = rep(1:8, 2), y = rep(2:9, 2), arm = rep(c("A", "B"), each = 8)
  ))
  expect_error(
    plan_correlations(
      data, x, with = y, strata = arm, interaction_test = TRUE,
      method = spearman_correlation(), comparator = comparator
    ),
    class = "bq_error_invalid_correlation"
  )
})

test_that("resampled correlations provide reproducible bootstrap CI and permutation p", {
  data <- as_bq_data(tibble::tibble(
    x = 1:20, y = c(2, 1, 4, 3, 7, 5, 8, 6, 10, 9, 13, 11, 14, 12, 16, 15,
      19, 17, 20, 18)
  ))
  method <- resampled_correlation(
    pearson_correlation(), bootstrap = 199, permutations = 199, seed = 42
  )
  run <- function() data |>
    plan_correlations(x, with = y, method = method) |>
    validate_plan(data) |>
    run_analysis(data) |>
    correlations()
  first <- run()
  second <- run()

  expect_equal(first$estimate, stats::cor(data$x, data$y))
  expect_equal(first$std_error, second$std_error)
  expect_equal(first$conf_low, second$conf_low)
  expect_equal(first$p_value, second$p_value)
  expect_identical(first$ci_method, "bootstrap_percentile")
  expect_identical(first$std_error_scale, "correlation")
  expect_identical(first$resampling_seed, 42L)
  expect_identical(first$bootstrap_successful, 199L)
  expect_identical(first$permutation_successful, 199L)
  expect_true(first$p_value >= 1 / 200)
})

test_that("resampling preserves global random state and records provenance", {
  data <- as_bq_data(tibble::tibble(x = 1:12, y = c(2:12, 1)))
  method <- resampled_correlation(
    spearman_correlation(), bootstrap = 49, permutations = 49, seed = 11
  )
  set.seed(712)
  before <- .Random.seed
  result <- data |>
    plan_correlations(x, with = y, method = method) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_identical(.Random.seed, before)
  expect_identical(result$provenance$resampling_seed, 11L)
  expect_identical(result$provenance$bootstrap_replicates, 49L)
  expect_identical(result$provenance$permutation_replicates, 49L)
})

test_that("resampled methods keep the minimum-n rule of their base method", {
  data <- as_bq_data(tibble::tibble(x = c(1, 2, 3), y = c(2, 1, 3)))
  plan <- data |>
    plan_correlations(x, with = y, method = resampled_correlation(
      pearson_correlation(), bootstrap = 99, permutations = 0, seed = 2
    )) |>
    validate_plan(data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "at least 4")
})

test_that("resampling parameters are validated", {
  expect_error(
    resampled_correlation(pearson_correlation(), bootstrap = 1, seed = 1),
    class = "bq_error_invalid_correlation"
  )
  expect_error(
    resampled_correlation(pearson_correlation(), bootstrap = 10),
    class = "bq_error_invalid_correlation"
  )
})

test_that("resampled weighted correlation resamples weights with observations", {
  set.seed(31)
  n <- 40
  x <- rnorm(n)
  heavy <- rep(c(TRUE, FALSE), each = n / 2)
  data <- as_bq_data(tibble::tibble(
    x = x,
    y = ifelse(heavy, x + rnorm(n, sd = 0.05), -x + rnorm(n, sd = 0.05)),
    w = ifelse(heavy, 100, 0.01)
  ))
  method <- resampled_correlation(
    weighted_pearson_correlation(), bootstrap = 199, permutations = 0, seed = 5
  )
  output <- data |>
    plan_correlations(x, with = y, weights = w, method = method) |>
    validate_plan(data) |>
    run_analysis(data) |>
    correlations()

  # The estimate is dominated by the heavily weighted, positively correlated
  # half. Bootstrap replicates that keep weights paired with their
  # observations must stay in its neighbourhood; misaligned weights would
  # produce intervals spanning zero.
  expect_true(output$estimate > 0.9)
  expect_true(output$conf_low > 0.5)
  expect_identical(output$bootstrap_successful, 199L)
})

test_that("resampled weighted correlation with unit weights matches unweighted", {
  data <- as_bq_data(tibble::tibble(
    x = c(1, 3, 2, 5, 4, 7, 6, 9, 8, 10, 12, 11),
    y = c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 11, 13),
    w = rep(1, 12)
  ))
  weighted <- data |>
    plan_correlations(x, with = y, weights = w, method = resampled_correlation(
      weighted_pearson_correlation(), bootstrap = 99, permutations = 0, seed = 3
    )) |>
    validate_plan(data) |>
    run_analysis(data) |>
    correlations()
  unweighted <- data |>
    plan_correlations(x, with = y, method = resampled_correlation(
      pearson_correlation(), bootstrap = 99, permutations = 0, seed = 3
    )) |>
    validate_plan(data) |>
    run_analysis(data) |>
    correlations()

  expect_equal(weighted$estimate, unweighted$estimate)
  expect_equal(weighted$conf_low, unweighted$conf_low)
  expect_equal(weighted$conf_high, unweighted$conf_high)
})

test_that("resampled repeated-measures correlation resamples whole subjects", {
  set.seed(8)
  subjects <- sprintf("s%02d", 1:10)
  data <- as_bq_data(tibble::tibble(
    id = rep(subjects, each = 6),
    x = rep(rnorm(10, sd = 5), each = 6) + rep(1:6, 10),
    y = rep(rnorm(10, sd = 5), each = 6) + rep(1:6, 10) * 2 +
      rnorm(60, sd = 0.3)
  ))
  method <- resampled_correlation(
    repeated_measures_correlation(), bootstrap = 199, permutations = 199,
    seed = 9
  )
  run <- function() data |>
    plan_correlations(x, with = y, id = id, method = method) |>
    validate_plan(data) |>
    run_analysis(data) |>
    correlations()
  first <- run()
  second <- run()

  expect_equal(first$conf_low, second$conf_low)
  expect_equal(first$p_value, second$p_value)
  expect_true(first$estimate > 0.9)
  expect_true(first$conf_low > 0.5)
  expect_true(first$conf_low <= first$estimate)
  expect_true(first$conf_high >= first$estimate)
  # A strong within-subject association must be detected by the
  # within-subject permutation test.
  expect_lt(first$p_value, 0.05)
  expect_identical(first$bootstrap_successful, 199L)
  expect_identical(first$permutation_successful, 199L)
})

test_that("resampling helpers preserve pairing and subject structure", {
  weighted_context <- structure(list(
    x = 1:8 + 0.5, y = 8:1 - 0.25,
    adjustment = matrix(numeric(), nrow = 8L, ncol = 0L),
    weights = c(1, 2, 3, 4, 5, 6, 7, 8), id = NULL,
    confidence_level = 0.95
  ), class = "correlation_context")
  set.seed(21)
  sampled <- resample_correlation_context(weighted_context)
  original_triples <- paste(
    weighted_context$x, weighted_context$y, weighted_context$weights
  )
  expect_true(all(
    paste(sampled$x, sampled$y, sampled$weights) %in% original_triples
  ))

  subject_context <- structure(list(
    x = as.numeric(1:9), y = as.numeric(9:1),
    adjustment = matrix(numeric(), nrow = 9L, ncol = 0L),
    weights = NULL, id = rep(c("a", "b", "c"), each = 3L),
    confidence_level = 0.95
  ), class = "correlation_context")
  set.seed(22)
  cluster <- resample_correlation_context(subject_context)
  original_blocks <- vapply(
    split(paste(subject_context$x, subject_context$y), subject_context$id),
    paste, character(1), collapse = ";"
  )
  sampled_blocks <- vapply(
    split(paste(cluster$x, cluster$y), cluster$id),
    paste, character(1), collapse = ";"
  )
  expect_true(all(sampled_blocks %in% original_blocks))
  expect_identical(length(unique(cluster$id)), 3L)

  set.seed(23)
  permuted <- permute_correlation_context(subject_context)
  expect_identical(permuted$x, subject_context$x)
  for (rows in split(seq_along(subject_context$y), subject_context$id)) {
    expect_identical(
      sort(permuted$y[rows]), sort(subject_context$y[rows])
    )
  }
})

test_that("weighted Pearson correlation agrees with direct weighted moments", {
  data <- as_bq_data(tibble::tibble(
    x = c(1, 2, 3, 5, 8, 13), y = c(2, 1, 4, 6, 7, 12),
    w = c(1, 2, 1, 3, 2, 1)
  ))
  result <- data |>
    plan_correlations(
      x, with = y, weights = w, method = weighted_pearson_correlation()
    ) |>
    validate_plan(data) |>
    run_analysis(data)
  output <- correlations(result)
  mx <- weighted.mean(data$x, data$w)
  my <- weighted.mean(data$y, data$w)
  expected <- sum(data$w * (data$x - mx) * (data$y - my)) /
    sqrt(sum(data$w * (data$x - mx)^2) * sum(data$w * (data$y - my)^2))
  effective_n <- sum(data$w)^2 / sum(data$w^2)

  expect_equal(output$estimate, expected)
  expect_equal(output$effective_n, effective_n)
  expect_identical(output$weight, "w")
  expect_identical(output$effect_measure, "weighted_pearson_correlation")
})

test_that("repeated-measures correlation removes subject means", {
  data <- as_bq_data(tibble::tibble(
    id = rep(letters[1:5], each = 4),
    x = rep(1:4, 5) + rep(c(0, 10, -5, 20, 7), each = 4),
    y = rep(c(1, 3, 2, 5), 5) + rep(c(5, -4, 12, 30, -8), each = 4)
  ))
  result <- data |>
    plan_correlations(
      x, with = y, id = id, method = repeated_measures_correlation()
    ) |>
    validate_plan(data) |>
    run_analysis(data)
  output <- correlations(result)
  centered_x <- data$x - ave(data$x, data$id)
  centered_y <- data$y - ave(data$y, data$id)

  expect_equal(output$estimate, stats::cor(centered_x, centered_y))
  expect_identical(output$id, "id")
  expect_identical(output$n_subjects, 5L)
  expect_identical(output$estimand, "within_subject_correlation")
})

test_that("weighted and repeated correlation requirements fail in preflight", {
  data <- as_bq_data(tibble::tibble(
    id = rep(c("a", "b"), each = 3), x = 1:6, y = 2:7,
    bad_weight = c(1, 1, -1, 1, 1, 1)
  ))
  expect_error(
    plan_correlations(data, x, with = y, method = weighted_pearson_correlation()),
    class = "bq_error_invalid_correlation"
  )
  weighted <- plan_correlations(
    data, x, with = y, weights = bad_weight,
    method = weighted_pearson_correlation()
  ) |>
    validate_plan(data)
  expect_identical(weighted$status, "invalid")
  expect_match(weighted$reason, "weight", ignore.case = TRUE)

  expect_error(
    plan_correlations(data, x, with = y, method = repeated_measures_correlation()),
    class = "bq_error_invalid_correlation"
  )
})

test_that("point-estimate-only methods require review before execution", {
  data <- as_bq_data(tibble::tibble(
    x = c(1:20, 100), y = c(1:20, -100)
  ))
  plan <- data |>
    plan_correlations(x, with = y, method = biweight_correlation()) |>
    validate_plan(data)

  expect_identical(plan$status, "review")
  expect_match(plan$reason, "point estimate", ignore.case = TRUE)

  skipped <- run_analysis(plan, data)
  expect_identical(nrow(correlations(skipped)), 0L)
  expect_identical(nrow(issues(skipped)), 1L)

  resampled_plan <- data |>
    plan_correlations(x, with = y, method = resampled_correlation(
      biweight_correlation(), bootstrap = 99, permutations = 0, seed = 4
    )) |>
    validate_plan(data)
  expect_identical(resampled_plan$status, "ready")
})

test_that("biweight correlation is robust to a bivariate outlier", {
  data <- as_bq_data(tibble::tibble(
    x = c(1:20, 100), y = c(1:20, -100)
  ))
  output <- data |>
    plan_correlations(x, with = y, method = biweight_correlation()) |>
    validate_plan(data) |>
    approve_plan() |>
    run_analysis(data) |>
    correlations()

  expect_true(output$estimate > 0.95)
  expect_true(output$estimate > stats::cor(data$x, data$y))
  expect_identical(output$effect_measure, "biweight_midcorrelation")
  expect_true(is.na(output$p_value))
})

test_that("polychoric methods declare categorical input and optional backend", {
  polychoric <- polychoric_correlation()
  tetrachoric <- tetrachoric_correlation()
  expect_identical(polychoric$input_type, "ordered_categorical")
  expect_identical(tetrachoric$input_type, "binary_categorical")
  expect_identical(polychoric$required_packages, "polycor")

  data <- as_bq_data(tibble::tibble(
    x = ordered(rep(1:3, 6)), y = ordered(rep(c(1, 2, 3, 2, 1, 3), 3))
  )) |>
    set_predictor(x, type = "ordinal") |>
    set_outcome(y, type = "ordinal")
  plan <- plan_correlations(data, x, with = y, method = polychoric) |>
    validate_plan(data)
  if (!requireNamespace("polycor", quietly = TRUE)) {
    expect_identical(plan$status, "invalid")
    expect_match(plan$reason, "polycor")
  }
})
