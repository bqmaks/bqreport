test_that("explicit one-way and Welch ANOVA preserve method identity", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2, 3, 4, 5, 7, 8, 9, 12),
    arm = factor(rep(c("A", "B", "C"), each = 3L))
  )) |>
    set_outcome(value, type = "continuous") |>
    set_role(arm, "group") |>
    set_predictor(arm, type = "nominal", reference = "A")
  run <- function(method) data |>
    plan_descriptives(
      value, groups = arm, comparisons = method,
      contrasts = all_pairwise()
    ) |>
    validate_plan(data) |>
    run_analysis(data)

  ordinary <- run(one_way_anova(post_hoc = pairwise_post_hoc(
    method = tukey_test(), comparisons = all_pairwise(),
    multiplicity = built_in_multiplicity()
  )))
  welch <- run(welch_anova(post_hoc = pairwise_post_hoc(
    method = games_howell_test(), comparisons = all_pairwise(),
    multiplicity = built_in_multiplicity()
  )))
  direct_ordinary <- summary(stats::aov(value ~ arm, data = data))[[1]]
  direct_welch <- stats::oneway.test(value ~ arm, data = data, var.equal = FALSE)

  ordinary_test <- tests(ordinary)
  ordinary_test <- ordinary_test[ordinary_test$test == "one_way_anova", ]
  welch_test <- tests(welch)
  welch_test <- welch_test[welch_test$test == "welch_anova", ]
  expect_equal(ordinary_test$statistic, direct_ordinary$`F value`[[1]])
  expect_equal(ordinary_test$p_value, direct_ordinary$`Pr(>F)`[[1]])
  expect_equal(welch_test$statistic, unname(direct_welch$statistic))
  expect_equal(welch_test$p_value, direct_welch$p.value)
  expect_identical(nrow(contrasts(ordinary)), 3L)
  expect_identical(nrow(contrasts(welch)), 3L)
  expect_true(all(contrasts(ordinary)$adjust_method == "tukey"))
  expect_true(all(contrasts(welch)$adjust_method == "games_howell"))
  expect_s3_class(models(ordinary)[[ordinary$plan$analysis_id]],
    "group_analysis_artifact")
  expect_identical(
    models(ordinary)[[ordinary$plan$analysis_id]]$fit_count, 1L
  )
})

test_that("Kruskal-Wallis with Dunn post-hoc agrees with backends", {
  skip_if_not_installed("PMCMRplus")
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2, 2, 4, 5, 7, 8, 9, 12),
    arm = factor(rep(c("A", "B", "C"), each = 3L))
  )) |>
    set_outcome(value, type = "continuous") |>
    set_role(arm, "group") |>
    set_predictor(arm, type = "nominal", reference = "A")

  result <- data |>
    plan_descriptives(
      value, groups = arm,
      comparisons = kruskal_wallis(post_hoc = pairwise_post_hoc(
        method = dunn_test(), comparisons = all_pairwise(),
        multiplicity = p_adjustment("holm")
      )),
      contrasts = all_pairwise(), adjust = "holm"
    ) |>
    validate_plan(data) |>
    run_analysis(data)
  direct <- stats::kruskal.test(value ~ arm, data = data)
  test <- tests(result)
  test <- test[test$test == "kruskal_wallis", ]

  expect_equal(test$statistic, unname(direct$statistic))
  expect_equal(test$p_value, direct$p.value)
  expect_identical(nrow(contrasts(result)), 3L)
  expect_true(all(contrasts(result)$adjust_method == "holm"))
  expect_true(all(is.finite(contrasts(result)$p_adjusted)))
})

test_that("Brunner-Munzel returns probability-of-superiority estimand", {
  skip_if_not_installed("brunnermunzel")
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2, 4, 6, 2, 3, 5, 7),
    arm = factor(rep(c("A", "B"), each = 4L))
  )) |>
    set_outcome(value, type = "continuous") |>
    set_role(arm, "group") |>
    set_predictor(arm, type = "binary", reference = "A")
  result <- data |>
    plan_descriptives(
      value, groups = arm,
      comparisons = two_group_comparison(
        method = brunner_munzel(
          backend = brunnermunzel_backend(permutation = FALSE)
        ),
        estimand = probability_of_superiority(),
        contrast = against_reference("A"),
        hypothesis = two_sided(null = 0.5, alpha = 0.05),
        multiplicity = no_adjustment()
      )
    ) |>
    validate_plan(data) |>
    run_analysis(data)
  direct <- brunnermunzel::brunnermunzel.test(
    data$value[data$arm == "A"], data$value[data$arm == "B"],
    est = "original"
  )
  comparison <- contrasts(result)

  expect_equal(comparison$estimate, unname(direct$estimate))
  expect_equal(comparison$conf_low, direct$conf.int[[1]])
  expect_equal(comparison$p_value, direct$p.value)
  expect_identical(comparison$effect_measure, "probability_of_superiority")
  expect_identical(tests(result)$test, "brunner_munzel")
})

test_that("Brunner-Munzel supports centered effect and equivalence", {
  skip_if_not_installed("brunnermunzel")
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2, 4, 6, 2, 3, 5, 7),
    arm = factor(rep(c("A", "B"), each = 4L))
  )) |>
    set_outcome(value, type = "continuous") |>
    set_predictor(arm, type = "binary", reference = "A")
  run <- function(estimand, hypothesis) data |>
    plan_descriptives(
      value, groups = arm,
      comparisons = two_group_comparison(
        method = brunner_munzel(
          backend = brunnermunzel_backend(permutation = FALSE)
        ),
        estimand = estimand, contrast = against_reference("A"),
        hypothesis = hypothesis, multiplicity = no_adjustment()
      )
    ) |>
    validate_plan(data) |>
    run_analysis(data)

  probability <- run(
    probability_of_superiority(), two_sided(null = 0.5, alpha = 0.05)
  )
  centered <- run(
    relative_effect(), equivalence(lower = -1, upper = 1, alpha = 0.05)
  )

  expect_equal(
    contrasts(centered)$estimate,
    2 * contrasts(probability)$estimate - 1
  )
  expect_identical(contrasts(centered)$scale, "minus_one_to_one")
  expect_true(is.finite(contrasts(centered)$p_value))
})

test_that("lawstat is an explicit alternative Brunner-Munzel backend", {
  skip_if_not_installed("lawstat")
  method <- brunner_munzel(backend = lawstat_backend())
  expect_identical(method$backend$id, "lawstat")
  expect_identical(method$required_packages, "lawstat")
})

test_that("pairwise comparisons do not manufacture an omnibus test", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2, 3, 4, 5, 7, 8, 9, 12),
    arm = factor(rep(c("A", "B", "C"), each = 3L))
  )) |>
    set_outcome(value, type = "continuous") |>
    set_role(arm, "group") |>
    set_predictor(arm, type = "nominal", reference = "A")

  method <- pairwise_group_comparisons(
    pairwise = pairwise_post_hoc(
      method = tukey_test(), comparisons = all_pairwise(),
      multiplicity = built_in_multiplicity()
    ),
    omnibus = no_omnibus()
  )
  result <- data |>
    plan_descriptives(value, groups = arm, comparisons = method) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_identical(nrow(tests(result)), 0L)
  expect_identical(nrow(omnibus_effects(result)), 0L)
  expect_identical(nrow(contrasts(result)), 3L)
})

test_that("omnibus outputs share one fitted analysis artifact", {
  values <- c(1, 2, 3, 4, 5, 7, 8, 9, 12)
  groups <- factor(rep(c("A", "B", "C"), each = 3L))
  method <- one_way_anova(post_hoc = pairwise_post_hoc(
    method = tukey_test(), comparisons = all_pairwise(),
    multiplicity = built_in_multiplicity()
  ))

  artifact <- bqreport:::fit_group_analysis_artifact(values, groups, method)

  expect_s3_class(artifact, "group_analysis_artifact")
  expect_s3_class(artifact$model, "aov")
  expect_identical(artifact$fit_count, 1L)
  expect_identical(artifact$omnibus_test, artifact$model_summary)
  expect_true(is.matrix(artifact$post_hoc))
})

test_that("Kruskal-Wallis statistic and effect reuse the same rank artifact", {
  values <- c(1, 2, 2, 4, 5, 7, 8, 9, 12)
  groups <- factor(rep(c("A", "B", "C"), each = 3L))
  method <- kruskal_wallis(post_hoc = no_post_hoc())

  artifact <- bqreport:::fit_group_analysis_artifact(values, groups, method)

  expect_identical(artifact$fit_count, 1L)
  expect_equal(artifact$ranks, rank(values, ties.method = "average"))
  expect_identical(artifact$omnibus_test, artifact$kruskal_test)
})
