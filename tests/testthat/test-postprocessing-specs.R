test_that("post-processing pipelines are explicit and cumulative", {
  pipeline <- model_postprocessing(
    pairwise_post_hoc(
      method = tukey_test(),
      comparisons = all_pairwise(),
      multiplicity = built_in_multiplicity()
    )
  ) |>
    add_postprocessing(
      confidence_intervals(level = 0.90, method = "profile")
    )

  expect_s3_class(pipeline, "model_postprocessing_spec")
  expect_length(pipeline$steps, 2L)
  expect_identical(pipeline$steps[[1]]$method$id, "tukey")
  expect_identical(pipeline$steps[[2]]$level, 0.90)
})

test_that("post-processing can be attached to any method specification", {
  pipeline <- model_postprocessing(
    confidence_intervals(level = 0.95, method = "wald")
  )
  method <- with_postprocessing(
    logistic_model(ci_method = "wald", exponentiate = TRUE), pipeline
  )

  expect_s3_class(method$postprocessing, "model_postprocessing_spec")
  expect_identical(method$postprocessing$steps[[1]]$method, "wald")
})

test_that("custom post-processing uses a validated function contract", {
  step <- postprocessing_function(
    id = "my_model_summary",
    run = function(context) tibble::tibble(metric = "aic", value = 1),
    output = "diagnostics"
  )
  expect_s3_class(step, "model_postprocessing_step")
  expect_true(nzchar(step$function_hash))
  expect_error(
    postprocessing_function("bad", run = 1, output = "tests"),
    class = "bq_error_invalid_postprocessing"
  )
})

test_that("omnibus methods require an explicit post-hoc policy", {
  expect_error(one_way_anova(), class = "bq_error_invalid_postprocessing")
  expect_error(welch_anova(), class = "bq_error_invalid_postprocessing")
  expect_error(kruskal_wallis(), class = "bq_error_invalid_postprocessing")

  expect_identical(
    one_way_anova(post_hoc = no_post_hoc())$post_hoc$type,
    "none"
  )
})

test_that("pairwise group tests can be planned without an omnibus test", {
  method <- pairwise_group_comparisons(
    pairwise = pairwise_post_hoc(
      method = tukey_test(), comparisons = all_pairwise(),
      multiplicity = built_in_multiplicity()
    ),
    omnibus = no_omnibus()
  )

  expect_identical(method$omnibus_method, "none")
  expect_identical(method$posthoc_method, "tukey")
  expect_s3_class(method$omnibus, "omnibus_spec")
})

test_that("omnibus policy is always explicit for standalone pairwise tests", {
  pairwise <- pairwise_post_hoc(
    method = dunn_test(), comparisons = all_pairwise(),
    multiplicity = p_adjustment("holm")
  )
  expect_error(
    pairwise_group_comparisons(pairwise = pairwise),
    class = "bq_error_invalid_postprocessing"
  )
})

test_that("post-hoc specifications require comparisons and multiplicity", {
  expect_error(
    pairwise_post_hoc(method = tukey_test()),
    class = "bq_error_invalid_postprocessing"
  )
  expect_error(
    pairwise_post_hoc(
      method = tukey_test(), comparisons = all_pairwise()
    ),
    class = "bq_error_invalid_postprocessing"
  )

  spec <- pairwise_post_hoc(
    method = dunn_test(), comparisons = against_reference("A"),
    multiplicity = p_adjustment("holm")
  )
  expect_identical(spec$multiplicity$method, "holm")
})

test_that("hypotheses explicitly preserve margins and direction", {
  superiority_spec <- superiority(null = 0, direction = "greater", alpha = 0.025)
  noninferiority_spec <- non_inferiority(
    margin = -0.1, direction = "greater", alpha = 0.025
  )
  equivalence_spec <- equivalence(lower = -0.1, upper = 0.1, alpha = 0.05)

  expect_identical(superiority_spec$type, "superiority")
  expect_identical(noninferiority_spec$margin, -0.1)
  expect_identical(equivalence_spec$bounds, c(-0.1, 0.1))
})

test_that("two-group comparisons require every inferential decision", {
  expect_error(t_test(), class = "bq_error_invalid_group_comparison")
  expect_error(
    two_group_comparison(method = t_test(var_equal = FALSE)),
    class = "bq_error_invalid_group_comparison"
  )

  spec <- two_group_comparison(
    method = t_test(var_equal = FALSE),
    estimand = mean_difference(),
    contrast = against_reference("Control"),
    hypothesis = two_sided(null = 0, alpha = 0.05),
    multiplicity = no_adjustment()
  )
  expect_identical(spec$id, "two_group_t_test")
  expect_false(spec$method$var_equal)
  expect_identical(spec$hypothesis$type, "two_sided")
  expect_identical(spec$omnibus_method, "none")
})

test_that("Brunner-Munzel backend and estimand are explicit", {
  expect_error(brunner_munzel(), class = "bq_error_invalid_group_comparison")
  expect_error(
    brunnermunzel_backend(), class = "bq_error_invalid_group_comparison"
  )

  method <- brunner_munzel(
    backend = brunnermunzel_backend(permutation = FALSE)
  )
  probability <- two_group_comparison(
    method = method,
    estimand = probability_of_superiority(),
    contrast = against_reference("Control"),
    hypothesis = superiority(null = 0.5, direction = "greater", alpha = 0.025),
    multiplicity = no_adjustment()
  )
  centered <- two_group_comparison(
    method = method,
    estimand = relative_effect(),
    contrast = against_reference("Control"),
    hypothesis = two_sided(null = 0, alpha = 0.05),
    multiplicity = no_adjustment()
  )

  expect_identical(probability$estimand$scale, "probability")
  expect_identical(centered$estimand$scale, "minus_one_to_one")
  expect_identical(probability$method$backend$id, "brunnermunzel")
})

test_that("categorical omnibus test and effect size are independent", {
  spec <- categorical_group_analysis(
    omnibus = fisher_exact_test(simulate_p_value = FALSE),
    effect_size = cramers_v(ci_method = "noncentral_chi_squared"),
    pairwise = no_post_hoc()
  )

  expect_identical(spec$omnibus_method, "fisher_exact")
  expect_identical(spec$effect_size$id, "cramers_v")
  expect_identical(spec$effect_size$statistic_source, "pearson_chi_squared")
})
