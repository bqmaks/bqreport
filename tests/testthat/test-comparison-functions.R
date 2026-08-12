test_that("custom comparison function runs after model fit", {
  pairwise <- comparison_function(
    id = "custom_pairwise",
    effect_measure = "mean_difference",
    scale = "identity",
    compute = function(model, context) {
      coefficients <- stats::coef(model)
      tibble::tibble(
        analysis_id = context$analysis_id,
        outcome = context$outcome_spec$name,
        predictor = context$predictor_spec$name,
        contrast_id = context$comparison_spec$contrast_id,
        contrast = "A - Placebo",
        numerator = "A",
        denominator = "Placebo",
        estimate = unname(coefficients[[2]]),
        conf_low = unname(coefficients[[2]] - 1),
        conf_high = unname(coefficients[[2]] + 1),
        p_value = 0.02,
        p_adjusted = NA_real_,
        adjust_method = "none",
        effect_measure = "mean_difference",
        scale = "identity"
      )
    }
  )
  x <- as_bq_data(tibble::tibble(
    y = c(1, 2, 4, 5), treatment = c("Placebo", "Placebo", "A", "A")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "Placebo") |>
    set_coding(treatment, reference = "Placebo") |>
    set_comparisons(treatment, pairwise, adjust = "holm")

  result <- run_analysis(validate_plan(plan_analysis(x), x), x)
  comparison <- contrasts(result)

  expect_identical(comparison$contrast, "A - Placebo")
  expect_identical(comparison$adjust_method, "holm")
  expect_identical(comparison$p_adjusted, 0.02)
  expect_identical(contrasts(x)$function_id, "custom_pairwise")
  expect_true(nzchar(contrasts(x)$function_hash))
})

test_that("custom comparisons are validated independently", {
  bad <- comparison_function(
    id = "bad_comparison", effect_measure = "mean_difference",
    scale = "identity",
    compute = function(model, context) tibble::tibble(estimate = 1)
  )
  x <- as_bq_data(tibble::tibble(
    y = c(1, 2, 4, 5), treatment = c("Placebo", "Placebo", "A", "A")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "Placebo") |>
    set_comparisons(treatment, bad)

  result <- run_analysis(validate_plan(plan_analysis(x), x), x)

  expect_equal(nrow(contrasts(result)), 0L)
  expect_match(issues(result)$condition_class, "bq_error_invalid_comparison_output")
  expect_length(models(result), 1L)
  expect_gt(nrow(estimates(result)), 0L)
})

test_that("comparison exponentiation is applied once by normalization", {
  log_comparison <- comparison_function(
    id = "log_pairwise", effect_measure = "odds_ratio",
    model_scale = "link", scale = "ratio", exponentiate = TRUE,
    compute = function(model, context) tibble::tibble(
      analysis_id = context$analysis_id,
      outcome = context$outcome_spec$name,
      predictor = context$predictor_spec$name,
      contrast_id = context$comparison_spec$contrast_id,
      contrast = "A - Placebo", numerator = "A", denominator = "Placebo",
      estimate = log(2), conf_low = log(1.2), conf_high = log(3.3),
      p_value = 0.01, p_adjusted = NA_real_, adjust_method = "none",
      effect_measure = "odds_ratio", scale = "link"
    )
  )
  x <- as_bq_data(tibble::tibble(
    y = c(0, 0, 1, 1, 0, 1),
    treatment = c("Placebo", "Placebo", "A", "A", "Placebo", "A")
  )) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(treatment, type = "binary", reference = "Placebo") |>
    set_comparisons(treatment, log_comparison)

  comparison <- contrasts(run_analysis(validate_plan(plan_analysis(x), x), x))

  expect_equal(comparison$estimate, 2)
  expect_equal(comparison$conf_low, 1.2)
  expect_identical(comparison$scale, "ratio")
})
