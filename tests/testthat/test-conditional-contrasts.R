test_that("within_levels computes treatment effects inside modifier levels", {
  x <- as_bq_data(tibble::tibble(
    y = c(1, 3, 2, 4, 5, 9, 6, 10),
    treatment = rep(c("Placebo", "Drug"), 4),
    sex = rep(c("F", "M"), each = 4)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "Placebo") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_coding(treatment, reference = "Placebo") |>
    set_comparisons(treatment, within_levels(sex), adjust = "holm")
  plan <- validate_plan(plan_analysis(
    x, predictors = treatment, effect_modifiers = sex
  ), x)

  result <- run_analysis(plan, x)
  conditional <- contrasts(result)
  fit <- models(result)[[plan$analysis_id]]
  beta <- stats::coef(fit)

  expect_identical(conditional$modifier, c("sex", "sex"))
  expect_identical(conditional$modifier_level, c("F", "M"))
  expect_equal(conditional$estimate[[1]], unname(beta[["treatmentDrug"]]))
  expect_equal(
    conditional$estimate[[2]],
    unname(beta[["treatmentDrug"]] + beta[["treatmentDrug:sexM"]])
  )
  expect_identical(conditional$numerator, c("Drug", "Drug"))
  expect_identical(conditional$denominator, c("Placebo", "Placebo"))
  expect_identical(conditional$adjust_method, c("holm", "holm"))
})

test_that("conditional logistic effects are returned as odds ratios", {
  x <- as_bq_data(tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1),
    treatment = rep(c("P", "D"), 6),
    sex = rep(c("F", "M"), each = 6)
  )) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_comparisons(treatment, within_levels(sex))
  plan <- validate_plan(plan_analysis(
    x, predictors = treatment, effect_modifiers = sex
  ), x)

  conditional <- contrasts(run_analysis(plan, x))

  expect_true(all(conditional$effect_measure == "odds_ratio"))
  expect_true(all(conditional$scale == "ratio"))
  expect_true(all(conditional$estimate > 0))
})

test_that("within_levels requires the modifier in the fitted plan", {
  x <- as_bq_data(tibble::tibble(
    y = 1:6, treatment = rep(c("P", "D"), 3), sex = rep(c("F", "M"), 3)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_comparisons(treatment, within_levels(sex))
  plan <- validate_plan(plan_analysis(x, predictors = treatment), x)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "effect modifier")
})

test_that("contrast_of_contrasts computes a covariance-aware difference in differences", {
  x <- as_bq_data(tibble::tibble(
    y = c(1, 3, 2, 5, 5, 10, 6, 12, 2, 4, 3, 6, 7, 13, 8, 15),
    treatment = rep(c("P", "D"), 8),
    sex = rep(rep(c("F", "M"), each = 4), 2)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_comparisons(
      treatment,
      contrast_of_contrasts(
        sex, inner = against_reference("P"),
        outer = against_reference("F"), exponentiate = FALSE
      ),
      adjust = "holm"
    )
  plan <- validate_plan(plan_analysis(
    x, predictors = treatment, effect_modifiers = sex
  ), x)
  result <- run_analysis(plan, x)
  output <- contrasts(result)
  fit <- models(result)[[plan$analysis_id]]
  interaction_name <- grep(":", names(stats::coef(fit)), value = TRUE)

  expect_equal(nrow(output), 1L)
  expect_identical(output$estimand, "difference_of_differences")
  expect_equal(output$estimate, unname(stats::coef(fit)[interaction_name]))
  expect_equal(output$std_error,
    sqrt(stats::vcov(fit)[interaction_name, interaction_name]))
  expect_identical(output$inner_contrast, "D - P")
  expect_identical(output$outer_contrast, "M - F")
  expect_false(output$exponentiated)
  expect_equal(output$p_adjusted, output$p_value)
})

test_that("contrast_of_contrasts exponentiates logistic contrasts only explicitly", {
  x <- as_bq_data(tibble::tibble(
    y = c(0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0,
      0, 1, 0, 1, 1, 1, 0, 1),
    treatment = rep(rep(c("P", "D"), each = 6), 2),
    sex = rep(c("F", "M"), each = 12)
  )) |>
    set_outcome(y, type = "binary", event = 1) |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_comparisons(
      treatment,
      contrast_of_contrasts(
        sex, inner = against_reference("P"),
        outer = against_reference("F"), exponentiate = TRUE
      )
    )
  plan <- validate_plan(plan_analysis(
    x, predictors = treatment, effect_modifiers = sex
  ), x)
  result <- run_analysis(plan, x)
  output <- contrasts(result)
  fit <- models(result)[[plan$analysis_id]]
  interaction_name <- grep(":", names(stats::coef(fit)), value = TRUE)

  expect_equal(output$estimate, exp(unname(stats::coef(fit)[interaction_name])))
  expect_identical(output$effect_measure, "ratio_of_odds_ratios")
  expect_identical(output$scale, "ratio")
  expect_true(output$exponentiated)
})

test_that("contrast_of_contrasts validates its explicit contract", {
  expect_error(
    contrast_of_contrasts(
      sex, against_reference("P"), against_reference("F")
    ),
    class = "bq_error_invalid_comparison"
  )
  expect_error(
    contrast_of_contrasts(
      sex, inner = within_levels(sex), outer = all_pairwise(),
      exponentiate = FALSE
    ),
    class = "bq_error_invalid_comparison"
  )
})
