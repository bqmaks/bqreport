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
