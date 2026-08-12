test_that("effect modifiers compile an explicit interaction formula", {
  x <- as_bq_data(tibble::tibble(
    y = 1:8, treatment = rep(c("Placebo", "Drug"), 4),
    sex = rep(c("F", "M"), each = 4), age = 30:37
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "Placebo") |>
    set_predictor(sex, type = "binary", reference = "F") |>
    set_predictor(age, type = "continuous")

  plan <- plan_analysis(
    x, predictors = treatment, covariates = age, effect_modifiers = sex
  )

  expect_identical(plan$effect_modifier_ids[[1]], variables(x)$var_id[[3]])
  expect_identical(plan$effect_modifiers[[1]], "sex")
  expect_identical(deparse(plan$formula[[1]]), "y ~ treatment * sex + age")
})

test_that("interaction preflight detects empty cells", {
  x <- as_bq_data(tibble::tibble(
    y = 1:6,
    treatment = c("Placebo", "Drug", "Placebo", "Placebo", "Placebo", "Placebo"),
    sex = c("F", "F", "F", "M", "M", "M")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "Placebo") |>
    set_predictor(sex, type = "binary", reference = "F")

  plan <- validate_plan(plan_analysis(
    x, predictors = treatment, effect_modifiers = sex
  ), x)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "empty predictor-by-modifier cells")
})

test_that("interaction model records coefficients and omnibus test", {
  x <- as_bq_data(tibble::tibble(
    y = c(1, 2, 2, 4, 5, 8, 6, 10),
    treatment = rep(c("Placebo", "Drug"), 4),
    sex = rep(c("F", "M"), each = 4)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "Placebo") |>
    set_predictor(sex, type = "binary", reference = "F")
  plan <- validate_plan(plan_analysis(
    x, predictors = treatment, effect_modifiers = sex
  ), x)

  result <- run_analysis(plan, x)

  expect_true(any(grepl(":", estimates(result)$term, fixed = TRUE)))
  interaction_test <- tests(result)[tests(result)$test == "interaction", ]
  expect_equal(nrow(interaction_test), 1L)
  direct_full <- stats::lm(y ~ treatment * sex, data = x)
  direct_reduced <- stats::lm(y ~ treatment + sex, data = x)
  expect_equal(interaction_test$p_value, stats::anova(direct_reduced, direct_full)$`Pr(>F)`[[2]])
})

test_that("strata and effect modification remain independent", {
  x <- as_bq_data(tibble::tibble(
    y = 1:16, treatment = rep(c("P", "D"), 8),
    sex = rep(rep(c("F", "M"), each = 4), 2), center = rep(c("A", "B"), each = 8)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "binary", reference = "P") |>
    set_predictor(sex, type = "binary", reference = "F")

  plan <- plan_analysis(
    x, predictors = treatment, strata = center, effect_modifiers = sex
  )

  expect_equal(nrow(plan), 2L)
  expect_true(all(vapply(plan$formula, deparse, character(1)) == "y ~ treatment * sex"))
})
