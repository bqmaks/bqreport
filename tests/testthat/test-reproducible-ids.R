make_reproducibility_data <- function() {
  tibble::tibble(
    response = c(0, 0, 1, 0, 1, 1, 1, 0),
    treatment = factor(rep(c("Control", "Treatment"), each = 4)),
    age = c(44, 57, 51, 63, 46, 55, 60, 49)
  ) |>
    as_bq_data() |>
    set_outcome(response, type = "binary", event = 1) |>
    set_predictor(treatment, type = "binary", reference = "Control") |>
    set_predictor(age, type = "continuous")
}

test_that("identical inputs produce identical variable registries", {
  expect_identical(
    variables(make_reproducibility_data()),
    variables(make_reproducibility_data())
  )
})

test_that("identical inputs compile identical analysis identifiers", {
  make_plan <- function() {
    data <- make_reproducibility_data()
    data |>
      plan_analysis(response, treatment, covariates = age) |>
      validate_plan(data)
  }
  first <- make_plan()
  second <- make_plan()

  expect_identical(first$analysis_id, second$analysis_id)
  expect_identical(first$outcome_id, second$outcome_id)
  expect_identical(first$contrast_ids, second$contrast_ids)
})

test_that("correlation plans compile identical family and interaction ids", {
  make_plan <- function() {
    data <- as_bq_data(tibble::tibble(
      x = c(1, 4, 2, 8, 5, 9), y = c(2, 1, 5, 4, 8, 7), z = c(3, 1, 4, 2, 6, 5)
    ))
    plan_correlations(data, c(x, y, z))
  }
  first <- make_plan()
  second <- make_plan()

  expect_identical(first$analysis_id, second$analysis_id)
  expect_identical(first$correlation_family_id, second$correlation_family_id)
  expect_identical(
    first$correlation_interaction_id, second$correlation_interaction_id
  )
  expect_identical(anyDuplicated(first$analysis_id), 0L)
})

test_that("different correlation methods compile different analysis ids", {
  data <- as_bq_data(tibble::tibble(
    x = c(1, 4, 2, 8, 5, 9), y = c(2, 1, 5, 4, 8, 7)
  ))
  pearson <- plan_correlations(data, x, with = y)
  spearman <- plan_correlations(
    data, x, with = y, method = spearman_correlation()
  )

  expect_false(pearson$analysis_id == spearman$analysis_id)
})

test_that("analysis types namespace otherwise identical tasks", {
  data <- as_bq_data(tibble::tibble(
    value = c(1.2, 3.4, 2.8, 5.1, 4.4, 6.0),
    arm = factor(rep(c("A", "B"), 3))
  )) |>
    set_outcome(value, type = "continuous") |>
    set_predictor(arm, type = "binary", reference = "A") |>
    set_role(arm, "group")
  regression <- plan_analysis(data, value, arm)
  descriptive <- plan_descriptives(data, value, group = arm)

  expect_false(any(regression$analysis_id %in% descriptive$analysis_id))
})

test_that("a reused column name does not collide with a retained variable id", {
  data <- as_bq_data(tibble::tibble(a = c(1, 2, 3), b = c(2, 3, 4)))
  original <- variables(data)
  renamed <- dplyr::rename(data, c = a)
  mutated <- dplyr::mutate(renamed, a = c(4, 5, 6))
  registry <- variables(mutated)

  expect_identical(anyDuplicated(registry$var_id), 0L)
  expect_identical(
    registry$var_id[registry$name == "c"],
    original$var_id[original$name == "a"]
  )
})

test_that("survival outcomes and longitudinal designs have stable ids", {
  make_survival <- function() {
    as_bq_data(tibble::tibble(
      os_time = c(5, 8, 3, 9, 7, 4), death = c(1, 0, 1, 0, 1, 0)
    )) |>
      add_survival_outcome(
        os, time = os_time, event = death, event_value = 1,
        time_unit = "months"
      ) |>
      outcomes()
  }
  expect_identical(make_survival()$outcome_id, make_survival()$outcome_id)

  make_design <- function() {
    as_bq_data(tibble::tibble(
      patient = rep(c("p1", "p2", "p3"), each = 2),
      visit = rep(c("V0", "V1"), 3),
      bmi = c(22, 23, 25, 24, 27, 28)
    )) |>
      set_longitudinal_design(
        id = patient, time = visit, layout = "long", baseline = "V0",
        time_scale = "categorical"
      ) |>
      designs()
  }
  expect_identical(make_design()$design_id, make_design()$design_id)
})
