test_that("correlation method constructors declare estimands and CI", {
  expect_identical(pearson_correlation()$id, "pearson")
  expect_identical(pearson_correlation()$ci_method, "fisher_z")
  expect_identical(spearman_correlation()$ci_method, "fisher_z_approximation")
  expect_identical(kendall_correlation()$ci_method, "normal_approximation")
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
