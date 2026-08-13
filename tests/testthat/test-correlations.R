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
