test_that("coding and comparison specifications are stored independently", {
  x <- as_bq_data(tibble::tibble(
    y = 1:6,
    treatment = c("Placebo", "Placebo", "A", "A", "B", "B")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "nominal") |>
    set_coding(treatment, coding = "treatment", reference = "Placebo") |>
    set_comparisons(
      treatment,
      comparisons = against_reference("Placebo"),
      adjust = "holm"
    )

  expect_identical(variables(x)$coding[[2]], "treatment")
  expect_identical(variables(x)$reference[[2]], "Placebo")
  expect_s3_class(contrasts(x), "tbl_df")
  expect_identical(contrasts(x)$comparison_type, "against_reference")
  expect_identical(contrasts(x)$reference, list("Placebo"))
  expect_identical(contrasts(x)$adjust_method, "holm")
  expect_identical(contrasts(x)$predictor_id, variables(x)$var_id[[2]])
})

test_that("contrast specs validate coding and adjustment", {
  x <- as_bq_data(tibble::tibble(x = 1:3)) |>
    set_predictor(x, type = "continuous")

  expect_error(
    set_coding(x, x, coding = "sum", reference = 1),
    class = "bq_error_invalid_coding"
  )
  expect_error(
    set_coding(x, x, coding = "treatment", reference = 1),
    class = "bq_error_invalid_coding"
  )
  expect_error(
    set_comparisons(x, x, against_reference(1), adjust = "unknown"),
    class = "bq_error_invalid_comparison"
  )
})

test_that("multi-group contrast specifications declare pair strategies", {
  expect_identical(all_pairwise()$type, "all_pairwise")
  expect_identical(consecutive_comparisons()$type, "consecutive")
  expect_identical(against_reference("A")$reference, "A")
  expect_error(
    against_global_mean(), class = "bq_error_invalid_comparison"
  )
  expect_identical(against_global_mean(exponentiate = FALSE)$weights, "equal")
  expect_identical(
    against_global_mean(exponentiate = FALSE, weights = "observed")$weights,
    "observed"
  )
})

test_that("model contrasts compare every level with the global mean", {
  raw <- tibble::tibble(
    y = c(1, 2, 4, 5, 8, 9),
    arm = factor(rep(c("A", "B", "C"), each = 2))
  )
  data <- as_bq_data(raw) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_comparisons(
      arm, against_global_mean(exponentiate = FALSE), adjust = "holm"
    )
  result <- data |>
    plan_analysis(y, arm) |>
    validate_plan(data) |>
    run_analysis(data)
  output <- contrasts(result)
  expected <- tapply(raw$y, raw$arm, mean) - mean(tapply(raw$y, raw$arm, mean))

  expect_identical(output$numerator, c("A", "B", "C"))
  expect_true(all(output$denominator == ".global_mean"))
  expect_equal(output$estimate, as.numeric(expected))
  expect_true(all(output$scale == "identity"))
})

test_that("global mean contrasts explicitly control exponentiation", {
  raw <- tibble::tibble(
    response = c(0, 1, 0, 1, 1, 1, 0, 0, 1),
    arm = factor(rep(c("A", "B", "C"), each = 3))
  )
  base <- as_bq_data(raw) |>
    set_outcome(response, type = "binary", event = 1) |>
    set_predictor(arm, type = "nominal", reference = "A")
  log_data <- base |>
    set_comparisons(arm, against_global_mean(exponentiate = FALSE))
  ratio_data <- base |>
    set_comparisons(arm, against_global_mean(exponentiate = TRUE))

  log_result <- log_data |> plan_analysis(response, arm) |>
    validate_plan(log_data) |> run_analysis(log_data)
  ratio_result <- ratio_data |> plan_analysis(response, arm) |>
    validate_plan(ratio_data) |> run_analysis(ratio_data)

  expect_equal(contrasts(ratio_result)$estimate, exp(contrasts(log_result)$estimate))
  expect_equal(
    contrasts(ratio_result)$std_error, contrasts(log_result)$std_error
  )
  expect_true(all(is.finite(contrasts(log_result)$std_error)))
  expect_true(all(contrasts(log_result)$std_error_scale == "link"))
  expect_true(all(contrasts(ratio_result)$std_error_scale == "link"))
  expect_true(all(
    contrasts(ratio_result)$conf_low < contrasts(ratio_result)$estimate &
      contrasts(ratio_result)$estimate < contrasts(ratio_result)$conf_high
  ))
  expect_true(all(contrasts(log_result)$scale == "link"))
  expect_true(all(contrasts(ratio_result)$scale == "ratio"))
})

test_that("model comparisons follow all-pairwise and consecutive strategies", {
  set.seed(12)
  raw <- tibble::tibble(
    y = rnorm(90), arm = factor(rep(c("A", "B", "C"), each = 30))
  )
  base <- as_bq_data(raw) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(arm, type = "nominal", reference = "A")

  pairwise_data <- base |>
    set_comparisons(arm, all_pairwise(), adjust = "holm")
  pairwise <- pairwise_data |>
    plan_analysis(y, arm) |>
    validate_plan(pairwise_data)
  pairwise_result <- run_analysis(pairwise, pairwise_data)
  expect_setequal(
    paste(contrasts(pairwise_result)$numerator, contrasts(pairwise_result)$denominator),
    c("B A", "C A", "C B")
  )
  expect_true(all(contrasts(pairwise_result)$adjust_method == "holm"))
  expect_true("predictor_omnibus" %in% tests(pairwise_result)$test)

  consecutive_data <- base |>
    set_comparisons(arm, consecutive_comparisons(), adjust = "none")
  consecutive_result <- consecutive_data |>
    plan_analysis(y, arm) |>
    validate_plan(consecutive_data) |>
    run_analysis(consecutive_data)
  expect_identical(
    paste(contrasts(consecutive_result)$numerator, contrasts(consecutive_result)$denominator),
    c("B A", "C B")
  )
})

test_that("run_analysis computes treatment contrasts independently of estimates", {
  x <- as_bq_data(tibble::tibble(
    y = c(2.0, 2.2, 4.0, 4.2, 6.0, 6.2),
    treatment = c("Placebo", "Placebo", "A", "A", "B", "B")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "nominal") |>
    set_coding(treatment, reference = "Placebo") |>
    set_comparisons(treatment, against_reference("Placebo"), adjust = "holm")
  result <- run_analysis(validate_plan(plan_analysis(x), x), x)
  comparison <- contrasts(result)
  coefficient_rows <- estimates(result)$term == "treatment"

  expect_identical(comparison$numerator, c("A", "B"))
  expect_identical(comparison$denominator, c("Placebo", "Placebo"))
  expect_identical(comparison$estimate, estimates(result)$estimate[coefficient_rows])
  expect_identical(comparison$adjust_method, c("holm", "holm"))
  expect_identical(
    comparison$p_adjusted,
    stats::p.adjust(comparison$p_value, method = "holm")
  )
})

test_that("a plan links multiple estimands without duplicating model tasks", {
  x <- as_bq_data(tibble::tibble(
    y = 1:6,
    treatment = c("Placebo", "Placebo", "A", "A", "B", "B")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "nominal") |>
    set_coding(treatment, reference = "Placebo") |>
    set_comparisons(treatment, against_reference("Placebo"), adjust = "none") |>
    set_comparisons(treatment, against_reference("Placebo"), adjust = "holm")

  plan <- plan_analysis(x)
  result <- run_analysis(validate_plan(plan, x), x)

  expect_equal(nrow(plan), 1L)
  expect_identical(plan$contrast_ids[[1]], contrasts(x)$contrast_id)
  expect_equal(nrow(contrasts(result)), 4L)
  expect_setequal(contrasts(result)$contrast_id, contrasts(x)$contrast_id)
  expect_length(models(result), 1L)
})
