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
