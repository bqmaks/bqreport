test_that("set_model_term stores structured nonlinear covariate metadata", {
  data <- as_bq_data(tibble::tibble(age = 20:30, response = rep(c(0, 1), length.out = 11))) |>
    set_model_term(age, natural_spline(df = 4))

  term <- variables(data)$model_term[[1]]
  expect_s3_class(term, "model_term_spec")
  expect_identical(term$id, "natural_spline")
  expect_identical(term$parameters$df, 4L)
  expect_null(term$resolved_parameters)
})

test_that("model term metadata is invalidated when its source changes", {
  data <- as_bq_data(tibble::tibble(age = 20:30)) |>
    set_model_term(age, polynomial_term(2)) |>
    dplyr::mutate(age = age * 12)

  expect_null(variables(data)$model_term[[1]])
  expect_identical(variables(data)$status, "review")
})

test_that("model terms validate their constructor domains", {
  expect_error(polynomial_term(0), class = "bq_error_invalid_model_term")
  expect_error(natural_spline(df = 1), class = "bq_error_invalid_model_term")
  expect_error(
    natural_spline(knots = c(2, 1)),
    class = "bq_error_invalid_model_term"
  )
})

test_that("nonlinear model terms are restricted to adjustment covariates", {
  data <- as_bq_data(tibble::tibble(y = 1:10, x = 1:10)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_model_term(x, polynomial_term(2))

  plan <- validate_plan(plan_analysis(data, y, x), data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "primary predictor", ignore.case = TRUE)
})

test_that("natural spline covariates agree with a direct lm", {
  set.seed(42)
  raw <- tibble::tibble(
    y = rnorm(80), treatment = rep(c("A", "B"), 40), age = runif(80, 20, 80)
  )
  data <- as_bq_data(raw) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    set_model_term(age, natural_spline(df = 4))
  plan <- data |>
    plan_analysis(y, treatment, covariates = age) |>
    validate_plan(data)
  result <- run_analysis(plan, data)
  resolved <- plan$model_term_specs[[1]][[variables(data)$var_id[variables(data)$name == "age"]]]
  direct <- stats::lm(
    y ~ treatment + splines::ns(
      age,
      knots = resolved$resolved_parameters$knots,
      Boundary.knots = resolved$resolved_parameters$boundary_knots
    ),
    data = raw
  )

  expect_identical(plan$status, "ready")
  expect_equal(stats::coef(models(result)[[1]]), stats::coef(direct), ignore_attr = TRUE)
  expect_identical(resolved$id, "natural_spline")
  expect_true(length(resolved$resolved_parameters$knots) > 0L)
})

test_that("orthogonal polynomial covariates are fixed during preflight", {
  set.seed(11)
  raw <- tibble::tibble(
    response = rbinom(100, 1, 0.5), treatment = rep(c("A", "B"), 50),
    age = runif(100, 20, 80)
  )
  data <- as_bq_data(raw) |>
    set_outcome(response, type = "binary", event = 1) |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    set_model_term(age, polynomial_term(2, raw = FALSE))
  plan <- validate_plan(
    plan_analysis(data, response, treatment, covariates = age), data
  )
  term <- plan$model_term_specs[[1]][[variables(data)$var_id[variables(data)$name == "age"]]]

  expect_identical(plan$status, "ready")
  expect_false(is.null(term$resolved_parameters$coefs))
  expect_length(term$output_names, 2L)

  before <- term$resolved_parameters
  result <- run_analysis(plan, data)
  expect_identical(plan$model_term_specs[[1]][[variables(data)$var_id[variables(data)$name == "age"]]]$resolved_parameters, before)
  expect_s3_class(models(result)[[1]], "glm")
})

test_that("nonlinear covariates produce a separate omnibus test", {
  set.seed(7)
  data <- as_bq_data(tibble::tibble(
    y = rnorm(60), treatment = rep(c("A", "B"), 30), age = runif(60, 20, 80)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(treatment, type = "nominal", reference = "A") |>
    set_model_term(age, polynomial_term(2, raw = TRUE))
  result <- data |>
    plan_analysis(y, treatment, covariates = age) |>
    validate_plan(data) |>
    run_analysis(data)

  term_test <- tests(result)
  term_test <- term_test[term_test$test == "model_term_omnibus", ]
  expect_equal(nrow(term_test), 1L)
  expect_identical(term_test$predictor, "age")
  expect_equal(term_test$df, 2)
})
