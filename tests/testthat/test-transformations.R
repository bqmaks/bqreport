test_that("structured transformations are stored and compiled by stable id", {
  x <- as_bq_data(tibble::tibble(y = 1:5, age = c(20, 30, 40, 50, 60))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(age, type = "continuous") |>
    set_transformation(age, per(10))

  plan <- plan_analysis(x)
  spec <- plan$transformation_specs[[1]][[variables(x)$var_id[[2]]]]

  expect_identical(spec$id, "per_10")
  expect_identical(spec$effect_increment, 10)
  expect_identical(spec$label, "per 10 units")
})

test_that("per transformation changes coefficient scale but not source data", {
  x <- as_bq_data(tibble::tibble(y = c(1, 2, 3, 4, 5), age = c(20, 30, 40, 50, 60))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(age, type = "continuous") |>
    set_transformation(age, per(10))

  result <- run_analysis(validate_plan(plan_analysis(x), x), x)
  slope <- estimates(result)[estimates(result)$term == "age", ]

  expect_equal(slope$estimate, 1)
  expect_identical(slope$transformation_id, "per_10")
  expect_identical(slope$transformation_label, "per 10 units")
  expect_identical(x$age, c(20, 30, 40, 50, 60))
})

test_that("log2 transformation represents effect per doubling", {
  x <- as_bq_data(tibble::tibble(y = c(1, 2, 3, 4), biomarker = c(1, 2, 4, 8))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(biomarker, type = "continuous") |>
    set_transformation(biomarker, log2_transform())

  result <- run_analysis(validate_plan(plan_analysis(x), x), x)
  slope <- estimates(result)[estimates(result)$term == "biomarker", ]

  expect_equal(slope$estimate, 1)
  expect_identical(slope$transformation_label, "per doubling")
})

test_that("log transformation domain is checked during preflight", {
  x <- as_bq_data(tibble::tibble(y = 1:4, biomarker = c(1, 2, 0, -1))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(biomarker, type = "continuous") |>
    set_transformation(biomarker, log2_transform())

  plan <- validate_plan(plan_analysis(x), x)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "strictly positive")
})

test_that("custom scalar transformation is validated and recorded", {
  shifted <- transformation_function(
    id = "shift_by_one", label = "per shifted unit",
    transform = function(x, context) x + 1,
    parameters = list(offset = 1)
  )
  x <- as_bq_data(tibble::tibble(y = 1:4, x = 2:5)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_transformation(x, shifted)

  result <- run_analysis(validate_plan(plan_analysis(x), x), x)

  expect_identical(unique(estimates(result)$transformation_id[estimates(result)$term == "x"]), "shift_by_one")
  expect_true(nzchar(variables(x)$transformation[[2]]$function_hash))
})

test_that("invalid custom transformation output fails preflight", {
  bad <- transformation_function(
    id = "bad", label = "bad", transform = function(x, context) x[-1]
  )
  x <- as_bq_data(tibble::tibble(y = 1:4, x = 2:5)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_transformation(x, bad)

  plan <- validate_plan(plan_analysis(x), x)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "same length")
})
