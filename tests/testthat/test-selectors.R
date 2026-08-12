selected_names <- function(data, expression) {
  names(tidyselect::eval_select(rlang::enquo(expression), data))
}

test_that("role selectors include variables regardless of status", {
  x <- as_bq_data(tibble::tibble(y_review = c("a", "b"), y_valid = 1:2, x = 3:4)) |>
    set_outcome(y_review) |>
    set_outcome(y_valid, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_role(x, "group")

  expect_identical(selected_names(x, all_outcomes()), c("y_review", "y_valid"))
  expect_identical(selected_names(x, all_predictors()), "x")
  expect_identical(selected_names(x, all_groups()), "x")
  expect_identical(selected_names(x, where_role("group")), "x")
})

test_that("metadata selectors compose with tidyselect operators", {
  x <- as_bq_data(tibble::tibble(
    primary = 1:3,
    secondary_exploratory = 2:4,
    treatment = c("A", "B", "A")
  )) |>
    set_outcome(c(primary, secondary_exploratory), type = "continuous") |>
    set_predictor(treatment, type = "nominal")

  expect_identical(
    selected_names(x, all_outcomes() & !matches("_exploratory$")),
    "primary"
  )
  expect_identical(
    selected_names(x, where_continuous() | where_nominal()),
    names(x)
  )
})

test_that("type and status selectors use registry metadata", {
  x <- as_bq_data(tibble::tibble(binary = c("no", "yes"), numeric = 1:2)) |>
    set_predictor(numeric, type = "count")

  expect_identical(selected_names(x, where_type("binary")), "binary")
  expect_identical(selected_names(x, where_binary()), "binary")
  expect_identical(selected_names(x, where_count()), "numeric")
  expect_identical(selected_names(x, where_status("review")), "binary")
  expect_identical(selected_names(x, where_status("valid")), "numeric")
  expect_identical(selected_names(x, where_inferred()), "binary")
})

test_that("distribution selectors compose with role selectors", {
  x <- as_bq_data(tibble::tibble(y = 1:3, x = 2:4)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  registry <- attr(x, "variable_registry")
  registry$distribution <- c("gaussian", "skewed")
  attr(x, "variable_registry") <- registry

  expect_identical(
    selected_names(x, all_outcomes() & where_gaussian()),
    "y"
  )
  expect_identical(selected_names(x, where_skewed()), "x")
})

test_that("selectors validate their arguments", {
  x <- as_bq_data(tibble::tibble(x = 1:3))

  expect_error(
    tidyselect::eval_select(rlang::expr(where_type("unsupported")), x),
    class = "bq_error_invalid_variable_type"
  )
  expect_error(
    tidyselect::eval_select(rlang::expr(where_role("unsupported")), x),
    class = "bq_error_invalid_role"
  )
})
