test_that("add_statistic() stores one specification for several variables", {
  plan <- plan_summary(labelled_data(), c(age, bmi))
  fun <- function(x) data.frame(
    centre = if (length(x) == 0L) NA_real_ else mean(x, na.rm = TRUE),
    observed = sum(!is.na(x))
  )
  statistic <- continuous_statistic(
    "custom_centre",
    fun,
    scale = c(centre = "variable", observed = "count")
  )
  result <- add_statistic(plan, c(age, bmi), statistic)

  expect_identical(
    result$statistics,
    tibble::tibble(
      statistic_id = "s001",
      name = "custom_centre",
      kind = "continuous_statistic",
      source = "custom_raw",
      missing = "user"
    )
  )
  expect_identical(
    result$statistic_components,
    tibble::tibble(
      statistic_id = rep("s001", 2),
      component = c("centre", "observed"),
      type = c("double", "integer"),
      scale = c("variable", "count"),
      rounding = c(NA_character_, NA_character_),
      digits = c(NA_integer_, NA_integer_),
      position = 1:2
    )
  )
  expect_identical(
    result$statistic_assignments,
    tibble::tibble(
      statistic_id = rep("s001", 2),
      var_id = c("v001", "v003")
    )
  )
  expect_identical(names(result$statistic_functions), "s001")
  expect_identical(result$statistic_functions[["s001"]], fun)
  expect_identical(result$next_statistic_number, 2L)
})

test_that("add_statistic() returns a new plan without changing its input", {
  plan <- plan_summary(labelled_data(), age)
  statistic <- continuous_statistic(
    "value",
    function(x) data.frame(value = NA_real_)
  )
  result <- add_statistic(plan, age, statistic)

  expect_identical(nrow(plan$statistics), 0L)
  expect_identical(length(plan$statistic_functions), 0L)
  expect_identical(nrow(result$statistics), 1L)
})

test_that("add_statistic() gives later specifications fresh identifiers", {
  plan <- plan_summary(labelled_data(), age)
  first <- continuous_statistic("first", function(x) data.frame(value = NA_real_))
  second <- continuous_statistic("second", function(x) data.frame(value = NA_real_))

  result <- plan |>
    add_statistic(age, first) |>
    add_statistic(age, second)

  expect_identical(result$statistics$statistic_id, c("s001", "s002"))
  expect_identical(names(result$statistic_functions), c("s001", "s002"))
  expect_identical(result$next_statistic_number, 3L)
})

test_that("add_statistic() resolves renamed planned variables by var_id", {
  data <- dplyr::rename(labelled_data(), years = age)
  plan <- plan_summary(data, years)
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))

  result <- add_statistic(plan, years, statistic)

  expect_identical(result$statistic_assignments$var_id, "v001")
})

test_that("add_statistic() rejects columns outside the plan variables", {
  plan <- plan_summary(labelled_data(), age, group = sex)
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))

  expect_error(
    add_statistic(plan, bmi, statistic),
    class = "bq_error_invalid_plan"
  )
  expect_error(add_statistic(plan, sex, statistic), "not a summary variable")
})

test_that("add_statistic() requires a unique name within the plan", {
  plan <- plan_summary(labelled_data(), age)
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))
  plan <- add_statistic(plan, age, statistic)

  expect_error(
    add_statistic(plan, age, statistic),
    class = "bq_error_invalid_plan"
  )
  expect_error(add_statistic(plan, age, statistic), "already used")
})

test_that("add_statistic() validates plan and statistic objects", {
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))
  plan <- plan_summary(labelled_data(), age)

  expect_error(
    add_statistic(labelled_data(), age, statistic),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    add_statistic(plan, age, "value"),
    class = "bq_error_invalid_statistic"
  )

  malformed <- continuous_statistic(
    "value",
    function(x) data.frame(value = NA_real_)
  )
  malformed$components <- ""
  expect_error(
    add_statistic(plan, age, malformed),
    class = "bq_error_invalid_statistic"
  )
})

test_that("add_statistic() leaves type compatibility to preflight", {
  data <- set_type(labelled_data(), sex, binary("m"))
  plan <- plan_summary(data, sex)
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))

  expect_no_error(add_statistic(plan, sex, statistic))
})
