test_that("add_statistic() adds variables with the default specification", {
  plan <- plan_summary(labelled_data())
  result <- add_statistic(plan, c(age, bmi))

  expect_identical(result$variables, c("v001", "v003"))
  expect_identical(
    result$statistics,
    tibble::tibble(
      statistic_id = "s001",
      name = "descriptives",
      kind = "continuous_statistic",
      source = "built_in_raw",
      missing = "omit"
    )
  )
  expect_identical(
    result$statistic_components,
    tibble::tibble(
      statistic_id = rep("s001", 7),
      component = c("mean", "sd", "median", "q1", "q3", "min", "max"),
      type = rep("double", 7),
      scale = rep("variable", 7),
      rounding = rep(NA_character_, 7),
      digits = rep(NA_integer_, 7),
      position = 1:7
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
  expect_identical(result$next_statistic_number, 2L)
})

test_that("add_statistic() preserves first-selection variable order", {
  skip_if_not_installed("datawizard")
  result <- labelled_data() |>
    plan_summary() |>
    add_statistic(bmi, continuous_descriptives_extended()) |>
    add_statistic(age)

  expect_identical(result$variables, c("v003", "v001"))
  expect_identical(
    result$statistics$name,
    c("descriptives_extended", "descriptives")
  )
  expect_identical(result$statistics$statistic_id, c("s001", "s002"))
})

test_that("add_statistic() returns a new plan without changing its input", {
  plan <- plan_summary(labelled_data())
  statistic <- continuous_statistic(
    "value",
    function(x) data.frame(value = NA_real_)
  )
  result <- add_statistic(plan, age, statistic)

  expect_identical(plan$variables, character())
  expect_identical(nrow(plan$statistics), 0L)
  expect_identical(length(plan$statistic_functions), 0L)
  expect_identical(nrow(result$statistics), 1L)
})

test_that("add_statistic() gives later specifications fresh identifiers", {
  first <- continuous_statistic("first", function(x) data.frame(value = NA_real_))
  second <- continuous_statistic("second", function(x) data.frame(value = NA_real_))

  result <- labelled_data() |>
    plan_summary() |>
    add_statistic(age, first) |>
    add_statistic(age, second)

  expect_identical(result$variables, "v001")
  expect_identical(result$statistics$statistic_id, c("s001", "s002"))
  expect_identical(names(result$statistic_functions), c("s001", "s002"))
  expect_identical(result$next_statistic_number, 3L)
})

test_that("add_statistic() resolves renamed variables by var_id", {
  data <- dplyr::rename(labelled_data(), years = age)
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))

  result <- plan_summary(data) |>
    add_statistic(years, statistic)

  expect_identical(result$variables, "v001")
  expect_identical(result$statistic_assignments$var_id, "v001")
})

test_that("add_statistic() rejects design axes", {
  plan <- plan_summary(labelled_data(), group = sex)
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))

  expect_error(
    add_statistic(plan, sex, statistic),
    class = "bq_error_invalid_plan"
  )
  expect_error(add_statistic(plan, sex, statistic), "design axis")
})

test_that("add_statistic() allows repeated names only for different variables", {
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))
  plan <- labelled_data() |>
    plan_summary() |>
    add_statistic(age, statistic) |>
    add_statistic(bmi, statistic)

  expect_identical(plan$statistics$name, c("value", "value"))
  expect_true(preflight(plan)$ok)
  expect_error(
    add_statistic(plan, age, statistic),
    class = "bq_error_invalid_plan"
  )
  expect_error(add_statistic(plan, age, statistic), "already has")
})

test_that("add_statistic() validates plan, selection and statistic objects", {
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))
  plan <- plan_summary(labelled_data())

  expect_error(
    add_statistic(labelled_data(), age, statistic),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    add_statistic(plan, missing, statistic),
    class = "bq_error_invalid_selection"
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
  data <- set_type(labelled_data(), sex, type_binary("m"))
  statistic <- continuous_statistic("value", function(x) data.frame(value = NA_real_))

  expect_no_error(
    plan_summary(data) |>
      add_statistic(sex, statistic)
  )
})
