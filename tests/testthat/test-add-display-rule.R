test_that("add_display_rule() assigns one rule to several variables", {
  plan <- plan_summary(labelled_data()) |>
    add_statistic(c(age, bmi))
  result <- add_display_rule(
    plan,
    c(age, bmi),
    enumerate_values(max_n = 3L)
  )

  expect_identical(
    result$display_rules,
    tibble::tibble(
      rule_id = "r001",
      kind = "enumerate_values",
      max_n = 3L,
      display_statistics = FALSE
    )
  )
  expect_identical(
    result$display_rule_assignments,
    tibble::tibble(
      rule_id = rep("r001", 2),
      var_id = c("v001", "v003")
    )
  )
  expect_identical(result$next_display_rule_number, 2L)
})

test_that("add_display_rule() returns a new plan without changing its input", {
  plan <- plan_summary(labelled_data()) |>
    add_statistic(age)
  result <- add_display_rule(plan, age, enumerate_values())

  expect_identical(nrow(plan$display_rules), 0L)
  expect_identical(nrow(plan$display_rule_assignments), 0L)
  expect_identical(nrow(result$display_rules), 1L)
})

test_that("add_display_rule() gives later rules fresh identifiers", {
  plan <- plan_summary(labelled_data()) |>
    add_statistic(c(age, bmi))
  result <- plan |>
    add_display_rule(age, enumerate_values(2L)) |>
    add_display_rule(bmi, enumerate_values(3L))

  expect_identical(result$display_rules$rule_id, c("r001", "r002"))
  expect_identical(result$display_rule_assignments$rule_id, c("r001", "r002"))
  expect_identical(result$next_display_rule_number, 3L)
})

test_that("add_display_rule() resolves renamed variables by var_id", {
  data <- dplyr::rename(labelled_data(), years = age)
  plan <- plan_summary(data) |>
    add_statistic(years)

  result <- add_display_rule(plan, years, enumerate_values())

  expect_identical(result$display_rule_assignments$var_id, "v001")
})

test_that("add_display_rule() rejects columns outside summary variables", {
  plan <- plan_summary(labelled_data(), group = sex) |>
    add_statistic(age)

  expect_error(
    add_display_rule(plan, bmi, enumerate_values()),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    add_display_rule(plan, sex, enumerate_values()),
    "not a summary variable"
  )
})

test_that("add_display_rule() permits only one rule per variable", {
  plan <- plan_summary(labelled_data()) |>
    add_statistic(c(age, bmi))
  plan <- add_display_rule(plan, age, enumerate_values())

  expect_error(
    add_display_rule(plan, c(age, bmi), enumerate_values(3L)),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    add_display_rule(plan, age, enumerate_values(3L)),
    "already has a display rule"
  )
})

test_that("add_display_rule() validates plan and rule objects", {
  plan <- plan_summary(labelled_data()) |>
    add_statistic(age)

  expect_error(
    add_display_rule(labelled_data(), age, enumerate_values()),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    add_display_rule(plan, age, "enumerate"),
    class = "bq_error_invalid_display_rule"
  )

  malformed <- enumerate_values()
  malformed$max_n <- 2
  expect_error(
    add_display_rule(plan, age, malformed),
    class = "bq_error_invalid_display_rule"
  )
})
