test_that("plan_summary() records a minimal summary plan", {
  data <- labelled_data()
  plan <- plan_summary(data, c(age, bmi))

  expect_s3_class(plan, c("bq_plan_summary", "bq_plan"), exact = TRUE)
  expect_identical(
    unclass(plan),
    list(
      analysis = "summary",
      data = data,
      variables = c("v001", "v003"),
      group = character(),
      strata = character(),
      overall = character(),
      statistics = tibble::tibble(
        statistic_id = character(),
        name = character(),
        kind = character(),
        source = character(),
        missing = character()
      ),
      statistic_components = tibble::tibble(
        statistic_id = character(),
        component = character(),
        type = character(),
        position = integer()
      ),
      statistic_assignments = tibble::tibble(
        statistic_id = character(),
        var_id = character()
      ),
      statistic_functions = list(),
      next_statistic_number = 1L
    )
  )
})

test_that("plan_summary() records group, multiple strata and overall axes", {
  data <- as_bq_data(tibble::tibble(
    outcome = 1:3,
    treatment = c("A", "B", "A"),
    centre = c("X", "X", "Y"),
    sex = c("f", "m", "m")
  ))
  plan <- plan_summary(
    data,
    outcome,
    group = treatment,
    strata = c(centre, sex),
    overall = c("strata", "group")
  )

  expect_identical(plan$variables, "v001")
  expect_identical(plan$group, "v002")
  expect_identical(plan$strata, c("v003", "v004"))
  expect_identical(plan$overall, c("group", "strata"))
})

test_that("plan_summary() resolves renamed columns to their stable identifiers", {
  data <- dplyr::rename(labelled_data(), years = age)
  plan <- plan_summary(data, years)

  expect_identical(plan$variables, "v001")
  expect_identical(plan$data, data)
})

test_that("plan_summary() requires an available axis for each overall", {
  data <- labelled_data()

  expect_error(
    plan_summary(data, age, overall = "group"),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    plan_summary(data, age, overall = "strata"),
    class = "bq_error_invalid_plan"
  )
})

test_that("plan_summary() validates the overall specification", {
  data <- labelled_data()

  for (overall in list(NA_character_, "centre", c("group", "group"), 1)) {
    expect_error(
      plan_summary(data, age, group = sex, overall = overall),
      class = "bq_error_invalid_plan"
    )
  }
})

test_that("plan_summary() keeps summarised variables separate from design axes", {
  data <- labelled_data()

  expect_error(
    plan_summary(data, age, group = age),
    class = "bq_error_invalid_plan"
  )
  expect_error(plan_summary(data, age, group = age), "both summarised")
  expect_error(
    plan_summary(data, age, strata = age),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    plan_summary(data, age, group = sex, strata = sex),
    class = "bq_error_invalid_plan"
  )
  expect_error(plan_summary(data, age, group = sex, strata = sex), "both `group` and `strata`")
})

test_that("plan_summary() validates data and selection cardinality", {
  data <- labelled_data()

  expect_error(
    plan_summary(tibble::tibble(age = 1), age),
    class = "bq_error_invalid_data"
  )
  expect_error(plan_summary(data, missing), class = "bq_error_invalid_selection")
  expect_error(
    plan_summary(data, age, group = c(sex, bmi)),
    class = "bq_error_invalid_selection"
  )
  expect_error(plan_summary(data, age, group = c(sex, bmi)), "at most 1 column")
})

test_that("a summary plan prints its selections without printing data", {
  data <- as_bq_data(tibble::tibble(
    outcome = 1:3,
    treatment = c("A", "B", "A"),
    centre = c("X", "X", "Y"),
    sex = c("f", "m", "m")
  ))
  plan <- plan_summary(
    data,
    outcome,
    group = treatment,
    strata = c(centre, sex),
    overall = c("group", "strata")
  )

  output <- capture.output(visibility <- withVisible(print(plan)))

  expect_identical(
    output,
    c(
      "<bq summary plan>",
      "Variables: outcome",
      "Group: treatment",
      "Strata: centre, sex",
      "Overall: group, strata"
    )
  )
  expect_false(visibility$visible)
  expect_identical(visibility$value, plan)
})

test_that("a summary plan prints absent design axes as none", {
  plan <- plan_summary(labelled_data(), c(age, bmi))
  output <- capture.output(print(plan))

  expect_identical(output[3:5], c("Group: none", "Strata: none", "Overall: none"))
})
