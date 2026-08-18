test_that("plan_summary() creates an empty summary design", {
  data <- labelled_data()
  plan <- plan_summary(data)

  expect_s3_class(plan, c("bq_plan_summary", "bq_plan"), exact = TRUE)
  expect_identical(
    unclass(plan),
    list(
      analysis = "summary",
      data = data,
      variables = character(),
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
        scale = character(),
        rounding = character(),
        digits = integer(),
        position = integer()
      ),
      statistic_assignments = tibble::tibble(
        statistic_id = character(),
        var_id = character()
      ),
      statistic_functions = list(),
      next_statistic_number = 1L,
      display_rules = tibble::tibble(
        rule_id = character(),
        kind = character(),
        max_n = integer(),
        display_statistics = logical()
      ),
      display_rule_assignments = tibble::tibble(
        rule_id = character(),
        var_id = character()
      ),
      next_display_rule_number = 1L
    )
  )
})

test_that("plan_summary() records group, multiple strata and overall axes", {
  data <- as_bq_data(tibble::tibble(
    treatment = c("A", "B", "A"),
    centre = c("X", "X", "Y"),
    sex = c("f", "m", "m")
  ))
  plan <- plan_summary(
    data,
    group = treatment,
    strata = c(centre, sex),
    overall = c("strata", "group")
  )

  expect_identical(plan$variables, character())
  expect_identical(plan$group, "v001")
  expect_identical(plan$strata, c("v002", "v003"))
  expect_identical(plan$overall, c("group", "strata"))
})

test_that("plan_summary() resolves renamed axes to stable identifiers", {
  data <- dplyr::rename(labelled_data(), treatment = sex)
  plan <- plan_summary(data, group = treatment)

  expect_identical(plan$group, "v002")
  expect_identical(plan$data, data)
})

test_that("plan_summary() requires an available axis for each overall", {
  data <- labelled_data()

  expect_error(
    plan_summary(data, overall = "group"),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    plan_summary(data, overall = "strata"),
    class = "bq_error_invalid_plan"
  )
})

test_that("plan_summary() validates the overall specification", {
  data <- labelled_data()

  for (overall in list(NA_character_, "centre", c("group", "group"), 1)) {
    expect_error(
      plan_summary(data, group = sex, overall = overall),
      class = "bq_error_invalid_plan"
    )
  }
})

test_that("plan_summary() keeps group and strata separate", {
  data <- labelled_data()

  expect_error(
    plan_summary(data, group = sex, strata = sex),
    class = "bq_error_invalid_plan"
  )
  expect_error(
    plan_summary(data, group = sex, strata = sex),
    "both `group` and `strata`"
  )
})

test_that("plan_summary() validates data and axis cardinality", {
  data <- labelled_data()

  expect_error(
    plan_summary(tibble::tibble(age = 1)),
    class = "bq_error_invalid_data"
  )
  expect_error(
    plan_summary(data, group = missing),
    class = "bq_error_invalid_selection"
  )
  expect_error(
    plan_summary(data, group = c(sex, bmi)),
    class = "bq_error_invalid_selection"
  )
  expect_error(
    plan_summary(data, group = c(sex, bmi)),
    "at most 1 column"
  )
})

test_that("a summary design prints without printing data", {
  data <- as_bq_data(tibble::tibble(
    treatment = c("A", "B", "A"),
    centre = c("X", "X", "Y")
  ))
  plan <- plan_summary(
    data,
    group = treatment,
    strata = centre,
    overall = c("group", "strata")
  )

  output <- capture.output(visibility <- withVisible(print(plan)))

  expect_identical(
    output,
    c(
      "<bq summary plan>",
      "Variables: none",
      "Group: treatment",
      "Strata: centre",
      "Overall: group, strata",
      "Statistics: none"
    )
  )
  expect_false(visibility$visible)
  expect_identical(visibility$value, plan)
})

test_that("a configured summary plan prints statistic assignments", {
  robust <- continuous_statistic(
    "robust",
    function(x) data.frame(median = NA_real_, observed = NA_integer_)
  )
  average <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- labelled_data() |>
    plan_summary() |>
    add_statistic(c(age, bmi), robust) |>
    add_statistic(age, average)

  output <- capture.output(print(plan))

  expect_identical(
    output,
    c(
      "<bq summary plan>",
      "Variables: age, bmi",
      "Group: none",
      "Strata: none",
      "Overall: none",
      "Statistics:",
      "  robust: median, observed -> age, bmi",
      "  average: mean -> age"
    )
  )
})
