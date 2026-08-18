test_that("preflight() accepts a ready continuous summary plan", {
  plan <- plan_summary(labelled_data())
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, age, statistic)
  plan <- add_display_rule(plan, age, enumerate_values())

  result <- preflight(plan)

  expect_s3_class(
    result,
    c("bq_preflight_summary", "bq_preflight"),
    exact = TRUE
  )
  expect_identical(
    unclass(result),
    list(
      analysis = "summary",
      ok = TRUE,
      diagnostics = tibble::tibble(
        severity = character(),
        code = character(),
        var_id = character(),
        statistic_id = character(),
        component = character(),
        rule_id = character(),
        cell_id = character(),
        message = character()
      ),
      cells = tibble::tibble(
        cell_id = "c001",
        overall_group = FALSE,
        overall_strata = FALSE,
        n = 3L
      ),
      cell_axes = tibble::tibble(
        cell_id = character(),
        var_id = character(),
        value = character(),
        is_overall = logical()
      ),
      cell_rows = tibble::tibble(
        cell_id = rep("c001", 3),
        row = 1:3
      )
    )
  )
})

test_that("preflight() compiles design cells and reports empty leaf cells", {
  data <- as_bq_data(tibble::tibble(
    value = c(10, 20, 30),
    treatment = factor(c("A", "B", "A"), levels = c("A", "B")),
    centre = factor(c("X", "X", "Y"), levels = c("X", "Y"))
  ))
  data <- set_type(data, value, continuous())
  data <- set_type(data, treatment, binary("B"))
  data <- set_type(data, centre, nominal("X"))
  plan <- plan_summary(
    data,
    group = treatment,
    strata = centre,
    overall = c("group", "strata")
  )
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, value, statistic)

  result <- preflight(plan)
  compiled <- compile_summary_cells(plan)

  expect_true(result$ok)
  expect_identical(result[c("cells", "cell_axes", "cell_rows")], compiled)
  expect_identical(result$diagnostics$severity, "warning")
  expect_identical(result$diagnostics$code, "empty_cell")
  expect_identical(result$diagnostics$cell_id, "c004")
})

test_that("preflight() reports missing design values without dropping rows", {
  data <- as_bq_data(tibble::tibble(
    value = c(10, 20),
    treatment = c("A", NA_character_)
  ))
  data <- set_type(data, value, continuous())
  data <- set_type(data, treatment, binary("A"))
  plan <- plan_summary(
    data,
    group = treatment
  )
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, value, statistic)

  result <- preflight(plan)

  expect_true(result$ok)
  expect_identical(result$diagnostics$severity, "warning")
  expect_identical(result$diagnostics$code, "missing_design_value")
  expect_identical(result$diagnostics$var_id, "v002")
  expect_identical(result$cells$n, c(1L, 1L))
  expect_identical(result$cell_rows$row, c(1L, 2L))
})

test_that("preflight() reports missing and unknown analytic types", {
  missing_data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  missing_plan <- plan_summary(missing_data)
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  missing_plan <- add_statistic(missing_plan, value, statistic)
  missing_plan <- add_display_rule(
    missing_plan,
    value,
    enumerate_values()
  )

  unknown_data <- apply_dictionary(
    missing_data,
    tibble::tibble(name = "value", type = "unknown")
  )
  unknown_plan <- plan_summary(unknown_data)
  unknown_plan <- add_statistic(unknown_plan, value, statistic)
  unknown_plan <- add_display_rule(
    unknown_plan,
    value,
    enumerate_values()
  )

  missing_result <- preflight(missing_plan)
  unknown_result <- preflight(unknown_plan)

  expect_false(missing_result$ok)
  expect_identical(missing_result$diagnostics$code, "missing_type")
  expect_identical(missing_result$diagnostics$var_id, "v001")
  expect_false(unknown_result$ok)
  expect_identical(unknown_result$diagnostics$code, "unknown_type")
})

test_that("preflight() validates continuous model-frame coercion", {
  valid_data <- as_bq_data(tibble::tibble(value = c("1.5", "2.5")))
  valid_data <- set_type(valid_data, value, continuous())
  valid_plan <- plan_summary(valid_data) |>
    add_statistic(value)

  invalid_data <- as_bq_data(tibble::tibble(value = c("1.5", "unknown")))
  invalid_data <- set_type(invalid_data, value, continuous())
  invalid_plan <- plan_summary(invalid_data) |>
    add_statistic(value)

  expect_true(preflight(valid_plan)$ok)
  invalid_result <- preflight(invalid_plan)
  expect_false(invalid_result$ok)
  expect_identical(
    invalid_result$diagnostics$code,
    "invalid_continuous_storage"
  )
  expect_identical(invalid_result$diagnostics$var_id, "v001")
})

test_that("preflight() checks types of design axes", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2),
    group = c("a", "b")
  ))
  data <- set_type(data, value, continuous())
  plan <- plan_summary(
    data,
    group = group
  )
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, value, statistic)

  result <- preflight(plan)

  expect_false(result$ok)
  expect_identical(result$diagnostics$code, "missing_type")
  expect_identical(result$diagnostics$var_id, "v002")
})

test_that("preflight() diagnoses non-atomic design storage before compilation", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2),
    group = list("a", "b")
  ))
  data <- set_type(data, value, continuous())
  data <- set_type(data, group, binary("b"))
  plan <- plan_summary(data, group = group) |>
    add_statistic(value)

  result <- preflight(plan)

  expect_false(result$ok)
  expect_identical(result$diagnostics$code, "invalid_design_storage")
  expect_identical(result$diagnostics$var_id, "v002")
  expect_identical(nrow(result$cells), 0L)
})

test_that("preflight() rejects values outside declared design domains", {
  binary_data <- as_bq_data(tibble::tibble(
    value = c(1, 2),
    group = c("a", "b")
  ))
  binary_data <- set_type(binary_data, value, continuous())
  binary_data <- set_type(binary_data, group, binary("b"))
  binary_data <- dplyr::bind_rows(
    binary_data,
    tibble::tibble(value = 3, group = "c")
  )
  binary_plan <- plan_summary(binary_data, group = group) |>
    add_statistic(value)

  ordinal_data <- as_bq_data(tibble::tibble(
    value = c(1, 2),
    severity = c("low", "high")
  ))
  ordinal_data <- set_type(ordinal_data, value, continuous())
  ordinal_data <- set_type(
    ordinal_data,
    severity,
    ordinal(c("low", "medium", "high"))
  )
  ordinal_data <- dplyr::bind_rows(
    ordinal_data,
    tibble::tibble(value = 3, severity = "severe")
  )
  ordinal_plan <- plan_summary(ordinal_data, group = severity) |>
    add_statistic(value)

  for (plan in list(binary_plan, ordinal_plan)) {
    result <- preflight(plan)

    expect_false(result$ok)
    expect_identical(result$diagnostics$code, "invalid_design_domain")
    expect_identical(result$diagnostics$var_id, "v002")
    expect_identical(nrow(result$cells), 0L)
  }
})

test_that("preflight() restricts units and rounding to quantitative variables", {
  data <- set_unit(labelled_data(), sex, "kg")
  data <- set_rounding(data, sex, 1)
  plan <- plan_summary(
    data,
    group = sex
  )
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, age, statistic)

  result <- preflight(plan)

  expect_false(result$ok)
  expect_identical(
    result$diagnostics$code,
    c("incompatible_unit", "incompatible_rounding")
  )
  expect_identical(result$diagnostics$var_id, c("v002", "v002"))
})

test_that("preflight() requires at least one summary variable", {
  result <- preflight(plan_summary(labelled_data()))

  expect_false(result$ok)
  expect_identical(result$diagnostics$code, "missing_summary_variable")
  expect_identical(result$diagnostics$var_id, NA_character_)
})

test_that("preflight() reports incompatible continuous statistics", {
  data <- set_type(labelled_data(), sex, binary("m"))
  plan <- plan_summary(data)
  statistic <- continuous_statistic(
    "average",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- add_statistic(plan, sex, statistic)

  result <- preflight(plan)

  expect_false(result$ok)
  expect_identical(result$diagnostics$code, "incompatible_statistic")
  expect_identical(result$diagnostics$var_id, "v002")
  expect_identical(result$diagnostics$statistic_id, "s001")
})

test_that("preflight() reports incompatible display rules", {
  data <- set_type(labelled_data(), sex, binary("m"))
  plan <- plan_summary(data) |>
    add_statistic(sex)
  plan <- add_display_rule(plan, sex, enumerate_values())

  result <- preflight(plan)
  diagnostic <- result$diagnostics[
    result$diagnostics$code == "incompatible_display_rule",
  ]

  expect_false(result$ok)
  expect_identical(diagnostic$severity, "error")
  expect_identical(diagnostic$var_id, "v002")
  expect_identical(diagnostic$statistic_id, NA_character_)
  expect_identical(diagnostic$rule_id, "r001")
})

test_that("preflight() requires rounding for dimensionless components", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  data <- set_type(data, value, continuous())
  statistic <- continuous_statistic(
    "ratio",
    function(x) data.frame(ratio = NA_real_),
    scale = "dimensionless"
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)

  missing_result <- preflight(plan)

  expect_false(missing_result$ok)
  expect_identical(
    missing_result$diagnostics$code,
    "missing_component_rounding"
  )
  expect_identical(missing_result$diagnostics$statistic_id, "s001")
  expect_identical(missing_result$diagnostics$component, "ratio")

  statistic <- set_component_rounding(
    statistic,
    "ratio",
    3,
    "significant"
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)

  expect_true(preflight(plan)$ok)
})

test_that("preflight() accepts unique summary format components", {
  data <- set_summary_format(
    labelled_data(),
    age,
    c(
      "Mean (SD)" = "{mean} ({sd})",
      "Median (Q1; Q3)" = "{median} ({q1}; {q3})"
    )
  )
  plan <- plan_summary(data) |>
    add_statistic(age)

  result <- preflight(plan)

  expect_true(result$ok)
  expect_identical(nrow(result$diagnostics), 0L)
})

test_that("preflight() reports unknown summary format components once", {
  data <- set_summary_format(
    labelled_data(),
    age,
    c("Mean" = "{average} / {average}")
  )
  plan <- plan_summary(data) |>
    add_statistic(age)

  result <- preflight(plan)
  diagnostic <- result$diagnostics[
    result$diagnostics$code == "unknown_summary_component",
  ]

  expect_false(result$ok)
  expect_identical(nrow(diagnostic), 1L)
  expect_identical(diagnostic$severity, "error")
  expect_identical(diagnostic$var_id, "v001")
  expect_identical(diagnostic$statistic_id, NA_character_)
  expect_identical(diagnostic$component, "average")
  expect_match(diagnostic$message, "Summary format `Mean`")
})

test_that("unassigned summary formats do not duplicate an empty-plan diagnostic", {
  data <- set_summary_format(labelled_data(), age, "{mean}")

  result <- preflight(plan_summary(data))

  expect_false(result$ok)
  expect_identical(result$diagnostics$code, "missing_summary_variable")
})

test_that("preflight() reports ambiguous summary format components", {
  data <- set_summary_format(labelled_data(), age, "{mean}")
  first <- continuous_statistic(
    "first",
    function(x) data.frame(mean = NA_real_)
  )
  second <- continuous_statistic(
    "second",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, age, first)
  plan <- add_statistic(plan, age, second)

  result <- preflight(plan)
  diagnostic <- result$diagnostics[
    result$diagnostics$code == "ambiguous_summary_component",
  ]

  expect_false(result$ok)
  expect_identical(nrow(diagnostic), 1L)
  expect_identical(diagnostic$var_id, "v001")
  expect_identical(diagnostic$component, "mean")
  expect_match(diagnostic$message, "first, second")
})

test_that("preflight() rejects damaged summary format registries", {
  data <- set_summary_format(labelled_data(), age, "{mean}")
  plan <- plan_summary(data) |>
    add_statistic(age)
  formats <- attr(plan$data, "summary_formats")
  formats$position <- 2L
  attr(plan$data, "summary_formats") <- formats

  expect_error(preflight(plan), class = "bq_error_invalid_plan")
})

test_that("preflight() rejects non-plans and damaged plan structures", {
  expect_error(
    preflight(labelled_data()),
    class = "bq_error_invalid_plan"
  )

  plan <- plan_summary(labelled_data()) |>
    add_statistic(age)
  plan$statistics <- plan$statistics[, -1]

  expect_error(preflight(plan), class = "bq_error_invalid_plan")

  plan <- plan_summary(labelled_data()) |>
    add_statistic(age)
  plan$display_rules <- plan$display_rules[, -1]

  expect_error(preflight(plan), class = "bq_error_invalid_plan")

  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = NA_real_)
  )
  plan <- plan_summary(labelled_data())
  plan <- add_statistic(plan, age, statistic)
  plan$statistic_components$scale <- "distance"

  expect_error(preflight(plan), class = "bq_error_invalid_plan")
})
