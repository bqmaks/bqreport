compose_table_example <- function() {
  data <- as_bq_data(tibble::tibble(
    value = c(NA_real_, 1.2, 3),
    stratum = factor(c("A", "B", "B"), levels = c("A", "B", "C"))
  ))
  data <- set_type(data, value, type_continuous())
  data <- set_type(data, stratum, type_nominal("A"))
  data <- set_unit(data, value, "mg")
  data <- set_rounding(data, value, 1)
  data <- apply_dictionary(
    data,
    data.frame(name = "value", label = "Serum level")
  )
  statistic <- continuous_statistic(
    "summary",
    function(x) {
      data.frame(
        mean = if (length(x) == 0L || all(is.na(x))) {
          NA_real_
        } else {
          mean(x, na.rm = TRUE)
        },
        observed = sum(!is.na(x))
      )
    },
    scale = c(mean = "variable", observed = "count")
  )
  plan <- plan_summary(
    data,
    strata = stratum
  )
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(
    plan,
    value,
    enumerate_values(max_n = 2L, display_statistics = TRUE)
  )
  presentation <- prepare_presentation(run_analysis(plan))
  format_presentation(presentation, missing = "Missing", empty = "Empty")
}

test_that("compose_table() builds rows and visible body values", {
  formatted <- compose_table_example()

  table <- compose_table(formatted)

  expect_s3_class(
    table,
    c("bq_table_summary", "bq_table"),
    exact = TRUE
  )
  expect_identical(
    names(table),
    c(
      "analysis", "formatted", "rows", "columns", "column_axes",
      "cell_displays", "body"
    )
  )
  expect_identical(table$analysis, "summary")
  expect_identical(table$formatted, formatted)
  expect_identical(
    table$rows,
    tibble::tibble(
      row_id = c("r001", "r002", "r003"),
      var_id = rep("v001", 3),
      row_kind = c("statistic", "statistic", "enumeration"),
      statistic_id = c("s001", "s001", NA_character_),
      statistic_name = c("summary", "summary", NA_character_),
      component = c("mean", "observed", NA_character_),
      component_scale = c("variable", "count", NA_character_),
      row_label = rep(NA_character_, 3),
      template = rep(NA_character_, 3),
      variable_label = rep("Serum level", 3),
      unit = rep("mg", 3),
      position = 1:3
    )
  )
  expect_identical(
    table$body,
    tibble::tibble(
      row_id = c("r001", "r002", "r003"),
      cell_id = rep("c002", 3),
      value = c("2.1", "2", "1.2, 3.0")
    )
  )
})

test_that("compose_table() keeps statuses outside the body", {
  table <- compose_table(compose_table_example())

  expect_identical(
    table$cell_displays,
    tibble::tibble(
      cell_id = c("c001", "c002", "c003"),
      var_id = rep("v001", 3),
      status = c("all_missing", "observed", "empty"),
      show_statistics = rep(TRUE, 3),
      show_values = c(FALSE, TRUE, FALSE),
      rule_id = rep("r001", 3),
      status_text = c("Missing", NA_character_, "Empty")
    )
  )
  expect_false(any(table$body$cell_id %in% c("c001", "c003")))
})

test_that("compose_table() substitutes named summary formats", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2, 3)))
  data <- set_type(data, value, type_continuous())
  data <- set_rounding(data, value, 1)
  data <- set_summary_format(
    data,
    value,
    c(
      "Mean (SD)" = "{mean} ({sd})",
      "Median (Q1; Q3)" = "{median} ({q1}; {q3})"
    )
  )
  plan <- plan_summary(data) |>
    add_statistic(value)
  plan <- add_display_rule(
    plan,
    value,
    enumerate_values(max_n = 3L, display_statistics = TRUE)
  )
  formatted <- format_presentation(prepare_presentation(run_analysis(plan)))

  table <- compose_table(formatted)

  expect_identical(
    table$rows$row_kind,
    c("summary_format", "summary_format", "enumeration")
  )
  expect_identical(
    table$rows$row_label,
    c("Mean (SD)", "Median (Q1; Q3)", NA_character_)
  )
  expect_identical(
    table$rows$template,
    c(
      "{mean} ({sd})",
      "{median} ({q1}; {q3})",
      NA_character_
    )
  )
  expect_true(all(is.na(table$rows$component)))
  expect_identical(
    table$body$value,
    c("2.0 (1.0)", "2.0 (1.5; 2.5)", "1.0, 2.0, 3.0")
  )
})

test_that("compose_table() omits statistics replaced by enumeration", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  data <- set_type(data, value, type_continuous())
  data <- set_rounding(data, value, 0)
  data <- set_summary_format(data, value, c("Mean" = "{mean}"))
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = mean(x))
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(plan, value, enumerate_values(max_n = 2L))
  formatted <- format_presentation(prepare_presentation(run_analysis(plan)))

  table <- compose_table(formatted)

  expect_identical(table$rows$row_kind, "enumeration")
  expect_identical(table$body$value, "1, 2")
})

test_that("compose_table() retains an unnamed summary format", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  data <- set_type(data, value, type_continuous())
  data <- set_rounding(data, value, 0)
  data <- set_summary_format(data, value, "{mean}")
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = mean(x))
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  formatted <- format_presentation(prepare_presentation(run_analysis(plan)))

  table <- compose_table(formatted)

  expect_identical(table$rows$row_kind, "summary_format")
  expect_identical(table$rows$row_label, NA_character_)
  expect_identical(table$rows$template, "{mean}")
  expect_identical(table$body$value, "2")
})

test_that("compose_table() identifies group, strata and Overall headers", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2),
    treatment = factor(c("A", "B"), levels = c("A", "B")),
    centre = factor(c("X", "Y"), levels = c("X", "Y"))
  ))
  data <- set_type(data, value, type_continuous())
  data <- set_type(data, treatment, type_binary("B"))
  data <- set_type(data, centre, type_nominal("X"))
  data <- set_rounding(data, value, 0)
  statistic <- continuous_statistic(
    "mean",
    function(x) {
      data.frame(
        mean = if (length(x) == 0L) NA_real_ else mean(x, na.rm = TRUE)
      )
    }
  )
  plan <- plan_summary(
    data,
    group = treatment,
    strata = centre,
    overall = c("group", "strata")
  )
  plan <- add_statistic(plan, value, statistic)
  formatted <- format_presentation(prepare_presentation(run_analysis(plan)))

  table <- compose_table(formatted)

  expect_identical(table$columns$cell_id, sprintf("c%03d", 1:9))
  expect_identical(table$columns$position, 1:9)
  expect_identical(
    table$column_axes[1:2, ],
    tibble::tibble(
      cell_id = rep("c001", 2),
      var_id = c("v002", "v003"),
      axis = c("group", "strata"),
      axis_position = 1:2,
      value = c("A", "X"),
      is_overall = c(FALSE, FALSE)
    )
  )
  expect_identical(
    table$column_axes[17:18, ],
    tibble::tibble(
      cell_id = rep("c009", 2),
      var_id = c("v002", "v003"),
      axis = c("group", "strata"),
      axis_position = 1:2,
      value = c(NA_character_, NA_character_),
      is_overall = c(TRUE, TRUE)
    )
  )
})

test_that("compose_table() supports a table without design axes", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  data <- set_type(data, value, type_continuous())
  data <- set_rounding(data, value, 0)
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = mean(x))
  )
  plan <- add_statistic(
    plan_summary(data),
    value,
    statistic
  )
  formatted <- format_presentation(prepare_presentation(run_analysis(plan)))

  table <- compose_table(formatted)

  expect_identical(
    table$column_axes,
    tibble::tibble(
      cell_id = character(),
      var_id = character(),
      axis = character(),
      axis_position = integer(),
      value = character(),
      is_overall = logical()
    )
  )
})

test_that("compose_table() requires formatted presentation results", {
  expect_error(
    compose_table(compose_table_example()$presentation),
    class = "bq_error_invalid_presentation"
  )
})
