test_that("tbl_descriptive builds a backend-independent table model", {
  data <- as_bq_data(
    tibble::tibble(
      age = c(10, 20, 30, 40),
      arm = c("Control", "Control", "Treatment", "Treatment")
    ),
    metadata = tibble::tibble(
      name = "age", label = "Age", unit = "years", digits = 1L
    )
  ) |>
    set_predictor(age, type = "continuous") |>
    set_predictor(arm, type = "nominal", reference = "Control") |>
    set_descriptive_statistics(
      age,
      c("{mean} ({sd})", "{median} ({q1}; {q3})")
    )
  result <- data |>
    plan_descriptives(age, groups = arm, overall = TRUE) |>
    validate_plan(data) |>
    run_analysis(data)

  table <- tbl_descriptive(result)
  body <- table_body(table)
  header <- table_header(table)

  expect_s3_class(table, "bq_table")
  expect_s3_class(table, "tbl_descriptive")
  expect_identical(body$variable_label, c("Age", "Age"))
  expect_identical(body$unit, c("years", "years"))
  expect_identical(body$stat_1, c("25.0 (12.9)", "25.0 (17.5; 32.5)"))
  expect_identical(body$stat_2, c("15.0 (7.1)", "15.0 (12.5; 17.5)"))
  expect_identical(body$stat_3, c("35.0 (7.1)", "35.0 (32.5; 37.5)"))
  expect_identical(header$label, c("All patients", "Control", "Treatment"))
  expect_identical(header$column, c("stat_1", "stat_2", "stat_3"))
})

test_that("tbl_descriptive renders categorical levels using n, N, and percent", {
  data <- as_bq_data(tibble::tibble(
    response = factor(c("Yes", "No", "Yes", NA), levels = c("No", "Yes")),
    arm = c("A", "A", "B", "B")
  )) |>
    set_predictor(response, type = "nominal", reference = "No") |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_descriptive_statistics(response, "{n}/{N} ({p}%)")
  result <- data |>
    plan_descriptives(response, groups = arm, overall = TRUE) |>
    validate_plan(data) |>
    run_analysis(data)

  body <- table_body(tbl_descriptive(result, percent_digits = 1L))
  yes <- body[body$level == "Yes", ]

  expect_identical(yes$stat_1, "2/3 (66.7%)")
  expect_identical(yes$stat_2, "1/2 (50.0%)")
  expect_identical(yes$stat_3, "1/1 (100.0%)")
})

test_that("tbl_descriptive supports localized overall labels without data mutation", {
  data <- as_bq_data(tibble::tibble(age = 1:3)) |>
    set_predictor(age, type = "continuous") |>
    set_descriptive_statistics(age, "{mean} ({sd})")
  result <- data |>
    plan_descriptives(age) |>
    validate_plan(data) |>
    run_analysis(data)
  before <- descriptives(result)

  table <- tbl_descriptive(result, overall_label = "Все пациенты")

  expect_identical(table_header(table)$label, "Все пациенты")
  expect_identical(descriptives(result), before)
})

test_that("tbl_descriptive preserves non-computable provider messages", {
  data <- as_bq_data(tibble::tibble(x = c(1, 1, 1))) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(x, "p={shapiro.p.value}")
  result <- data |>
    plan_descriptives(x, functions = list(shapiro_wilk())) |>
    validate_plan(data) |>
    run_analysis(data)

  table <- tbl_descriptive(result, missing = "—")

  expect_identical(table_body(table)$stat_1, "p=—")
  expect_true(any(grepl(
    "constant", table$footnotes$message, ignore.case = TRUE
  )))
})

test_that("tbl_descriptive validates formatting arguments", {
  data <- as_bq_data(tibble::tibble(age = 1:3)) |>
    set_predictor(age, type = "continuous")
  result <- data |>
    plan_descriptives(age) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_error(
    tbl_descriptive(result, percent_digits = -1L),
    class = "bq_error_invalid_table"
  )
  expect_error(
    tbl_descriptive(result, overall_label = NA_character_),
    class = "bq_error_invalid_table"
  )
})
