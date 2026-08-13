test_that("plan_descriptives compiles one inspectable task per variable", {
  data <- as_bq_data(tibble::tibble(
    age = c(40, 50, 60),
    sex = c("F", "M", "F"),
    arm = c("A", "A", "B")
  )) |>
    set_predictor(age, type = "continuous") |>
    set_predictor(sex, type = "nominal", reference = "F") |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_descriptive_statistics(age, "{mean} ({sd})") |>
    set_descriptive_statistics(sex, "{n} ({p}%)")

  plan <- plan_descriptives(
    data,
    variables = c(age, sex),
    groups = arm,
    overall = TRUE
  )

  expect_s3_class(plan, "analysis_plan")
  expect_identical(plan$analysis_type, rep("descriptive", 2))
  expect_identical(plan$variable, c("age", "sex"))
  expect_identical(plan$group, c("arm", "arm"))
  expect_true(all(plan$overall))
  expect_identical(
    plan$descriptive_templates,
    list("{mean} ({sd})", "{n} ({p}%)")
  )
})

test_that("descriptives compute continuous statistics overall and by group", {
  data <- as_bq_data(tibble::tibble(
    age = c(10, 20, NA, 40),
    arm = c("A", "A", "B", "B")
  )) |>
    set_predictor(age, type = "continuous") |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_descriptive_statistics(
      age,
      c("{mean} ({sd})", "{median} ({q1}; {q3})", "{min}; {max}")
    )

  plan <- data |>
    plan_descriptives(age, groups = arm, overall = TRUE) |>
    validate_plan(data)
  result <- run_analysis(plan, data)
  out <- descriptives(result)

  expect_true(all(c(
    "n", "n_missing", "mean", "sd", "median", "q1", "q3", "min", "max"
  ) %in% out$statistic))
  overall <- out[out$overall, ]
  expect_equal(overall$value[overall$statistic == "mean"], 70 / 3)
  expect_identical(overall$numerator[overall$statistic == "n"], 3L)
  expect_identical(overall$denominator[overall$statistic == "n"], 4L)
  group_b <- out[!out$overall & out$group_level == "B" & out$statistic == "mean", ]
  expect_equal(group_b$value, 40)
  expect_identical(group_b$denominator, 2L)
})

test_that("categorical descriptives use an explicit within-column denominator", {
  data <- as_bq_data(tibble::tibble(
    response = c("Yes", "No", "Yes", NA),
    arm = c("A", "A", "B", "B")
  )) |>
    set_predictor(response, type = "nominal", reference = "No") |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_descriptive_statistics(response, "{n}/{N} ({p}%)")

  result <- data |>
    plan_descriptives(response, groups = arm, overall = TRUE) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- descriptives(result)

  overall_yes <- out[
    out$overall & out$level == "Yes" & out$statistic == "p",
  ]
  expect_equal(overall_yes$value, 2 / 3)
  expect_identical(overall_yes$numerator, 2L)
  expect_identical(overall_yes$denominator, 3L)
  overall_yes_n <- out[
    out$overall & out$level == "Yes" & out$statistic == "N",
  ]
  expect_equal(overall_yes_n$value, 3)
  expect_identical(overall_yes_n$numerator, 2L)
  expect_identical(overall_yes_n$denominator, 3L)

  group_b_yes <- out[
    !out$overall & out$group_level == "B" & out$level == "Yes" &
      out$statistic == "p",
  ]
  expect_equal(group_b_yes$value, 1)
  expect_identical(group_b_yes$denominator, 1L)
})

test_that("categorical templates expose n, N, and p with stable semantics", {
  data <- as_bq_data(tibble::tibble(
    response = factor(c("Yes", "No", "Yes", NA), levels = c("No", "Yes"))
  )) |>
    set_predictor(response, type = "nominal", reference = "No") |>
    set_descriptive_statistics(response, "{n}/{N} ({p}%)")

  plan <- validate_plan(plan_descriptives(data, response), data)
  expect_identical(plan$status, "ready")
  expect_identical(plan$requested_statistics[[1]], c("n", "N", "p"))

  out <- descriptives(run_analysis(plan, data))
  yes <- out[out$level == "Yes" & !is.na(out$level), ]

  expect_equal(yes$value[yes$statistic == "n"], 2)
  expect_equal(yes$value[yes$statistic == "N"], 3)
  expect_equal(yes$value[yes$statistic == "p"], 2 / 3)
})

test_that("descriptives can analyze the complete data without groups", {
  data <- as_bq_data(tibble::tibble(age = c(10, 20, 30))) |>
    set_predictor(age, type = "continuous") |>
    set_descriptive_statistics(age, "{mean} ({sd})")

  plan <- plan_descriptives(data, age)
  expect_true(plan$overall)
  expect_true(is.na(plan$group))

  result <- run_analysis(validate_plan(plan, data), data)
  expect_true(all(descriptives(result)$overall))
})

test_that("grouped descriptives can omit the overall population", {
  data <- as_bq_data(tibble::tibble(
    age = c(10, 20, 30), arm = c("A", "A", "B")
  )) |>
    set_predictor(age, type = "continuous") |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_descriptive_statistics(age, "{mean} ({sd})")

  result <- data |>
    plan_descriptives(age, groups = arm, overall = FALSE) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_false(any(descriptives(result)$overall))
  expect_setequal(descriptives(result)$group_level, c("A", "B"))
})

test_that("descriptive preflight rejects unsupported model-based placeholders", {
  data <- as_bq_data(tibble::tibble(response = c(0, 1, 1))) |>
    set_predictor(response, type = "binary", reference = 0) |>
    set_descriptive_statistics(
      response,
      "{estimate} ({conf.low}; {conf.high})"
    )

  plan <- validate_plan(plan_descriptives(data, response), data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "model-based", ignore.case = TRUE)
})

test_that("descriptive plans validate selectors and configuration", {
  data <- as_bq_data(tibble::tibble(age = 1:3, arm = letters[1:3])) |>
    set_predictor(age, type = "continuous") |>
    set_predictor(arm, type = "nominal", reference = "a")

  expect_error(
    plan_descriptives(data, age, groups = c(age, arm)),
    class = "bq_error_invalid_descriptive_plan"
  )
  expect_error(
    plan_descriptives(data, age, overall = FALSE),
    class = "bq_error_invalid_descriptive_plan"
  )
})

test_that("descriptives treat labelled special values as missing", {
  age <- labelled::labelled(c(10, 20, 999))
  labelled::na_values(age) <- 999
  data <- as_bq_data(tibble::tibble(age = age)) |>
    set_predictor(age, type = "continuous") |>
    set_descriptive_statistics(age, "{mean} ({sd})")

  out <- data |>
    plan_descriptives(age) |>
    validate_plan(data) |>
    run_analysis(data) |>
    descriptives()

  expect_equal(out$value[out$statistic == "mean"], 15)
  expect_equal(out$value[out$statistic == "n_missing"], 1)
})

test_that("continuous descriptives compute robust and shape statistics", {
  data <- as_bq_data(tibble::tibble(x = 1:5)) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(
      x,
      c("{median} ({mad})", "{skewness}; {kurtosis}")
    )

  out <- data |>
    plan_descriptives(x) |>
    validate_plan(data) |>
    run_analysis(data) |>
    descriptives()

  expect_equal(out$value[out$statistic == "mad"], stats::mad(1:5))
  expect_equal(out$value[out$statistic == "skewness"], 0, tolerance = 1e-14)
  expect_equal(out$value[out$statistic == "kurtosis"], -1.2)
  expect_identical(
    out$statistic_method[out$statistic == "skewness"],
    "adjusted_fisher_pearson"
  )
  expect_identical(
    out$statistic_method[out$statistic == "kurtosis"],
    "bias_corrected_excess"
  )
})

test_that("shape statistics are undefined for small or constant samples", {
  small <- as_bq_data(tibble::tibble(x = c(1, 2, 3))) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(x, "{skewness}; {kurtosis}")
  small_out <- small |>
    plan_descriptives(x) |>
    validate_plan(small) |>
    run_analysis(small) |>
    descriptives()

  expect_equal(small_out$value[small_out$statistic == "skewness"], 0)
  expect_true(is.na(small_out$value[small_out$statistic == "kurtosis"]))

  constant <- as_bq_data(tibble::tibble(x = rep(1, 5))) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(x, "{skewness}; {kurtosis}")
  constant_out <- constant |>
    plan_descriptives(x) |>
    validate_plan(constant) |>
    run_analysis(constant) |>
    descriptives()

  expect_true(all(is.na(
    constant_out$value[constant_out$statistic %in% c("skewness", "kurtosis")]
  )))
})
