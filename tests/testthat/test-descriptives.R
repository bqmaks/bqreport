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

test_that("descriptive_function validates its public contract", {
  provider <- descriptive_function(
    id = "trimmed_mean",
    fields = "trimmed.mean",
    types = c("continuous", "count"),
    compute = function(context) {
      tibble::tibble(
        statistic = "trimmed.mean",
        value = mean(context$values, trim = 0.1),
        statistic_method = "mean_trim_0.1"
      )
    }
  )

  expect_s3_class(provider, "descriptive_function")
  expect_identical(provider$id, "trimmed_mean")
  expect_identical(provider$fields, "trimmed.mean")
  expect_match(provider$function_hash, "^[[:xdigit:]]+$")

  expect_error(
    descriptive_function("bad", character(), function(context) NULL),
    class = "bq_error_invalid_descriptive_function"
  )
  expect_error(
    descriptive_function("bad", c("x", "x"), function(context) NULL),
    class = "bq_error_invalid_descriptive_function"
  )
})

test_that("custom descriptive functions run once per requested population", {
  provider <- descriptive_function(
    id = "trimmed_mean",
    fields = "trimmed.mean",
    types = "continuous",
    compute = function(context) {
      tibble::tibble(
        statistic = "trimmed.mean",
        value = mean(context$values, trim = 0.1),
        statistic_method = "mean_trim_0.1"
      )
    }
  )
  data <- as_bq_data(tibble::tibble(
    x = c(1, 2, 100, 4), arm = c("A", "A", "B", "B")
  )) |>
    set_predictor(x, type = "continuous") |>
    set_predictor(arm, type = "nominal", reference = "A") |>
    set_descriptive_statistics(x, "{trimmed.mean}")

  plan <- data |>
    plan_descriptives(
      x,
      groups = arm,
      overall = TRUE,
      functions = list(provider)
    ) |>
    validate_plan(data)
  result <- run_analysis(plan, data)
  out <- descriptives(result)
  custom <- out[out$statistic == "trimmed.mean", ]

  expect_identical(plan$status, "ready")
  expect_equal(nrow(custom), 3L)
  expect_setequal(custom$group_level[!custom$overall], c("A", "B"))
  expect_true(all(custom$source == "custom"))
  expect_true(all(custom$method == "trimmed_mean"))
  expect_true(all(custom$status == "observed"))
  expect_true("trimmed_mean" %in% result$provenance$descriptive_function_ids[[1]])
})

test_that("custom descriptive output is validated strictly", {
  malformed <- descriptive_function(
    id = "malformed",
    fields = "custom.value",
    types = "continuous",
    compute = function(context) {
      tibble::tibble(statistic = "another.value", value = 1)
    }
  )
  data <- as_bq_data(tibble::tibble(x = 1:5)) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(x, "{custom.value}")
  result <- data |>
    plan_descriptives(x, functions = list(malformed)) |>
    validate_plan(data) |>
    run_analysis(data)

  expect_equal(nrow(descriptives(result)), 0L)
  expect_true(any(
    issues(result)$condition_class == "bq_error_invalid_descriptive_output"
  ))
})

test_that("descriptive providers must uniquely own their fields", {
  first <- descriptive_function(
    "first", "custom.value",
    function(context) tibble::tibble(
      statistic = "custom.value", value = 1,
      statistic_method = "first"
    )
  )
  second <- descriptive_function(
    "second", "custom.value",
    function(context) tibble::tibble(
      statistic = "custom.value", value = 2,
      statistic_method = "second"
    )
  )
  data <- as_bq_data(tibble::tibble(x = 1:5)) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(x, "{custom.value}")

  expect_error(
    plan_descriptives(data, x, functions = list(first, second)),
    class = "bq_error_duplicate_descriptive_field"
  )
})

test_that("Shapiro-Wilk provides diagnostic fields without changing metadata", {
  data <- as_bq_data(tibble::tibble(x = c(-1.2, -0.4, 0, 0.2, 0.9, 1.4))) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(
      x,
      "W={shapiro.statistic}, p={shapiro.p.value}"
    )
  direct <- stats::shapiro.test(data$x)

  result <- data |>
    plan_descriptives(x, functions = list(shapiro_wilk())) |>
    validate_plan(data) |>
    run_analysis(data)
  out <- descriptives(result)

  expect_equal(
    out$value[out$statistic == "shapiro.statistic"], unname(direct$statistic)
  )
  expect_equal(
    out$value[out$statistic == "shapiro.p.value"], direct$p.value
  )
  expect_true(all(out$source[out$method == "shapiro_wilk"] == "diagnostic"))
  expect_identical(variables(data)$distribution, NA_character_)
})

test_that("Shapiro-Wilk records non-computable populations without fallback", {
  data <- as_bq_data(tibble::tibble(x = c(1, 1, 1))) |>
    set_predictor(x, type = "continuous") |>
    set_descriptive_statistics(x, "{shapiro.p.value}")

  out <- data |>
    plan_descriptives(x, functions = list(shapiro_wilk())) |>
    validate_plan(data) |>
    run_analysis(data) |>
    descriptives()
  shapiro <- out[out$method == "shapiro_wilk", ]

  expect_true(all(is.na(shapiro$value)))
  expect_true(all(shapiro$status == "not_computed"))
  expect_true(all(grepl("constant", shapiro$message, ignore.case = TRUE)))
})

test_that("model-based fields are accepted only from an explicit provider", {
  provider <- descriptive_function(
    id = "explicit_estimate",
    fields = c("estimate", "conf.low", "conf.high"),
    types = "binary",
    source = "model",
    compute = function(context) {
      estimate <- mean(context$values)
      tibble::tibble(
        statistic = c("estimate", "conf.low", "conf.high"),
        value = c(estimate, estimate - 0.1, estimate + 0.1),
        statistic_method = "declared_demo_estimator"
      )
    }
  )
  data <- as_bq_data(tibble::tibble(response = c(0, 1, 1))) |>
    set_predictor(response, type = "binary", reference = 0) |>
    set_descriptive_statistics(
      response,
      "{estimate} ({conf.low}; {conf.high})"
    )

  plan <- validate_plan(
    plan_descriptives(data, response, functions = list(provider)), data
  )
  expect_identical(plan$status, "ready")
  out <- descriptives(run_analysis(plan, data))
  expect_true(all(out$source[out$method == "explicit_estimate"] == "model"))
})
