test_that("set_outcome assigns role and explicit analytical type", {
  x <- as_bq_data(tibble::tibble(bmi = 1:3, crp = 4:6))

  out <- set_outcome(x, c(bmi, crp), type = "continuous")
  registry <- variables(out)

  expect_identical(registry$role, list("outcome", "outcome"))
  expect_identical(registry$type, c("continuous", "continuous"))
  expect_identical(registry$source, c("explicit", "explicit"))
  expect_identical(registry$status, c("valid", "valid"))
  expect_identical(registry$locked, c(TRUE, TRUE))
})

test_that("set_outcome preserves inferred type when type is omitted", {
  x <- as_bq_data(tibble::tibble(response = c("No", "Yes", "No")))

  out <- set_outcome(x, response)
  registry <- variables(out)

  expect_identical(registry$type, "binary")
  expect_identical(registry$source, "inferred")
  expect_identical(registry$status, "review")
  expect_identical(registry$locked, FALSE)
})

test_that("set_outcome stores event values without changing their type", {
  x <- as_bq_data(tibble::tibble(response = c(0, 1, 0)))

  out <- set_outcome(x, response, type = "binary", event = 1)

  expect_identical(variables(out)$event_value, list(1))
})

test_that("set_predictor can add a role and categorical reference", {
  x <- as_bq_data(tibble::tibble(treatment = c("Placebo", "Drug", "Placebo"))) |>
    set_role(treatment, "group")

  out <- set_predictor(
    x,
    treatment,
    type = "nominal",
    reference = "Placebo"
  )
  registry <- variables(out)

  expect_identical(registry$role, list(c("group", "predictor")))
  expect_identical(registry$reference, list("Placebo"))
  expect_identical(registry$status, "valid")
})

test_that("categorical colors accept unnamed, named, and functional palettes", {
  x <- as_bq_data(tibble::tibble(
    arm = factor(c("Control", "Drug", "Control"), levels = c("Control", "Drug")),
    response = factor(c("No", "Yes", "Yes"))
  )) |>
    set_predictor(arm, type = "nominal") |>
    set_outcome(response, type = "binary", event = "Yes")

  unnamed <- x |> set_colors(arm, c("#111111", "#22AA22"))
  named <- x |> set_colors(arm, c(Drug = "red", Control = "blue"))
  functional <- x |> set_colors(response, function(levels) {
    stats::setNames(c("grey70", "navy"), levels)
  })

  expect_identical(variable_colors(unnamed, arm),
    c(Control = "#111111", Drug = "#22AA22"))
  expect_identical(variable_colors(named, arm),
    c(Control = "blue", Drug = "red"))
  expect_identical(variable_colors(functional, response),
    c(No = "grey70", Yes = "navy"))
  expect_true(is.function(variables(functional)$colors[[2]]$input))
})

test_that("categorical color contracts reject invalid mappings", {
  x <- as_bq_data(tibble::tibble(arm = factor(c("A", "B")))) |>
    set_predictor(arm, type = "nominal")
  expect_error(set_colors(x, arm, "red"), class = "bq_error_invalid_colors")
  expect_error(set_colors(x, arm, c(A = "red", C = "blue")),
    class = "bq_error_invalid_colors")
  expect_error(set_colors(x, arm, function(levels) 1:2),
    class = "bq_error_invalid_colors")
  expect_error(set_colors(x, arm, c("red", "not_a_color")),
    class = "bq_error_invalid_colors")
})

test_that("variable settings support tidyselect expressions", {
  x <- as_bq_data(tibble::tibble(y1 = 1:3, y2 = 2:4, note = letters[1:3]))

  out <- set_outcome(x, starts_with("y"), type = "continuous")

  expect_identical(
    variables(out)$role,
    list("outcome", "outcome", "auxiliary")
  )
})

test_that("variable settings reject invalid semantic combinations", {
  x <- as_bq_data(tibble::tibble(x = 1:3))

  expect_error(
    set_outcome(x, x, type = "unsupported"),
    class = "bq_error_invalid_variable_type"
  )
  expect_error(
    set_outcome(x, x, type = "continuous", event = 1),
    class = "bq_error_invalid_outcome"
  )
  expect_error(
    set_predictor(x, x, type = "continuous", reference = 1),
    class = "bq_error_invalid_predictor"
  )
})

test_that("descriptive statistics store one or more display templates", {
  x <- as_bq_data(tibble::tibble(
    age = c(40, 50, 60),
    biomarker = c(1.2, 2.3, 3.4)
  ))

  out <- x |>
    set_descriptive_statistics(age, "{mean} ({sd})") |>
    set_descriptive_statistics(
      biomarker,
      c("{mean} ({sd})", "{median} ({q1}; {q3})")
    )

  expect_identical(
    variables(out)$descriptive_templates,
    list(
      "{mean} ({sd})",
      c("{mean} ({sd})", "{median} ({q1}; {q3})")
    )
  )
})

test_that("descriptive templates can request model-based results", {
  x <- as_bq_data(tibble::tibble(response = c(0, 1, 1)))

  out <- set_descriptive_statistics(
    x,
    response,
    c("{n} ({p}%)", "{estimate} ({conf.low}; {conf.high})")
  )

  expect_identical(
    variables(out)$descriptive_templates[[1]],
    c("{n} ({p}%)", "{estimate} ({conf.low}; {conf.high})")
  )
})

test_that("descriptive statistics support tidyselect and explicit clearing", {
  x <- as_bq_data(tibble::tibble(x1 = 1:3, x2 = 2:4, group = letters[1:3])) |>
    set_descriptive_statistics(starts_with("x"), "{median} ({min}; {max})")

  out <- set_descriptive_statistics(x, x2, NULL)

  expect_identical(
    variables(out)$descriptive_templates,
    list("{median} ({min}; {max})", NULL, NULL)
  )
})

test_that("descriptive statistics reject malformed templates", {
  x <- as_bq_data(tibble::tibble(age = 1:3))

  expect_error(
    set_descriptive_statistics(x, age, character()),
    class = "bq_error_invalid_descriptive_statistics"
  )
  expect_error(
    set_descriptive_statistics(x, age, c("{mean}", NA_character_)),
    class = "bq_error_invalid_descriptive_statistics"
  )
  expect_error(
    set_descriptive_statistics(x, age, ""),
    class = "bq_error_invalid_descriptive_statistics"
  )
  expect_error(
    set_descriptive_statistics(x, age, "{conf..low}"),
    class = "bq_error_invalid_descriptive_statistics"
  )
})
