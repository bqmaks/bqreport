test_that("add_survival_outcome registers a composite outcome by stable ids", {
  data <- as_bq_data(tibble::tibble(
    os_time = c(5, 10, 12), death = c(0, 1, 1), age = c(50, 60, 70)
  )) |>
    add_survival_outcome(
      overall_survival,
      time = os_time,
      event = death,
      event_value = 1,
      time_unit = "months"
    )

  registry <- outcomes(data)
  variables <- variables(data)

  expect_identical(registry$name, "overall_survival")
  expect_identical(registry$type, "survival")
  expect_identical(
    registry$time_var_id,
    variables$var_id[variables$name == "os_time"]
  )
  expect_identical(
    registry$event_var_id,
    variables$var_id[variables$name == "death"]
  )
  expect_identical(registry$event_value, list(1))
  expect_identical(registry$time_unit, "months")
  expect_true("outcome" %in% variables$role[[match("os_time", variables$name)]])
  expect_true("event" %in% variables$role[[match("death", variables$name)]])
})

test_that("survival outcomes resolve renamed component variables", {
  data <- as_bq_data(tibble::tibble(
    os_time = c(5, 10), death = c(0, 1)
  )) |>
    add_survival_outcome(
      overall_survival, os_time, death,
      event_value = 1, time_unit = "months"
    ) |>
    dplyr::rename(follow_up = os_time, status = death)

  registry <- outcomes(data)

  expect_identical(registry$time, "follow_up")
  expect_identical(registry$event, "status")
})

test_that("survival outcome becomes invalid when a component is removed", {
  data <- as_bq_data(tibble::tibble(
    os_time = c(5, 10), death = c(0, 1), age = c(50, 60)
  )) |>
    add_survival_outcome(
      overall_survival, os_time, death,
      event_value = 1, time_unit = "months"
    ) |>
    dplyr::select(os_time, age)

  registry <- outcomes(data)

  expect_identical(registry$status, "invalid")
  expect_match(registry$reason, "event", ignore.case = TRUE)
})

test_that("survival outcome validates its component contract", {
  data <- as_bq_data(tibble::tibble(
    os_time = c(5, 10), death = c(0, 1)
  ))

  expect_error(
    add_survival_outcome(
      data, overall_survival, os_time, death,
      event_value = c(0, 1), time_unit = "months"
    ),
    class = "bq_error_invalid_outcome"
  )
  expect_error(
    add_survival_outcome(
      data, overall_survival, os_time, os_time,
      event_value = 1, time_unit = "months"
    ),
    class = "bq_error_invalid_outcome"
  )
})
