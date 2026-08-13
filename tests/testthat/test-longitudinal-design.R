test_that("long longitudinal designs use stable variable identifiers", {
  data <- as_bq_data(tibble::tibble(
    patient = c(1, 1, 2, 2), visit = c("V0", "V1", "V0", "V1"),
    arm = c("A", "A", "B", "B"), value = 1:4
  )) |>
    set_longitudinal_design(
      id = patient, time = visit, group = arm, layout = "long",
      baseline = "V0", time_scale = "categorical"
    )

  design <- designs(data)
  expect_identical(design$layout, "long")
  expect_identical(design$id, "patient")
  expect_identical(design$time, "visit")
  expect_identical(design$group, "arm")
  expect_identical(design$baseline[[1]], "V0")

  renamed <- dplyr::rename(data, subject = patient, assessment = visit)
  expect_identical(designs(renamed)$id, "subject")
  expect_identical(designs(renamed)$time, "assessment")
})

test_that("long designs validate duplicate id-time observations", {
  data <- as_bq_data(tibble::tibble(
    id = c(1, 1), visit = c("V0", "V0"), value = c(1, 2)
  )) |>
    set_longitudinal_design(
      id, visit, layout = "long", baseline = "V0"
    )

  expect_identical(designs(data)$status, "invalid")
  expect_match(designs(data)$reason, "duplicate", ignore.case = TRUE)
})

test_that("wide longitudinal outcomes require explicit column-time mappings", {
  data <- as_bq_data(tibble::tibble(
    patient = 1:3, arm = c("A", "B", "A"),
    bmi_0 = c(20, 21, 22), bmi_1 = c(19, 22, 21), bmi_2 = c(18, 23, 20)
  )) |>
    set_longitudinal_design(id = patient, group = arm, layout = "wide") |>
    add_longitudinal_outcome(
      bmi, values = c(bmi_0, bmi_1, bmi_2),
      time = c("V0", "V1", "V2"), baseline = "V0",
      time_scale = "categorical", type = "continuous"
    )

  outcome <- outcomes(data)
  expect_identical(outcome$type, "longitudinal")
  expect_identical(outcome$values[[1]], c("bmi_0", "bmi_1", "bmi_2"))
  expect_identical(outcome$time_values[[1]], c("V0", "V1", "V2"))
  expect_identical(outcome$status, "valid")
})

test_that("wide mappings validate lengths, times, and baseline", {
  data <- as_bq_data(tibble::tibble(id = 1:2, y0 = 1:2, y1 = 2:3)) |>
    set_longitudinal_design(id = id, layout = "wide")

  expect_error(
    add_longitudinal_outcome(data, y, c(y0, y1), "V0", baseline = "V0"),
    class = "bq_error_invalid_outcome"
  )
  expect_error(
    add_longitudinal_outcome(
      data, y, c(y0, y1), c("V0", "V0"), baseline = "V0"
    ),
    class = "bq_error_invalid_outcome"
  )
  expect_error(
    add_longitudinal_outcome(
      data, y, c(y0, y1), c("V0", "V1"), baseline = "V2"
    ),
    class = "bq_error_invalid_outcome"
  )
})

test_that("wide canonical frames preserve source data and explicit provenance", {
  data <- as_bq_data(tibble::tibble(
    patient = 1:2, arm = c("A", "B"), y0 = c(10, 20), y1 = c(11, 19)
  )) |>
    set_longitudinal_design(id = patient, group = arm, layout = "wide") |>
    add_longitudinal_outcome(
      score, c(y0, y1), c("Baseline", "Week 4"), baseline = "Baseline"
    )
  original <- tibble::as_tibble(data)
  frame <- bqreport:::build_longitudinal_frame(data, "score")

  expect_identical(tibble::as_tibble(data), original)
  expect_identical(
    names(frame), c("..bq_id", "..bq_time", "..bq_outcome", "..bq_group")
  )
  expect_equal(frame$..bq_outcome, c(10, 11, 20, 19))
  expect_identical(as.character(frame$..bq_time), rep(c("Baseline", "Week 4"), 2))
  expect_identical(attr(frame, "reshape_spec")$layout, "wide")
  expect_length(attr(frame, "reshape_spec")$value_var_ids, 2L)
})

test_that("removing a wide source column invalidates the resolved outcome", {
  data <- as_bq_data(tibble::tibble(id = 1:2, y0 = 1:2, y1 = 2:3)) |>
    set_longitudinal_design(id = id, layout = "wide") |>
    add_longitudinal_outcome(y, c(y0, y1), c("V0", "V1"), baseline = "V0") |>
    dplyr::select(-y1)

  expect_identical(outcomes(data)$status, "invalid")
  expect_match(outcomes(data)$reason, "source", ignore.case = TRUE)
})
