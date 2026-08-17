test_that("compile_summary_cells() builds leaf and every requested Overall", {
  data <- as_bq_data(tibble::tibble(
    value = c(10, 20, 30),
    treatment = factor(c("A", "B", "A"), levels = c("A", "B")),
    centre = factor(c("X", "X", "Y"), levels = c("X", "Y"))
  ))
  plan <- plan_summary(
    data,
    group = treatment,
    strata = centre,
    overall = c("group", "strata")
  )

  compiled <- compile_summary_cells(plan)

  expect_identical(
    compiled$cells,
    tibble::tibble(
      cell_id = sprintf("c%03d", 1:9),
      overall_group = c(rep(FALSE, 4), rep(TRUE, 2), rep(FALSE, 2), TRUE),
      overall_strata = c(rep(FALSE, 6), rep(TRUE, 3)),
      n = c(1L, 1L, 1L, 0L, 2L, 1L, 2L, 1L, 3L)
    )
  )
  expect_identical(
    compiled$cell_axes[1:8, ],
    tibble::tibble(
      cell_id = rep(sprintf("c%03d", 1:4), each = 2),
      var_id = rep(c("v002", "v003"), 4),
      value = c("A", "X", "B", "X", "A", "Y", "B", "Y"),
      is_overall = FALSE
    )
  )
  expect_identical(
    compiled$cell_axes[9:12, ],
    tibble::tibble(
      cell_id = rep(c("c005", "c006"), each = 2),
      var_id = rep(c("v002", "v003"), 2),
      value = c(NA, "X", NA, "Y"),
      is_overall = rep(c(TRUE, FALSE), 2)
    )
  )
  expect_false("c004" %in% compiled$cell_rows$cell_id)
  expect_identical(sum(compiled$cell_rows$cell_id == "c009"), 3L)
})

test_that("compile_summary_cells() retains missing design values as cells", {
  data <- as_bq_data(tibble::tibble(
    value = c(10, 20),
    treatment = c("A", NA_character_)
  ))
  plan <- plan_summary(data, group = treatment)

  compiled <- compile_summary_cells(plan)

  expect_identical(compiled$cells$n, c(1L, 1L))
  expect_identical(compiled$cell_axes$value, c("A", NA_character_))
  expect_identical(compiled$cell_rows$row, c(1L, 2L))
})

test_that("strata Overall collapses multiple strata variables together", {
  data <- as_bq_data(tibble::tibble(
    value = c(10, 20),
    centre = factor(c("X", "Y"), levels = c("X", "Y")),
    sex = factor(c("F", "F"), levels = c("F", "M"))
  ))
  plan <- plan_summary(
    data,
    strata = c(centre, sex),
    overall = "strata"
  )

  compiled <- compile_summary_cells(plan)

  expect_identical(compiled$cells$n, c(1L, 1L, 0L, 0L, 2L))
  expect_identical(compiled$cells$overall_strata, c(rep(FALSE, 4), TRUE))
  expect_identical(
    compiled$cell_axes[9:10, ],
    tibble::tibble(
      cell_id = rep("c005", 2),
      var_id = c("v002", "v003"),
      value = c(NA_character_, NA_character_),
      is_overall = c(TRUE, TRUE)
    )
  )
})

test_that("compile_summary_cells() builds one leaf cell without design axes", {
  plan <- plan_summary(labelled_data())

  compiled <- compile_summary_cells(plan)

  expect_identical(
    compiled$cells,
    tibble::tibble(
      cell_id = "c001",
      overall_group = FALSE,
      overall_strata = FALSE,
      n = 3L
    )
  )
  expect_identical(
    compiled$cell_axes,
    tibble::tibble(
      cell_id = character(),
      var_id = character(),
      value = character(),
      is_overall = logical()
    )
  )
  expect_identical(
    compiled$cell_rows,
    tibble::tibble(cell_id = rep("c001", 3), row = 1:3)
  )
})

test_that("compile_summary_cells() requires a summary plan", {
  expect_error(
    compile_summary_cells(labelled_data()),
    class = "bq_error_invalid_plan"
  )
})
