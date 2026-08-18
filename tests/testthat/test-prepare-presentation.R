test_that("prepare_presentation() distinguishes cell status and display mode", {
  data <- as_bq_data(tibble::tibble(
    value = c(3, NA, 1, 2, 4, NA, NA),
    stratum = factor(
      c("A", "A", "B", "B", "B", "D", "D"),
      levels = c("A", "B", "C", "D")
    )
  ))
  data <- set_type(data, value, type_continuous())
  data <- set_type(data, stratum, type_nominal("A"))
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
    strata = stratum
  )
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(plan, value, enumerate_values(max_n = 2L))
  result <- run_analysis(plan)

  presentation <- prepare_presentation(result)

  expect_s3_class(
    presentation,
    c("bq_presentation_summary", "bq_presentation"),
    exact = TRUE
  )
  expect_identical(presentation$analysis, "summary")
  expect_identical(presentation$result, result)
  expect_identical(
    presentation$display_cells,
    tibble::tibble(
      cell_id = sprintf("c%03d", 1:4),
      var_id = rep("v001", 4),
      status = c("observed", "observed", "empty", "all_missing"),
      show_statistics = c(FALSE, TRUE, TRUE, TRUE),
      show_values = c(TRUE, FALSE, FALSE, FALSE),
      rule_id = rep("r001", 4)
    )
  )
  expect_identical(
    presentation$display_values,
    tibble::tibble(
      cell_id = "c001",
      var_id = "v001",
      position = 1L,
      value = 3
    )
  )
})

test_that("prepare_presentation() retains order and duplicates in Overall", {
  data <- as_bq_data(tibble::tibble(
    value = c(2, 1, 2),
    stratum = factor(c("A", "B", "A"), levels = c("A", "B"))
  ))
  data <- set_type(data, value, type_continuous())
  data <- set_type(data, stratum, type_nominal("A"))
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
    strata = stratum,
    overall = "strata"
  )
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(plan, value, enumerate_values(max_n = 3L))

  presentation <- prepare_presentation(run_analysis(plan))

  expect_identical(
    presentation$display_values,
    tibble::tibble(
      cell_id = c("c001", "c001", "c002", "c003", "c003", "c003"),
      var_id = rep("v001", 6),
      position = c(1L, 2L, 1L, 1L, 2L, 3L),
      value = c(2, 2, 1, 2, 1, 2)
    )
  )
})

test_that("prepare_presentation() shows only statistics without a rule", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  data <- set_type(data, value, type_continuous())
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = mean(x))
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)

  presentation <- prepare_presentation(run_analysis(plan))

  expect_identical(presentation$display_cells$status, "observed")
  expect_true(presentation$display_cells$show_statistics)
  expect_false(presentation$display_cells$show_values)
  expect_identical(presentation$display_cells$rule_id, NA_character_)
  expect_identical(nrow(presentation$display_values), 0L)
})

test_that("prepare_presentation() can show statistics and values together", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2)))
  data <- set_type(data, value, type_continuous())
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = mean(x))
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(
    plan,
    value,
    enumerate_values(display_statistics = TRUE)
  )

  presentation <- prepare_presentation(run_analysis(plan))

  expect_true(presentation$display_cells$show_statistics)
  expect_true(presentation$display_cells$show_values)
  expect_identical(presentation$display_values$value, c(1, 2))
})

test_that("prepare_presentation() detects inconsistent source counts", {
  data <- as_bq_data(tibble::tibble(value = 1))
  data <- set_type(data, value, type_continuous())
  statistic <- continuous_statistic(
    "mean",
    function(x) data.frame(mean = mean(x))
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(plan, value, enumerate_values())
  result <- run_analysis(plan)
  result$sample_sizes$n <- 2L

  expect_error(
    prepare_presentation(result),
    class = "bq_error_invalid_result"
  )
})

test_that("prepare_presentation() requires an analysis result", {
  expect_error(
    prepare_presentation(labelled_data()),
    class = "bq_error_invalid_result"
  )
})
