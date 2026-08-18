test_that("run_analysis() computes raw continuous summaries by cell", {
  data <- as_bq_data(tibble::tibble(
    value = c(3, NA, 1, 2),
    treatment = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  ))
  data <- set_type(data, value, continuous())
  data <- set_type(data, treatment, binary("B"))
  statistic <- continuous_statistic(
    "observed_values",
    function(x) {
      data.frame(
        first = if (length(x) == 0L) NA_real_ else x[1L],
        observed = sum(!is.na(x)),
        total = length(x)
      )
    }
  )
  plan <- plan_summary(
    data,
    group = treatment
  )
  plan <- add_statistic(plan, value, statistic)

  result <- run_analysis(plan)

  expect_s3_class(
    result,
    c("bq_result_summary", "bq_result"),
    exact = TRUE
  )
  expect_identical(result$analysis, "summary")
  expect_identical(result$plan, plan)
  expect_identical(
    result$sample_sizes,
    tibble::tibble(
      cell_id = c("c001", "c002"),
      var_id = rep("v001", 2),
      n = c(1L, 2L),
      n_missing = c(1L, 0L)
    )
  )
  expect_identical(
    result$estimates,
    tibble::tibble(
      cell_id = rep(c("c001", "c002"), each = 3),
      var_id = rep("v001", 6),
      statistic_id = rep("s001", 6),
      component = rep(c("first", "observed", "total"), 2),
      value = c(3, 1, 2, 1, 2, 2)
    )
  )
})

test_that("run_analysis() computes raw Overall from its member rows", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, NA, 3),
    treatment = factor(c("A", "A", "B"), levels = c("A", "B"))
  ))
  data <- set_type(data, value, continuous())
  data <- set_type(data, treatment, binary("B"))
  statistic <- continuous_statistic(
    "total",
    function(x) data.frame(total = sum(x, na.rm = TRUE))
  )
  plan <- plan_summary(
    data,
    group = treatment,
    overall = "group"
  )
  plan <- add_statistic(plan, value, statistic)

  result <- run_analysis(plan)

  expect_identical(result$cells$n, c(2L, 1L, 3L))
  expect_identical(result$sample_sizes$n, c(1L, 1L, 2L))
  expect_identical(result$sample_sizes$n_missing, c(1L, 0L, 1L))
  expect_identical(result$estimates$value, c(1, 3, 4))
})

test_that("run_analysis() executes statistics for empty cells", {
  data <- as_bq_data(tibble::tibble(
    value = c(1, 2),
    treatment = factor(c("A", "A"), levels = c("A", "B"))
  ))
  data <- set_type(data, value, continuous())
  data <- set_type(data, treatment, binary("B"))
  statistic <- continuous_statistic(
    "length",
    function(x) data.frame(length = length(x))
  )
  plan <- plan_summary(
    data,
    group = treatment
  )
  plan <- add_statistic(plan, value, statistic)

  result <- run_analysis(plan)

  expect_identical(result$diagnostics$code, "empty_cell")
  expect_identical(result$sample_sizes$n, c(2L, 0L))
  expect_identical(result$sample_sizes$n_missing, c(0L, 0L))
  expect_identical(result$estimates$value, c(2, 0))
})

test_that("run_analysis() keeps raw values despite display metadata", {
  data <- as_bq_data(tibble::tibble(value = 1.25))
  data <- set_type(data, value, continuous())
  data <- set_rounding(data, value, digits = 0L)
  statistic <- continuous_statistic(
    "mean",
    function(x) {
      data.frame(mean = if (length(x) == 0L) NA_real_ else mean(x))
    }
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)
  plan <- add_display_rule(plan, value, enumerate_values())

  result <- run_analysis(plan)

  expect_identical(result$estimates$value, 1.25)
})

test_that("run_analysis() uses the declared continuous model frame", {
  data <- as_bq_data(tibble::tibble(value = factor(c("2.5", "1.5"))))
  data <- set_type(data, value, continuous())
  plan <- plan_summary(data) |>
    add_statistic(value) |>
    add_display_rule(value, enumerate_values())

  result <- run_analysis(plan)
  presentation <- prepare_presentation(result)

  expect_identical(
    result$estimates$value[result$estimates$component == "mean"],
    2
  )
  expect_identical(presentation$display_values$value, c(2.5, 1.5))
})

test_that("run_analysis() stops before computing a plan that fails preflight", {
  plan <- plan_summary(labelled_data())

  error <- tryCatch(
    run_analysis(plan),
    bq_error_preflight = identity
  )

  expect_s3_class(error, "bq_error_preflight")
  expect_false(error$preflight$ok)
  expect_identical(
    error$preflight$diagnostics$code,
    "missing_summary_variable"
  )
})

test_that("run_analysis() wraps errors raised by a statistic", {
  data <- as_bq_data(tibble::tibble(value = 1))
  data <- set_type(data, value, continuous())
  statistic <- continuous_statistic(
    "failure",
    function(x) {
      if (length(x) > 0L) {
        stop("boom")
      }
      data.frame(value = NA_real_)
    }
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)

  error <- tryCatch(
    run_analysis(plan),
    bq_error_statistic_runtime = identity
  )

  expect_s3_class(error, "bq_error_statistic_runtime")
  expect_identical(error$cell_id, "c001")
  expect_identical(error$var_id, "v001")
  expect_identical(error$statistic_id, "s001")
  expect_identical(conditionMessage(error$parent), "boom")
})

test_that("run_analysis() rejects runtime component type changes", {
  data <- as_bq_data(tibble::tibble(value = 1))
  data <- set_type(data, value, continuous())
  statistic <- continuous_statistic(
    "unstable",
    function(x) {
      data.frame(value = if (length(x) == 0L) NA_real_ else 1L)
    }
  )
  plan <- plan_summary(data)
  plan <- add_statistic(plan, value, statistic)

  error <- tryCatch(
    run_analysis(plan),
    bq_error_statistic_schema = identity
  )

  expect_s3_class(error, "bq_error_statistic_schema")
  expect_identical(error$cell_id, "c001")
  expect_identical(error$var_id, "v001")
  expect_identical(error$statistic_id, "s001")
})

test_that("run_analysis() validates the complete runtime schema", {
  runtime_outputs <- list(
    1,
    data.frame(value = numeric()),
    data.frame(value = c(1, 2)),
    data.frame(changed = 1),
    data.frame(value = "text")
  )

  for (runtime_output in runtime_outputs) {
    data <- as_bq_data(tibble::tibble(value = 1))
    data <- set_type(data, value, continuous())
    statistic <- continuous_statistic(
      "unstable",
      local({
        output <- runtime_output
        function(x) {
          if (length(x) == 0L) {
            return(data.frame(value = NA_real_))
          }
          output
        }
      })
    )
    plan <- plan_summary(data)
    plan <- add_statistic(plan, value, statistic)

    expect_error(
      run_analysis(plan),
      class = "bq_error_statistic_schema"
    )
  }
})

test_that("run_analysis() requires an analysis plan", {
  expect_error(
    run_analysis(labelled_data()),
    class = "bq_error_invalid_plan"
  )
})
