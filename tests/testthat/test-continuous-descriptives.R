test_that("continuous_descriptives() declares the basic built-in set", {
  statistic <- continuous_descriptives()

  expect_s3_class(
    statistic,
    c("bq_continuous_statistic", "bq_statistic"),
    exact = TRUE
  )
  expect_identical(statistic$name, "descriptives")
  expect_identical(
    statistic$components,
    c("mean", "sd", "median", "q1", "q3", "min", "max")
  )
  expect_identical(
    unname(statistic$component_types),
    rep("double", 7)
  )
  expect_identical(
    unname(statistic$component_scales),
    rep("variable", 7)
  )
  expect_identical(statistic$source, "built_in_raw")
  expect_identical(statistic$missing, "omit")
})

test_that("continuous_descriptives() matches direct base calculations", {
  x <- c(1, 2, NA, 4, 8)
  observed <- x[!is.na(x)]

  result <- continuous_descriptives()$fun(x)

  expect_identical(
    result,
    data.frame(
      mean = mean(observed),
      sd = stats::sd(observed),
      median = stats::median(observed),
      q1 = unname(stats::quantile(observed, 0.25, type = 7)),
      q3 = unname(stats::quantile(observed, 0.75, type = 7)),
      min = as.double(min(observed)),
      max = as.double(max(observed))
    )
  )
})

test_that("continuous_descriptives() handles empty and small samples", {
  empty <- continuous_descriptives()$fun(numeric())
  all_missing <- continuous_descriptives()$fun(c(NA_real_, NA_real_))
  one <- continuous_descriptives()$fun(5)

  expect_identical(empty, all_missing)
  expect_true(all(is.na(empty)))
  expect_identical(
    one,
    data.frame(
      mean = 5,
      sd = NA_real_,
      median = 5,
      q1 = 5,
      q3 = 5,
      min = 5,
      max = 5
    )
  )
})

test_that("continuous_descriptives() runs through the summary engine", {
  data <- as_bq_data(tibble::tibble(value = c(1, 2, NA, 4)))
  data <- set_type(data, value, type_continuous())
  data <- set_rounding(data, value, 1)
  plan <- plan_summary(data) |>
    add_statistic(value)

  result <- run_analysis(plan)

  expect_identical(
    result$estimates$value,
    as.double(unlist(
      continuous_descriptives()$fun(data$value)[1, ],
      use.names = FALSE
    ))
  )
})
