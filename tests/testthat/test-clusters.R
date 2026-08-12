test_that("set_cluster and plan store matched-set identity by stable id", {
  x <- as_bq_data(tibble::tibble(y = 1:6, x = 2:7, matched_set = rep(1:3, each = 2))) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_cluster(matched_set, type = "matched_set")

  plan <- validate_plan(plan_analysis(x, cluster = matched_set), x)

  expect_identical(plan$cluster_id, variables(x)$var_id[[3]])
  expect_identical(plan$cluster_type, "matched_set")
  expect_identical(plan$variance, "cluster_robust")
  expect_identical(plan$cluster_diagnostics[[1]]$n_clusters, 3L)
  expect_identical(plan$cluster_diagnostics[[1]]$min_size, 2L)
  expect_identical(plan$cluster_diagnostics[[1]]$max_size, 2L)
})

test_that("missing cluster ids are excluded from the analysis frame", {
  x <- as_bq_data(tibble::tibble(
    y = 1:6, x = 2:7, matched_set = c(1, 1, 2, 2, NA, 3)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_cluster(matched_set)

  plan <- validate_plan(plan_analysis(x, cluster = matched_set), x)

  expect_identical(plan$n_analyzed, 5L)
  expect_identical(plan$cluster_diagnostics[[1]]$n_missing, 1L)
})

test_that("clustered weighted regression matches vcovCL", {
  x <- as_bq_data(tibble::tibble(
    y = c(2, 2.5, 3.8, 4.4, 5.7, 6.3, 7.2, 8.1),
    exposure = rep(c(0, 1), 4), age = c(30, 31, 40, 42, 50, 49, 60, 62),
    w = c(1, 1.1, 0.9, 1.2, 1, 0.8, 1.3, 1), set = rep(1:4, each = 2)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(exposure, type = "continuous") |>
    set_predictor(age, type = "continuous") |>
    set_weight(w, type = "ipw") |>
    set_cluster(set)
  plan <- validate_plan(plan_analysis(
    x, predictors = exposure, covariates = age, weights = w, cluster = set
  ), x)

  result <- run_analysis(plan, x)
  direct <- stats::lm(y ~ exposure + age, data = x, weights = w)
  direct_se <- sqrt(diag(sandwich::vcovCL(
    direct, cluster = x$set, type = "HC1", cadjust = TRUE
  )))

  expect_identical(estimates(result)$estimate, unname(stats::coef(direct)))
  expect_equal(estimates(result)$std_error, unname(direct_se))
  expect_identical(unique(estimates(result)$variance), "cluster_robust")
})

test_that("cluster configuration validates column and variance", {
  x <- as_bq_data(tibble::tibble(y = 1:4, x = 2:5, set = 1:4)) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_cluster(set)

  expect_error(
    plan_analysis(x, cluster = set, variance = "model_based"),
    class = "bq_error_invalid_cluster"
  )
})
