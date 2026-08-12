test_that("strata create one task per observed level", {
  x <- as_bq_data(tibble::tibble(
    y = c(1, 2, 3, 5, 7, 9), x = c(1, 2, 3, 1, 2, 3),
    sex = c("F", "F", "F", "M", "M", "M")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous") |>
    set_role(sex, "stratum")

  plan <- plan_analysis(x, strata = sex)

  expect_equal(nrow(plan), 2L)
  expect_identical(plan$stratum_ids, rep(list(variables(x)$var_id[[3]]), 2))
  expect_identical(plan$strata, rep(list("sex"), 2))
  expect_identical(vapply(plan$stratum_values, `[[`, character(1), "sex"), c("F", "M"))
  expect_identical(plan$stratum_label, c("sex=F", "sex=M"))
  expect_identical(vapply(plan$formula, deparse, character(1)), rep("y ~ x", 2))
})

test_that("multiple strata use observed combinations only", {
  x <- as_bq_data(tibble::tibble(
    y = 1:6, x = 2:7,
    sex = c("F", "F", "M", "M", "M", "F"),
    diabetes = c("No", "Yes", "No", "No", "Yes", "No")
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")

  plan <- plan_analysis(x, strata = c(sex, diabetes))

  expect_equal(nrow(plan), 4L)
  expect_setequal(
    plan$stratum_label,
    c("sex=F; diabetes=No", "sex=F; diabetes=Yes",
      "sex=M; diabetes=No", "sex=M; diabetes=Yes")
  )
})

test_that("missing stratum values do not silently become a stratum", {
  x <- as_bq_data(tibble::tibble(
    y = 1:5, x = 2:6, sex = c("F", "F", "M", "M", NA)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")

  plan <- plan_analysis(x, strata = sex)

  expect_equal(nrow(plan), 2L)
  expect_true(all(plan$n_excluded_strata == 1L))
})

test_that("preflight is evaluated independently inside each stratum", {
  x <- as_bq_data(tibble::tibble(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(1, 2, 3, 1, 1, 1),
    sex = rep(c("F", "M"), each = 3)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")

  plan <- validate_plan(plan_analysis(x, strata = sex), x)

  expect_identical(plan$status, c("ready", "invalid"))
  expect_identical(plan$n_total, c(3L, 3L))
  expect_match(plan$reason[[2]], "Predictor has no variation")
})

test_that("run_analysis fits independent models within strata", {
  x <- as_bq_data(tibble::tibble(
    y = c(1, 2, 3, 10, 14, 18), x = rep(1:3, 2),
    sex = rep(c("F", "M"), each = 3)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  plan <- validate_plan(plan_analysis(x, strata = sex), x)

  result <- run_analysis(plan, x)
  slopes <- estimates(result)[estimates(result)$term == "x", ]

  expect_equal(nrow(slopes), 2L)
  expect_identical(slopes$stratum_label, c("sex=F", "sex=M"))
  expect_equal(slopes$estimate, c(1, 4))
  expect_length(models(result), 2L)
})
