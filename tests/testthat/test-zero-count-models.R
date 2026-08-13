test_that("zero-inflated and hurdle models agree with pscl", {
  skip_if_not_installed("pscl")
  set.seed(44)
  x <- rep(c(0, 1), each = 60L)
  y <- stats::rpois(120, exp(0.2 + 0.7 * x))
  y[seq(1, 120, by = 4)] <- 0
  data <- as_bq_data(tibble::tibble(y, x)) |>
    set_outcome(y, type = "count") |>
    set_predictor(x, type = "continuous")
  run <- function(method) data |> plan_analysis(
    y, x, rules = analysis_rules(where_count() ~ method)
  ) |> validate_plan(data) |> run_analysis(data)

  zi <- run(zero_inflated_model(distribution = "poisson"))
  hurdle <- run(hurdle_model(distribution = "poisson"))
  direct_zi <- pscl::zeroinfl(y ~ x | 1, data = data, dist = "poisson")
  direct_hurdle <- pscl::hurdle(y ~ x | 1, data = data, dist = "poisson")

  expect_equal(
    estimates(zi)$estimate,
    exp(c(unname(stats::coef(direct_zi, model = "count")),
      unname(stats::coef(direct_zi, model = "zero"))))
  )
  expect_equal(
    estimates(hurdle)$estimate,
    exp(c(unname(stats::coef(direct_hurdle, model = "count")),
      unname(stats::coef(direct_hurdle, model = "zero"))))
  )
  expect_true(all(grepl("^(count|zero):", estimates(zi)$term)))
  expect_identical(zi$plan$required_packages, list("pscl"))
})
