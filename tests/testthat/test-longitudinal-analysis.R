test_that("LMM longitudinal analysis agrees with direct lme4 fit for long data", {
  skip_if_not_installed("lme4")
  set.seed(501)
  raw <- tidyr::expand_grid(id = factor(1:20), visit = factor(c("V0", "V1", "V2"))) |>
    dplyr::mutate(
      arm = factor(ifelse(as.integer(id) <= 10, "A", "B")),
      score = 10 + as.integer(visit) + 2 * (arm == "B") +
        1.5 * (arm == "B") * (visit == "V2") +
        rep(rnorm(20), each = 3) + rnorm(dplyr::n(), sd = 0.5)
    )
  data <- as_bq_data(raw) |>
    set_longitudinal_design(
      id, visit, group = arm, layout = "long", baseline = "V0"
    ) |>
    add_longitudinal_outcome(score_long, score)
  plan <- data |> plan_longitudinal(score_long, method = lmm_model()) |>
    validate_plan(data)
  result <- run_analysis(plan, data)
  direct <- lme4::lmer(score ~ arm * visit + (1 | id), data = raw, REML = FALSE)

  expect_identical(plan$status, "ready")
  expect_equal(
    estimates(result)$estimate,
    unname(lme4::fixef(direct)), tolerance = 1e-8
  )
  expect_true(inherits(models(result)[[plan$analysis_id]], "lmerMod"))
  expect_true("group_by_time" %in% tests(result)$test)
})

test_that("wide longitudinal outcomes use the canonical long frame", {
  skip_if_not_installed("lme4")
  data <- as_bq_data(tibble::tibble(
    id = factor(1:12), arm = factor(rep(c("A", "B"), each = 6)),
    y0 = rnorm(12), y1 = rnorm(12, 1), y2 = rnorm(12, 2)
  )) |>
    set_longitudinal_design(id, group = arm, layout = "wide") |>
    add_longitudinal_outcome(
      score, c(y0, y1, y2), c("V0", "V1", "V2"), baseline = "V0"
    )
  plan <- data |> plan_longitudinal(score, method = lmm_model()) |>
    validate_plan(data)
  result <- run_analysis(plan, data)

  expect_identical(plan$data_layout, "wide")
  expect_identical(plan$reshape_spec[[1]]$time_values, c("V0", "V1", "V2"))
  expect_true(all(estimates(result)$n == 36L))
})

test_that("GEE longitudinal analysis records a population-average estimand", {
  skip_if_not_installed("geepack")
  set.seed(502)
  raw <- tidyr::expand_grid(id = factor(1:30), visit = factor(c("V0", "V1"))) |>
    dplyr::mutate(
      arm = factor(ifelse(as.integer(id) <= 15, "A", "B")),
      score = rnorm(dplyr::n(), as.integer(visit) + (arm == "B"))
    )
  data <- as_bq_data(raw) |>
    set_longitudinal_design(id, visit, group = arm, layout = "long") |>
    add_longitudinal_outcome(score_long, score)
  plan <- data |> plan_longitudinal(score_long, method = gee_model("exchangeable")) |>
    validate_plan(data)
  result <- run_analysis(plan, data)

  expect_identical(plan$estimand, "population_average")
  expect_s3_class(models(result)[[plan$analysis_id]], "geeglm")
  expect_true(all(estimates(result)$variance == "sandwich"))
})

test_that("longitudinal preflight detects insufficient subjects", {
  data <- as_bq_data(tibble::tibble(
    id = "one", visit = c("V0", "V1"), arm = "A", score = c(1, 2)
  )) |>
    set_longitudinal_design(id, visit, group = arm, layout = "long") |>
    add_longitudinal_outcome(score_long, score)
  plan <- data |> plan_longitudinal(score_long, method = lmm_model()) |>
    validate_plan(data)

  expect_identical(plan$status, "invalid")
  expect_match(plan$reason, "subjects|variation", ignore.case = TRUE)
})

test_that("binary longitudinal outcomes run through explicit GLMM event coding", {
  skip_if_not_installed("lme4")
  set.seed(503)
  raw <- tidyr::expand_grid(id = factor(1:30), visit = factor(c("V0", "V1"))) |>
    dplyr::mutate(
      arm = factor(ifelse(as.integer(id) <= 15, "A", "B")),
      response = rbinom(dplyr::n(), 1,
        stats::plogis(-1 + (arm == "B") + (visit == "V1")))
    )
  data <- as_bq_data(raw) |>
    set_longitudinal_design(id, visit, group = arm, layout = "long") |>
    add_longitudinal_outcome(
      response_long, response, type = "binary", event_value = 1
    )
  plan <- data |> plan_longitudinal(response_long, method = glmm_model()) |>
    validate_plan(data)
  result <- run_analysis(plan, data)

  expect_identical(plan$status, "ready")
  expect_true(inherits(models(result)[[plan$analysis_id]], "glmerMod"))
  expect_true(all(estimates(result)$effect_measure == "odds_ratio"))
  expect_true(all(estimates(result)$estimate > 0))
})

test_that("binary longitudinal outcomes require an explicit event", {
  data <- as_bq_data(tibble::tibble(
    id = rep(1:2, each = 2), visit = rep(c("V0", "V1"), 2), y = c(0, 1, 1, 0)
  )) |>
    set_longitudinal_design(id, visit, layout = "long")
  expect_error(
    add_longitudinal_outcome(data, y_long, y, type = "binary"),
    class = "bq_error_invalid_outcome"
  )
})

test_that("singular mixed models are exposed in diagnostics", {
  skip_if_not_installed("lme4")
  set.seed(504)
  raw <- tidyr::expand_grid(id = factor(1:8), visit = factor(c("V0", "V1"))) |>
    dplyr::mutate(
      arm = factor(rep(c("A", "B"), each = 8)),
      score = as.numeric(visit) + (arm == "B") + rnorm(dplyr::n(), sd = 0.2)
    )
  data <- as_bq_data(raw) |>
    set_longitudinal_design(id, visit, group = arm, layout = "long") |>
    add_longitudinal_outcome(score_long, score)
  result <- data |> plan_longitudinal(score_long, lmm_model()) |>
    validate_plan(data) |> run_analysis(data)
  singular <- diagnostics(result)[diagnostics(result)$metric == "singular", ]

  expect_equal(singular$value, 1)
  expect_identical(singular$status, "warning")
})

test_that("longitudinal models return change and difference-in-change contrasts", {
  skip_if_not_installed("lme4")
  set.seed(505)
  raw <- tidyr::expand_grid(id = factor(1:24), visit = factor(c("V0", "V1", "V2"))) |>
    dplyr::mutate(
      arm = factor(ifelse(as.integer(id) <= 12, "A", "B")),
      score = 5 + as.integer(visit) + 2 * (arm == "B") +
        3 * (arm == "B") * (visit == "V2") + rep(rnorm(24), each = 3) +
        rnorm(dplyr::n(), 0, 0.3)
    )
  data <- as_bq_data(raw) |>
    set_longitudinal_design(
      id, visit, group = arm, layout = "long", baseline = "V0"
    ) |>
    add_longitudinal_outcome(score_long, score)
  result <- data |> plan_longitudinal(
    score_long, lmm_model(), comparisons = TRUE, adjust = "holm"
  ) |>
    validate_plan(data) |>
    run_analysis(data)
  output <- contrasts(result)
  difference <- output[output$estimand == "difference_in_changes" &
    output$modifier_level == "V2", ]
  fit <- models(result)[[1]]
  interaction <- grep("armB:visitV2|..bq_groupB:..bq_timeV2",
    names(lme4::fixef(fit)), value = TRUE)

  expect_true(any(output$estimand == "change_from_baseline"))
  expect_equal(difference$estimate, unname(lme4::fixef(fit)[interaction]))
  expect_equal(difference$std_error,
    sqrt(stats::vcov(fit)[interaction, interaction]))
  expect_true(all(output$adjust_method == "holm"))
})
