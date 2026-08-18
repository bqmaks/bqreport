test_that("oneway_anova() returns an inspectable analytic function", {
  analysis <- oneway_anova(
    var_equal = TRUE,
    effect_size = "eta_squared",
    conf_level = 0.9
  )

  expect_true(is.function(analysis))
  expect_s3_class(
    analysis,
    c("bq_oneway_anova", "bq_analysis_function", "function"),
    exact = TRUE
  )
  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "oneway_anova",
      var_equal = TRUE,
      effect_size = "eta_squared",
      conf_level = 0.9
    )
  )
  expect_identical(
    attr(analysis, "capabilities"),
    list(
      outcome_types = "continuous",
      outcomes_per_analysis = 1L,
      requires_group = TRUE,
      group_min_levels = 2L,
      group_max_levels = NA_integer_,
      max_strata = 0L,
      supports_covariates = FALSE,
      supports_weights = FALSE,
      supports_clusters = FALSE,
      supports_matched_sets = FALSE,
      provides_fits = FALSE,
      supplied_results = c("omnibus_test", "effect_size"),
      supplied_extractors = character(),
      suggested_dependencies = "effectsize"
    )
  )
  expect_identical(names(formals(analysis)), c("data", "context"))
})

test_that("oneway_anova() records its defaults", {
  analysis <- oneway_anova()

  expect_identical(
    attr(analysis, "specification"),
    list(
      kind = "oneway_anova",
      var_equal = TRUE,
      effect_size = "omega_squared",
      conf_level = 0.95
    )
  )
})

test_that("oneway_anova() declares only requested results and dependencies", {
  analysis <- oneway_anova(effect_size = "none")
  capabilities <- attr(analysis, "capabilities")

  expect_identical(capabilities$supplied_results, "omnibus_test")
  expect_identical(capabilities$suggested_dependencies, character())
})

test_that("oneway_anova() validates var_equal", {
  invalid <- list(NULL, NA, 1, "yes", c(TRUE, FALSE))

  for (var_equal in invalid) {
    expect_error(
      oneway_anova(var_equal = var_equal),
      class = "bq_error_invalid_analysis_function"
    )
  }

  expect_error(
    oneway_anova(var_equal = FALSE),
    "Welch's one-way ANOVA is not yet available",
    class = "bq_error_invalid_analysis_function"
  )
})

test_that("oneway_anova() validates effect_size", {
  invalid <- list(NULL, NA_character_, "", "cohen_d", c("none", "eta_squared"), 1)

  for (effect_size in invalid) {
    expect_error(
      oneway_anova(effect_size = effect_size),
      class = "bq_error_invalid_analysis_function"
    )
  }

  for (effect_size in c("none", "eta_squared", "omega_squared")) {
    analysis <- oneway_anova(effect_size = effect_size)
    expect_identical(attr(analysis, "specification")$effect_size, effect_size)
  }
})

test_that("oneway_anova() validates conf_level", {
  invalid <- list(NULL, NA_real_, NaN, Inf, -0.1, 0, 1, 1.1, c(0.9, 0.95), "0.95")

  for (conf_level in invalid) {
    expect_error(
      oneway_anova(conf_level = conf_level),
      class = "bq_error_invalid_analysis_function"
    )
  }
})

oneway_anova_input <- function(outcome, group) {
  group <- factor(group, levels = unique(group))
  list(
    data = tibble::tibble(
      .row_id = seq_along(outcome),
      .outcome = outcome,
      .group = group
    ),
    context = list(
      analysis_id = "a001",
      test_id = "t001",
      outcome_var_id = "v001",
      group_var_id = "v002",
      strata_var_id = NA_character_,
      group_levels = tibble::tibble(
        var_id = rep("v002", nlevels(group)),
        value = levels(group),
        position = seq_len(nlevels(group))
      )
    )
  )
}

test_that("oneway_anova() returns only its omnibus result", {
  input <- oneway_anova_input(
    outcome = c(2, 3, 5, 6, 9, 10),
    group = rep(c("control", "treatment", "other"), each = 2L)
  )
  original_data <- input$data
  analysis <- oneway_anova(effect_size = "none")
  result <- analysis(input$data, input$context)
  direct_fit <- stats::lm(.outcome ~ .group, data = input$data)
  direct_anova <- stats::anova(direct_fit)

  expect_named(
    result,
    c("tests", "estimates", "sample_flow")
  )
  expect_identical(
    result$tests,
    tibble::tibble(
      test_id = "t001",
      analysis_id = "a001",
      outcome_var_id = "v001",
      test = "oneway_anova",
      statistic = unname(as.double(direct_anova[["F value"]][1L])),
      df1 = unname(as.double(direct_anova[["Df"]][1L])),
      df2 = unname(as.double(direct_anova[["Df"]][2L])),
      p_value = unname(as.double(direct_anova[["Pr(>F)"]][1L]))
    )
  )
  expect_identical(
    result$estimates,
    tibble::tibble(
      estimate_id = character(),
      analysis_id = character(),
      outcome_var_id = character(),
      estimand = character(),
      estimate = double(),
      std_error = double(),
      conf_low = double(),
      conf_high = double()
    )
  )
  expect_identical(
    result$sample_flow,
    tibble::tibble(
      analysis_id = rep("a001", 3L),
      outcome_var_id = rep("v001", 3L),
      group_value = c("control", "treatment", "other"),
      n_total = rep(2L, 3L),
      n_missing = rep(0L, 3L),
      n_used = rep(2L, 3L)
    )
  )
  expect_identical(input$data, original_data)
})

test_that("oneway_anova() reports its missing outcome policy", {
  input <- oneway_anova_input(
    outcome = c(2, NA, 5, 6, 9, NA, 11),
    group = c("control", "control", "treatment", "treatment", "other", "other", "other")
  )
  result <- oneway_anova(effect_size = "none")(input$data, input$context)

  expect_identical(result$sample_flow$n_total, c(2L, 2L, 3L))
  expect_identical(result$sample_flow$n_missing, c(1L, 0L, 1L))
  expect_identical(result$sample_flow$n_used, c(1L, 2L, 2L))
  expect_identical(sum(result$sample_flow$n_used), 5L)
})

test_that("oneway_anova() validates prepared data and context", {
  input <- oneway_anova_input(
    outcome = c(2, 3, 5, 6),
    group = rep(c("control", "treatment"), each = 2L)
  )
  analysis <- oneway_anova(effect_size = "none")

  invalid_data <- as.data.frame(input$data)
  expect_error(
    analysis(invalid_data, input$context),
    class = "bq_error_invalid_analysis_input"
  )

  missing_group <- input$data
  missing_group$.group[1L] <- NA
  expect_error(
    analysis(missing_group, input$context),
    "without missing values",
    class = "bq_error_invalid_analysis_input"
  )

  invalid_context <- input$context
  invalid_context$group_levels$value <- rev(invalid_context$group_levels$value)
  expect_error(
    analysis(input$data, invalid_context),
    "group_levels",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("oneway_anova() rejects unestimable group data", {
  input <- oneway_anova_input(
    outcome = c(2, 3, NA, NA),
    group = rep(c("control", "treatment"), each = 2L)
  )
  analysis <- oneway_anova(effect_size = "none")

  expect_error(
    analysis(input$data, input$context),
    "no observed outcome values",
    class = "bq_error_invalid_analysis_input"
  )

  input <- oneway_anova_input(
    outcome = c(2, 3),
    group = c("control", "treatment")
  )
  expect_error(
    analysis(input$data, input$context),
    "positive residual degrees of freedom",
    class = "bq_error_invalid_analysis_input"
  )
})

test_that("oneway_anova() effect-size execution remains explicit", {
  input <- oneway_anova_input(
    outcome = c(2, 3, 5, 6),
    group = rep(c("control", "treatment"), each = 2L)
  )
  analysis <- oneway_anova(effect_size = "eta_squared")

  expect_error(
    analysis(input$data, input$context),
    "Effect sizes",
    class = "bq_error_analysis_not_implemented"
  )
})
