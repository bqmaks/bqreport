#' Declare a Dunnett many-to-one test
#'
#' Creates a terminal analytic function for simultaneous two-sided comparisons
#' of every other group with one declared reference.
#' `PMCMRplus::dunnettTest()` supplies the correlated multivariate-t p-values.
#' Estimates are arithmetic-mean differences oriented as comparison minus
#' reference. No omnibus test is computed or returned.
#'
#' @param reference One non-empty reference group value.
#'
#' @return A `bq_dunnett_test` analytic function.
#' @export
dunnett_test <- function(reference) {
  if (missing(reference)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`reference` must be one non-empty group value."
    )
  }
  check_string(reference, "reference")
  check_dependency("PMCMRplus", "`dunnett_test()`", "1.9.12")

  specification <- list(
    kind = "dunnett_test",
    family = "reference",
    reference = reference,
    estimand = "mean_difference",
    p_adjust_method = "dunnett_single_step"
  )
  capabilities <- list(
    outcome_types = "continuous",
    group_min_levels = 2L,
    group_max_levels = NA_integer_,
    supplied_results = "comparison_family",
    suggested_dependencies = "PMCMRplus (>= 1.9.12)"
  )
  analysis_function <- function(data, context) {
    prepared <- prepare_engine_input(data, context, "dunnett_test")
    group_values <- prepared$group_values
    pairs <- compile_comparison_family(
      group_values,
      "reference",
      specification$reference
    )
    comparison_n <- nrow(pairs)
    used <- prepared$used
    engine_levels <- c(
      specification$reference,
      setdiff(group_values, specification$reference)
    )
    engine_group <- factor(
      as.character(data$.group[used]),
      levels = engine_levels
    )
    engine_result <- tryCatch(
      PMCMRplus::dunnettTest(
        x = data$.outcome[used],
        g = engine_group,
        alternative = "two.sided"
      ),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`dunnett_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )
    statistic <- unname(vapply(pairs$comparison_value, function(value) {
      unname(as.double(engine_result$statistic[value, specification$reference]))
    }, double(1)))
    p_value_adjusted <- unname(vapply(pairs$comparison_value, function(value) {
      unname(as.double(engine_result$p.value[value, specification$reference]))
    }, double(1)))
    group_mean <- vapply(group_values, function(value) {
      mean(data$.outcome[data$.group == value], na.rm = TRUE)
    }, double(1))
    estimate <- unname(
      group_mean[pairs$comparison_value] - group_mean[pairs$reference_value]
    )
    residual_df <- sum(used) - length(group_values)
    valid_result <- all(is.finite(c(
      estimate, statistic, p_value_adjusted, residual_df
    ))) && residual_df > 0 && all(p_value_adjusted >= 0 & p_value_adjusted <= 1)
    if (!valid_result) {
      bq_abort(
        "bq_error_analysis_runtime",
        "Dunnett comparisons produced a non-finite result.",
        analysis_id = context$analysis_id
      )
    }

    comparisons <- tibble::tibble(
      comparison_id = pairs$comparison_id,
      analysis_id = rep(context$analysis_id, comparison_n),
      outcome_var_id = rep(context$outcome_var_id, comparison_n),
      group_var_id = rep(context$group_var_id, comparison_n),
      family = rep("reference", comparison_n),
      family_size = rep(as.integer(comparison_n), comparison_n),
      reference_value = pairs$reference_value,
      comparison_value = pairs$comparison_value,
      direction = pairs$direction,
      test = rep("dunnett", comparison_n),
      estimand = rep("mean_difference", comparison_n),
      estimate = estimate,
      std_error = rep(NA_real_, comparison_n),
      effect_size_type = rep(NA_character_, comparison_n),
      effect_size = rep(NA_real_, comparison_n),
      effect_std_error = rep(NA_real_, comparison_n),
      effect_size_method = rep(NA_character_, comparison_n),
      effect_size_correction = rep(NA_character_, comparison_n),
      effect_conf_low = rep(NA_real_, comparison_n),
      effect_conf_high = rep(NA_real_, comparison_n),
      effect_conf_level = rep(NA_real_, comparison_n),
      effect_interval_scope = rep(NA_character_, comparison_n),
      effect_ci_method = rep(NA_character_, comparison_n),
      effect_ci_clamped = rep(NA, comparison_n),
      statistic = statistic,
      statistic_type = rep("t", comparison_n),
      df = rep(as.double(residual_df), comparison_n),
      p_value = rep(NA_real_, comparison_n),
      p_value_adjusted = p_value_adjusted,
      p_adjust_method = rep("dunnett_single_step", comparison_n),
      conf_low = rep(NA_real_, comparison_n),
      conf_high = rep(NA_real_, comparison_n),
      conf_level = rep(NA_real_, comparison_n),
      interval_scope = rep("not_computed", comparison_n),
      ci_method = rep("not_computed", comparison_n),
      ci_clamped = rep(NA, comparison_n),
      inference = rep("analytical", comparison_n),
      variance_assumption = rep("equal", comparison_n),
      exact_requested = rep(NA_character_, comparison_n),
      exact_used = rep(NA, comparison_n),
      has_ties = rep(NA, comparison_n),
      continuity_correction = rep(NA, comparison_n),
      engine = rep("PMCMRplus::dunnettTest", comparison_n)
    )

    list(
      comparisons = comparisons,
      sample_flow = prepared$sample_flow
    )
  }

  structure(
    analysis_function,
    specification = specification,
    capabilities = capabilities,
    class = c("bq_dunnett_test", "bq_analysis_function", "function")
  )
}
