#' Declare a family of Brunner-Munzel tests
#'
#' Creates a terminal analytic function for a declared family of two-sided
#' Brunner-Munzel comparisons. Each comparison estimates the probability that
#' a comparison-group value exceeds a reference-group value, with half credit
#' for ties. P-values are adjusted only across the comparisons in the declared
#' family.
#'
#' No omnibus test is computed or returned. Kruskal-Wallis and Welch ANOVA do
#' not test the same stochastic-equality hypothesis under the assumptions
#' permitted by the Brunner-Munzel procedure.
#'
#' The primary estimate is the relative effect. Cliff's delta is supplied as
#' `2 * relative_effect - 1`, oriented as comparison minus reference. Relative
#' effect confidence intervals are individual and are not multiplicity
#' adjusted.
#'
#' @param comparisons Comparison family: `"pairwise"`, `"reference"` or
#'   `"consecutive"`.
#' @param reference Reference group value when `comparisons = "reference"`;
#'   otherwise `NULL`.
#' @param inference `"asymptotic"` for the t approximation or `"logit"` for
#'   logit-scale inference with a range-preserving relative-effect interval.
#' @param p_adjust Multiplicity method passed to [stats::p.adjust()] across the
#'   declared family.
#' @param conf_level Individual relative-effect confidence level.
#'
#' @return A `bq_brunner_munzel_family` analytic function.
#' @export
brunner_munzel_family <- function(
  comparisons = "pairwise",
  reference = NULL,
  inference = "asymptotic",
  p_adjust = "holm",
  conf_level = 0.95
) {
  reference <- check_comparison_family(comparisons, reference)
  check_choice(inference, "inference", c("asymptotic", "logit"))
  check_p_adjust(p_adjust)
  conf_level <- check_conf_level(conf_level)

  pair_test <- brunner_munzel_test(
    inference = inference,
    conf_level = conf_level
  )
  specification <- list(
    kind = "brunner_munzel_family",
    family = comparisons,
    reference = reference,
    inference = inference,
    p_adjust_method = p_adjust,
    conf_level = conf_level
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
    group_min_levels = 2L,
    group_max_levels = NA_integer_,
    supplied_results = c("comparison_family", "pairwise_effect_size"),
    suggested_dependencies = "TOSTER (>= 0.9.0)"
  )

  analysis_function <- function(data, context) {
    prepared <- prepare_engine_input(
      data, context, "brunner_munzel_family"
    )
    group_values <- prepared$group_values
    family_reference <- if (specification$family == "reference") {
      specification$reference
    } else {
      NULL
    }
    pairs <- compile_comparison_family(
      group_values,
      specification$family,
      family_reference
    )
    comparison_n <- nrow(pairs)
    pair_results <- vector("list", comparison_n)

    for (position in seq_len(comparison_n)) {
      included <- data$.group %in% c(
        pairs$reference_value[[position]],
        pairs$comparison_value[[position]]
      )
      pair_group <- factor(
        as.character(data$.group[included]),
        levels = c(
          pairs$reference_value[[position]],
          pairs$comparison_value[[position]]
        )
      )
      pair_data <- tibble::tibble(
        .row_id = data$.row_id[included],
        .outcome = data$.outcome[included],
        .group = pair_group
      )
      pair_context <- list(
        analysis_id = context$analysis_id,
        test_id = paste0(context$test_id, "_", pairs$comparison_id[[position]]),
        estimate_id = NA_character_,
        outcome_var_id = context$outcome_var_id,
        group_var_id = context$group_var_id,
        strata_var_id = NA_character_,
        reference_value = pairs$reference_value[[position]],
        group_levels = tibble::tibble(
          var_id = rep(context$group_var_id, 2L),
          value = levels(pair_group),
          position = 1:2
        )
      )
      pair_results[[position]] <- pair_test(pair_data, pair_context)$tests
    }

    estimate <- vapply(
      pair_results, function(result) result$raw_estimate, double(1)
    )
    std_error <- vapply(
      pair_results, function(result) result$std_error, double(1)
    )
    statistic <- vapply(
      pair_results, function(result) result$statistic, double(1)
    )
    df <- vapply(pair_results, function(result) result$df, double(1))
    p_value_raw <- vapply(
      pair_results, function(result) result$p_value, double(1)
    )
    conf_low <- vapply(
      pair_results, function(result) result$conf_low, double(1)
    )
    conf_high <- vapply(
      pair_results, function(result) result$conf_high, double(1)
    )
    interval_conf_level <- vapply(
      pair_results,
      function(result) result$interval_conf_level,
      double(1)
    )
    # TOSTER truncates the relative-effect interval at 0 and 1; the flag is
    # kept because a truncated bound no longer matches the standard error.
    ci_clamped <- vapply(
      pair_results, function(result) result$ci_clamped, logical(1)
    )
    p_value_adjusted <- stats::p.adjust(
      p_value_raw,
      method = specification$p_adjust_method
    )
    effect_size <- 2 * estimate - 1
    effect_std_error <- 2 * std_error
    effect_conf_low <- 2 * conf_low - 1
    effect_conf_high <- 2 * conf_high - 1
    valid_result <- all(is.finite(c(
      estimate, std_error, statistic, df, p_value_raw, p_value_adjusted,
      conf_low, conf_high, interval_conf_level, effect_size, effect_std_error,
      effect_conf_low, effect_conf_high
    ))) && all(std_error > 0) && all(effect_std_error > 0) &&
      all(df > 0) && all(estimate >= 0 & estimate <= 1) &&
      all(p_value_raw >= 0 & p_value_raw <= 1) &&
      all(p_value_adjusted >= 0 & p_value_adjusted <= 1) && all(conf_low <= conf_high) &&
      all(effect_size >= -1 & effect_size <= 1) &&
      all(effect_conf_low >= -1 & effect_conf_high <= 1) &&
      all(effect_conf_low <= effect_conf_high)
    if (!valid_result) {
      bq_abort(
        "bq_error_analysis_runtime",
        "The Brunner-Munzel family produced a non-finite comparison result.",
        analysis_id = context$analysis_id
      )
    }

    comparisons <- tibble::tibble(
      comparison_id = pairs$comparison_id,
      analysis_id = rep(context$analysis_id, comparison_n),
      outcome_var_id = rep(context$outcome_var_id, comparison_n),
      group_var_id = rep(context$group_var_id, comparison_n),
      family = rep(specification$family, comparison_n),
      family_size = rep(as.integer(comparison_n), comparison_n),
      reference_value = pairs$reference_value,
      comparison_value = pairs$comparison_value,
      direction = pairs$direction,
      test = rep("brunner_munzel", comparison_n),
      estimand = rep("relative_effect", comparison_n),
      estimate = estimate,
      std_error = std_error,
      effect_size_type = rep("cliffs_delta", comparison_n),
      effect_size = effect_size,
      effect_std_error = effect_std_error,
      effect_size_method = rep(
        "linear_transform_of_relative_effect", comparison_n
      ),
      effect_size_correction = rep("none", comparison_n),
      effect_conf_low = effect_conf_low,
      effect_conf_high = effect_conf_high,
      effect_conf_level = interval_conf_level,
      effect_interval_scope = rep("individual_unadjusted", comparison_n),
      effect_ci_method = paste0(
        "linear_transform_of_",
        vapply(pair_results, function(result) result$ci_method, character(1))
      ),
      effect_ci_clamped = ci_clamped,
      statistic = statistic,
      statistic_type = rep("t", comparison_n),
      df = df,
      p_value = p_value_raw,
      p_value_adjusted = unname(as.double(p_value_adjusted)),
      p_adjust_method = rep(specification$p_adjust_method, comparison_n),
      conf_low = conf_low,
      conf_high = conf_high,
      conf_level = interval_conf_level,
      interval_scope = rep("individual_unadjusted", comparison_n),
      ci_method = vapply(
        pair_results, function(result) result$ci_method, character(1)
      ),
      ci_clamped = ci_clamped,
      inference = rep(specification$inference, comparison_n),
      variance_assumption = rep("not_assumed", comparison_n),
      exact_requested = rep(NA_character_, comparison_n),
      exact_used = rep(NA, comparison_n),
      has_ties = vapply(seq_len(comparison_n), function(position) {
        included <- data$.group %in% c(
          pairs$reference_value[[position]],
          pairs$comparison_value[[position]]
        ) & !is.na(data$.outcome)
        anyDuplicated(data$.outcome[included]) > 0L
      }, logical(1)),
      continuity_correction = rep(NA, comparison_n),
      engine = rep("TOSTER::brunner_munzel", comparison_n)
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
    class = c(
      "bq_brunner_munzel_family", "bq_analysis_function", "function"
    )
  )
}
