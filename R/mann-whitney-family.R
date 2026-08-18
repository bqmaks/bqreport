#' Declare a family of Mann-Whitney tests
#'
#' Creates a terminal analytic function for a declared family of two-sided
#' Mann-Whitney tests. Every pair is executed by [mann_whitney_test()], so
#' exactness, tie handling, continuity correction and the Hodges-Lehmann
#' estimand follow the same contract as the standalone two-group provider. No
#' omnibus test is computed or returned.
#'
#' P-values are adjusted only across the comparisons in the declared family.
#' Hodges-Lehmann intervals remain individual unadjusted intervals and are
#' labelled accordingly.
#'
#' Every comparison also supplies Cliff's delta, oriented as comparison minus
#' reference and computed directly from the Mann-Whitney statistic. The
#' standard [stats::wilcox.test()] implementation does not supply a confidence
#' interval for Cliff's delta, so its effect-size interval fields remain
#' missing rather than introducing a second interval method.
#'
#' @param comparisons Comparison family: `"pairwise"`, `"reference"` or
#'   `"consecutive"`.
#' @param reference Reference group value when `comparisons = "reference"`;
#'   otherwise `NULL`.
#' @param exact Exactness policy passed to [mann_whitney_test()]: `"auto"`,
#'   `TRUE` or `FALSE`.
#' @param continuity_correction Whether asymptotic pairwise tests use a
#'   continuity correction.
#' @param p_adjust Multiplicity method passed to [stats::p.adjust()] across the
#'   declared family.
#' @param conf_level Individual Hodges-Lehmann confidence level.
#'
#' @return A `bq_mann_whitney_family` analytic function.
#' @export
mann_whitney_family <- function(
  comparisons = "pairwise",
  reference = NULL,
  exact = "auto",
  continuity_correction = TRUE,
  p_adjust = "holm",
  conf_level = 0.95
) {
  reference <- check_comparison_family(comparisons, reference)
  valid_exact <-
    is.logical(exact) && length(exact) == 1L && !is.na(exact) ||
      identical(exact, "auto")
  if (!valid_exact) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`exact` must be \"auto\", TRUE or FALSE."
    )
  }
  check_flag(continuity_correction, "continuity_correction")
  check_p_adjust(p_adjust)
  conf_level <- check_conf_level(conf_level)
  specification <- list(
    kind = "mann_whitney_family",
    family = comparisons,
    reference = reference,
    exact = exact,
    continuity_correction = continuity_correction,
    p_adjust_method = p_adjust,
    conf_level = conf_level
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
    group_min_levels = 2L,
    group_max_levels = NA_integer_,
    supplied_results = c("comparison_family", "pairwise_effect_size"),
    suggested_dependencies = character()
  )
  pair_test <- mann_whitney_test(
    exact = exact,
    continuity_correction = continuity_correction,
    hypothesis = "two_sided",
    conf_level = conf_level
  )

  analysis_function <- function(data, context) {
    prepared <- prepare_engine_input(
      data, context, "mann_whitney_family"
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
    effect_size <- numeric(comparison_n)

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
      comparison_n_used <- sum(
        pair_data$.group == pairs$comparison_value[[position]] &
          !is.na(pair_data$.outcome)
      )
      reference_n_used <- sum(
        pair_data$.group == pairs$reference_value[[position]] &
          !is.na(pair_data$.outcome)
      )
      effect_size[[position]] <-
        2 * pair_results[[position]]$statistic /
          (comparison_n_used * reference_n_used) - 1
    }

    estimate <- vapply(pair_results, function(result) result$raw_estimate, double(1))
    statistic <- vapply(pair_results, function(result) result$statistic, double(1))
    p_value_raw <- vapply(pair_results, function(result) result$p_value, double(1))
    conf_low <- vapply(pair_results, function(result) result$conf_low, double(1))
    conf_high <- vapply(pair_results, function(result) result$conf_high, double(1))
    interval_conf_level <- vapply(
      pair_results,
      function(result) result$interval_conf_level,
      double(1)
    )
    p_value_adjusted <- stats::p.adjust(
      p_value_raw,
      method = specification$p_adjust_method
    )
    valid_result <- all(is.finite(c(
      estimate, statistic, p_value_raw, p_value_adjusted,
      conf_low, conf_high, interval_conf_level, effect_size
    ))) && all(p_value_raw >= 0 & p_value_raw <= 1) &&
      all(p_value_adjusted >= 0 & p_value_adjusted <= 1) && all(conf_low <= conf_high) &&
      all(effect_size >= -1 & effect_size <= 1)
    if (!valid_result) {
      bq_abort(
        "bq_error_analysis_runtime",
        "The Mann-Whitney family produced a non-finite comparison result.",
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
      test = rep("mann_whitney", comparison_n),
      estimand = rep("hodges_lehmann_location_shift", comparison_n),
      estimate = estimate,
      std_error = rep(NA_real_, comparison_n),
      effect_size_type = rep("cliffs_delta", comparison_n),
      effect_size = effect_size,
      effect_std_error = rep(NA_real_, comparison_n),
      effect_size_method = rep(
        "linear_transform_of_mann_whitney_u", comparison_n
      ),
      effect_size_correction = rep("none", comparison_n),
      effect_conf_low = rep(NA_real_, comparison_n),
      effect_conf_high = rep(NA_real_, comparison_n),
      effect_conf_level = rep(NA_real_, comparison_n),
      effect_interval_scope = rep(NA_character_, comparison_n),
      effect_ci_method = rep(NA_character_, comparison_n),
      effect_ci_clamped = rep(NA, comparison_n),
      statistic = statistic,
      statistic_type = rep("wilcoxon_W", comparison_n),
      df = rep(NA_real_, comparison_n),
      p_value = p_value_raw,
      p_value_adjusted = unname(as.double(p_value_adjusted)),
      p_adjust_method = rep(specification$p_adjust_method, comparison_n),
      conf_low = conf_low,
      conf_high = conf_high,
      conf_level = interval_conf_level,
      interval_scope = rep("individual_unadjusted", comparison_n),
      ci_method = vapply(pair_results, function(result) result$ci_method, character(1)),
      ci_clamped = rep(FALSE, comparison_n),
      inference = rep("analytical", comparison_n),
      variance_assumption = rep("not_applicable", comparison_n),
      exact_requested = vapply(
        pair_results, function(result) result$exact_requested, character(1)
      ),
      exact_used = vapply(pair_results, function(result) result$exact_used, logical(1)),
      has_ties = vapply(pair_results, function(result) result$has_ties, logical(1)),
      continuity_correction = vapply(
        pair_results,
        function(result) result$continuity_correction,
        logical(1)
      ),
      engine = rep("stats::wilcox.test", comparison_n)
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
      "bq_mann_whitney_family", "bq_analysis_function", "function"
    )
  )
}
