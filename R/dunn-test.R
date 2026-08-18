#' Declare a Dunn multiple-comparison test
#'
#' Creates a terminal analytic function for Dunn comparisons through
#' `PMCMRplus::kwAllPairsDunnTest()`. The engine first supplies unadjusted
#' all-pairs statistics; the declared family is then selected and its p-values
#' are adjusted together. This keeps pairwise, reference and consecutive
#' families distinct. No omnibus test is computed or returned.
#'
#' The reported estimate is the difference in pooled mean ranks, oriented as
#' comparison minus reference. Dunn's procedure does not supply confidence
#' intervals, so interval fields are explicitly missing.
#'
#' @param comparisons Comparison family: `"pairwise"`, `"reference"` or
#'   `"consecutive"`.
#' @param reference Reference group value when `comparisons = "reference"`;
#'   otherwise `NULL`.
#' @param p_adjust Multiplicity method passed to [stats::p.adjust()] across the
#'   selected family.
#'
#' @return A `bq_dunn_test` analytic function.
#' @export
dunn_test <- function(
  comparisons = "pairwise",
  reference = NULL,
  p_adjust = "holm"
) {
  allowed_families <- c("pairwise", "reference", "consecutive")
  if (
    !is.character(comparisons) || length(comparisons) != 1L ||
      is.na(comparisons) || !comparisons %in% allowed_families
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`comparisons` must be \"pairwise\", \"reference\" or \"consecutive\"."
    )
  }
  if (comparisons == "reference") {
    if (
      !is.character(reference) || length(reference) != 1L ||
        is.na(reference) || !nzchar(reference)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        "`reference` must be one non-empty group value for a reference family."
      )
    }
  } else if (!is.null(reference)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`reference` must be NULL unless `comparisons = \"reference\"`."
    )
  }
  if (
    !is.character(p_adjust) || length(p_adjust) != 1L || is.na(p_adjust) ||
      !p_adjust %in% stats::p.adjust.methods
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`p_adjust` must be one method supported by `stats::p.adjust()`."
    )
  }
  if (
    !requireNamespace("PMCMRplus", quietly = TRUE) ||
      utils::packageVersion("PMCMRplus") < "1.9.12"
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "`dunn_test()` requires the suggested package `PMCMRplus` version ",
        "1.9.12 or later; install it with `install.packages(\"PMCMRplus\")`."
      )
    )
  }

  specification <- list(
    kind = "dunn_test",
    family = comparisons,
    reference = if (is.null(reference)) NA_character_ else reference,
    p_adjust_method = p_adjust
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
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
    comparison_families = allowed_families,
    supplied_results = "comparison_family",
    supplied_extractors = character(),
    suggested_dependencies = "PMCMRplus (>= 1.9.12)"
  )
  analysis_function <- function(data, context) {
    prepared <- prepare_post_hoc_input(data, context, "dunn_test")
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
    used <- !is.na(data$.outcome)
    engine_result <- tryCatch(
      PMCMRplus::kwAllPairsDunnTest(
        x = data$.outcome[used],
        g = droplevels(data$.group[used]),
        p.adjust.method = "none"
      ),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`dunn_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )
    statistic <- p_value_raw <- numeric(comparison_n)
    for (position in seq_len(comparison_n)) {
      reference_position <- match(
        pairs$reference_value[[position]], group_values
      )
      comparison_position <- match(
        pairs$comparison_value[[position]], group_values
      )
      later_value <- group_values[max(reference_position, comparison_position)]
      earlier_value <- group_values[min(reference_position, comparison_position)]
      stored_statistic <- engine_result$statistic[later_value, earlier_value]
      stored_p_value <- engine_result$p.value[later_value, earlier_value]
      statistic[[position]] <- unname(as.double(stored_statistic)) *
        if (comparison_position > reference_position) 1 else -1
      p_value_raw[[position]] <- unname(as.double(stored_p_value))
    }
    pooled_ranks <- rank(data$.outcome[used], ties.method = "average")
    rank_group <- factor(data$.group[used], levels = group_values)
    mean_rank <- vapply(group_values, function(value) {
      mean(pooled_ranks[rank_group == value])
    }, double(1))
    estimate <- unname(
      mean_rank[pairs$comparison_value] - mean_rank[pairs$reference_value]
    )
    p_value <- stats::p.adjust(
      p_value_raw,
      method = specification$p_adjust_method
    )
    valid_result <- all(is.finite(c(
      estimate, statistic, p_value_raw, p_value
    ))) && all(p_value_raw >= 0 & p_value_raw <= 1) &&
      all(p_value >= 0 & p_value <= 1)
    if (!valid_result) {
      bq_abort(
        "bq_error_analysis_runtime",
        "The Dunn family produced a non-finite comparison result.",
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
      test = rep("dunn", comparison_n),
      estimand = rep("pooled_mean_rank_difference", comparison_n),
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
      statistic = statistic,
      statistic_type = rep("z", comparison_n),
      df = rep(NA_real_, comparison_n),
      p_value = unname(as.double(p_value)),
      p_value_raw = p_value_raw,
      p_adjust_method = rep(specification$p_adjust_method, comparison_n),
      conf_low = rep(NA_real_, comparison_n),
      conf_high = rep(NA_real_, comparison_n),
      conf_level = rep(NA_real_, comparison_n),
      interval_scope = rep("not_computed", comparison_n),
      ci_method = rep("not_computed", comparison_n),
      inference = rep("analytical", comparison_n),
      variance_assumption = rep("not_applicable", comparison_n),
      exact_requested = rep(NA_character_, comparison_n),
      exact_used = rep(NA, comparison_n),
      has_ties = rep(anyDuplicated(data$.outcome[used]) > 0L, comparison_n),
      continuity_correction = rep(NA, comparison_n),
      engine = rep("PMCMRplus::kwAllPairsDunnTest", comparison_n)
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
    class = c("bq_dunn_test", "bq_analysis_function", "function")
  )
}
