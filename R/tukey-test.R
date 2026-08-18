#' Declare a Tukey all-pairs test
#'
#' Creates a terminal analytic function for a classical one-way ANOVA followed
#' by Tukey's honestly significant difference procedure. The comparison
#' family always contains every unordered pair of declared group levels.
#' Estimates are oriented as the later factor level minus the earlier factor
#' level. P-values and confidence intervals control the family-wise error rate
#' through the studentized-range distribution.
#'
#' The one-way model is fitted only to obtain Tukey comparisons. No omnibus
#' test is computed or returned.
#'
#' @param conf_level Family-wise confidence level. Must be one finite number
#'   strictly between zero and one.
#'
#' @return A `bq_tukey_test` analytic function.
#' @export
#' @examples
#' analysis <- tukey_test(conf_level = 0.95)
#' run_comparison(
#'   analysis,
#'   outcome = c(8, 10, 12, 1, 2, 6, 4, 5, 7),
#'   group = rep(c("new", "control", "other"), each = 3)
#' )
tukey_test <- function(conf_level = 0.95) {
  if (
    !is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`conf_level` must be one finite number strictly between zero and one."
    )
  }

  specification <- list(
    kind = "tukey_test",
    family = "pairwise",
    estimand = "mean_difference",
    p_adjust_method = "tukey_single_step",
    conf_level = as.double(conf_level)
  )
  capabilities <- list(
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
    supplied_results = "comparison_family",
    supplied_extractors = character(),
    suggested_dependencies = character()
  )
  analysis_function <- function(data, context) {
    prepared <- prepare_post_hoc_input(data, context, "tukey_test")
    used <- prepared$used
    model_data <- data.frame(
      .outcome = data$.outcome[used],
      .group = data$.group[used]
    )
    fit <- tryCatch(
      stats::aov(.outcome ~ .group, data = model_data),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`tukey_test()` failed to fit its model: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )
    tukey_result <- tryCatch(
      stats::TukeyHSD(fit, ".group", conf.level = specification$conf_level),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`tukey_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )[[".group"]]

    group_values <- prepared$group_values
    pairs <- utils::combn(group_values, 2L)
    expected_rows <- ncol(pairs)
    if (
      !is.matrix(tukey_result) || nrow(tukey_result) != expected_rows ||
        !identical(colnames(tukey_result), c("diff", "lwr", "upr", "p adj"))
    ) {
      bq_abort(
        "bq_error_analysis_runtime",
        "`tukey_test()` returned an unexpected comparison schema.",
        analysis_id = context$analysis_id
      )
    }

    estimate <- unname(as.double(tukey_result[, "diff"]))
    conf_low <- unname(as.double(tukey_result[, "lwr"]))
    conf_high <- unname(as.double(tukey_result[, "upr"]))
    p_value <- unname(as.double(tukey_result[, "p adj"]))
    residual_df <- unname(as.double(stats::df.residual(fit)))
    residual_variance <- unname(as.double(stats::deviance(fit) / residual_df))
    n_used <- table(factor(model_data$.group, levels = group_values))
    tukey_scale <- sqrt(
      residual_variance / 2 *
        (1 / as.double(n_used[pairs[1L, ]]) +
          1 / as.double(n_used[pairs[2L, ]]))
    )
    std_error <- sqrt(2) * tukey_scale
    statistic <- abs(estimate) / tukey_scale
    valid_result <- all(is.finite(c(
      estimate, std_error, conf_low, conf_high, p_value, statistic,
      residual_df, residual_variance
    ))) && residual_df > 0 && residual_variance > 0 &&
      all(std_error > 0) && all(conf_low <= conf_high) &&
      all(p_value >= 0 & p_value <= 1)
    if (!valid_result) {
      bq_abort(
        "bq_error_analysis_runtime",
        paste0(
          "`tukey_test()` could not compute finite simultaneous comparisons; ",
          "each group must provide enough within-group variation."
        ),
        analysis_id = context$analysis_id
      )
    }

    comparisons <- tibble::tibble(
      comparison_id = sprintf("cmp%03d", seq_len(expected_rows)),
      analysis_id = rep(context$analysis_id, expected_rows),
      outcome_var_id = rep(context$outcome_var_id, expected_rows),
      group_var_id = rep(context$group_var_id, expected_rows),
      family = rep("pairwise", expected_rows),
      family_size = rep(as.integer(expected_rows), expected_rows),
      reference_value = unname(pairs[1L, ]),
      comparison_value = unname(pairs[2L, ]),
      direction = rep("comparison_minus_reference", expected_rows),
      test = rep("tukey_hsd", expected_rows),
      estimand = rep("mean_difference", expected_rows),
      estimate = estimate,
      std_error = std_error,
      effect_size_type = rep(NA_character_, expected_rows),
      effect_size = rep(NA_real_, expected_rows),
      effect_std_error = rep(NA_real_, expected_rows),
      effect_size_method = rep(NA_character_, expected_rows),
      effect_size_correction = rep(NA_character_, expected_rows),
      effect_conf_low = rep(NA_real_, expected_rows),
      effect_conf_high = rep(NA_real_, expected_rows),
      effect_conf_level = rep(NA_real_, expected_rows),
      effect_interval_scope = rep(NA_character_, expected_rows),
      effect_ci_method = rep(NA_character_, expected_rows),
      statistic = statistic,
      statistic_type = rep("studentized_range", expected_rows),
      df = rep(residual_df, expected_rows),
      p_value = p_value,
      p_value_raw = rep(NA_real_, expected_rows),
      p_adjust_method = rep("tukey_single_step", expected_rows),
      conf_low = conf_low,
      conf_high = conf_high,
      conf_level = rep(specification$conf_level, expected_rows),
      interval_scope = rep("familywise_simultaneous", expected_rows),
      ci_method = rep("tukey_studentized_range", expected_rows),
      inference = rep("analytical", expected_rows),
      variance_assumption = rep("equal", expected_rows),
      exact_requested = rep(NA_character_, expected_rows),
      exact_used = rep(NA, expected_rows),
      has_ties = rep(NA, expected_rows),
      continuity_correction = rep(NA, expected_rows),
      engine = rep("stats::TukeyHSD", expected_rows)
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
    class = c("bq_tukey_test", "bq_analysis_function", "function")
  )
}
