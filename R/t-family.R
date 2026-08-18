#' Declare a family of parametric t-tests
#'
#' Creates a terminal analytic function for a declared family of two-sided
#' independent t-tests. P-values are adjusted together only across the
#' comparisons present in that family. No omnibus test is computed or returned.
#'
#' Confidence intervals are the individual intervals returned by
#' [stats::t.test()]. They are deliberately marked as unadjusted because
#' general p-value procedures such as Holm or false-discovery-rate control do
#' not define matching simultaneous intervals.
#'
#' @param comparisons Comparison family: `"pairwise"` for every unordered pair,
#'   `"reference"` for every other level versus one reference, or
#'   `"consecutive"` for each level versus the preceding factor level.
#' @param reference Reference group value when `comparisons = "reference"`;
#'   otherwise `NULL`.
#' @param var_equal Whether every pair uses Student's equal-variance t-test.
#'   `FALSE` uses Welch's t-test.
#' @param effect_size Effect size to report for every comparison. One of
#'   `"none"`, `"cohens_d"` and `"hedges_g"`. Student's tests use a pooled
#'   standard deviation and Welch's tests use an unpooled standard deviation.
#' @param p_adjust Multiplicity method passed to [stats::p.adjust()] across the
#'   declared family. Defaults to `"holm"`.
#' @param conf_level Individual confidence level for every mean difference.
#'
#' @return A `bq_t_family` analytic function.
#' @export
#' @examples
#' analysis <- t_family(
#'   comparisons = "reference",
#'   reference = "control",
#'   var_equal = FALSE,
#'   p_adjust = "holm"
#' )
t_family <- function(
  comparisons = "pairwise",
  reference = NULL,
  var_equal = FALSE,
  effect_size = "none",
  p_adjust = "holm",
  conf_level = 0.95
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
  if (!is.logical(var_equal) || length(var_equal) != 1L || is.na(var_equal)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`var_equal` must be either TRUE or FALSE."
    )
  }
  if (
    !is.character(effect_size) || length(effect_size) != 1L ||
      is.na(effect_size) ||
      !effect_size %in% c("none", "cohens_d", "hedges_g")
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`effect_size` must be one of \"none\", \"cohens_d\" and \"hedges_g\"."
    )
  }
  if (
    effect_size != "none" &&
      !requireNamespace("effectsize", quietly = TRUE)
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "The requested `t_family()` effect size requires the suggested ",
        "package `effectsize`; install it with ",
        "`install.packages(\"effectsize\")`."
      )
    )
  }
  if (
    !is.character(p_adjust) || length(p_adjust) != 1L || is.na(p_adjust) ||
      !p_adjust %in% stats::p.adjust.methods
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`p_adjust` must be one method supported by `stats::p.adjust()`: ",
        paste(stats::p.adjust.methods, collapse = ", "), "."
      )
    )
  }
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
    kind = "t_family",
    family = comparisons,
    reference = if (is.null(reference)) NA_character_ else reference,
    var_equal = var_equal,
    effect_size = effect_size,
    p_adjust_method = p_adjust,
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
    comparison_families = allowed_families,
    supplied_results = if (effect_size == "none") {
      "comparison_family"
    } else {
      c("comparison_family", "pairwise_effect_size")
    },
    supplied_extractors = character(),
    suggested_dependencies = if (effect_size == "none") {
      character()
    } else {
      "effectsize"
    }
  )
  analysis_function <- function(data, context) {
    prepared <- prepare_post_hoc_input(data, context, "t_family")
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
    test_results <- vector("list", comparison_n)
    effect_results <- if (specification$effect_size == "none") {
      NULL
    } else {
      vector("list", comparison_n)
    }

    for (position in seq_len(comparison_n)) {
      comparison_value <- pairs$comparison_value[[position]]
      reference_value <- pairs$reference_value[[position]]
      comparison <- data$.outcome[
        data$.group == comparison_value & !is.na(data$.outcome)
      ]
      reference_values <- data$.outcome[
        data$.group == reference_value & !is.na(data$.outcome)
      ]
      if (length(comparison) < 2L || length(reference_values) < 2L) {
        bq_abort(
          "bq_error_invalid_analysis_input",
          paste0(
            "Comparison `", comparison_value, "` versus `", reference_value,
            "` requires at least two observed outcomes in each group."
          ),
          analysis_id = context$analysis_id
        )
      }
      test_results[[position]] <- tryCatch(
        stats::t.test(
          x = comparison,
          y = reference_values,
          alternative = "two.sided",
          var.equal = specification$var_equal,
          conf.level = specification$conf_level
        ),
        error = function(error) {
          bq_abort(
            "bq_error_analysis_runtime",
            paste0(
              "T-test comparison for `", comparison_value, "` versus `",
              reference_value, "` failed: ", conditionMessage(error)
            ),
            analysis_id = context$analysis_id
          )
        }
      )
      if (specification$effect_size != "none") {
        effect_results[[position]] <- tryCatch(
          if (specification$effect_size == "cohens_d") {
            effectsize::cohens_d(
              comparison,
              reference_values,
              pooled_sd = specification$var_equal,
              ci = specification$conf_level,
              verbose = FALSE
            )
          } else {
            effectsize::hedges_g(
              comparison,
              reference_values,
              pooled_sd = specification$var_equal,
              ci = specification$conf_level,
              verbose = FALSE
            )
          },
          error = function(error) {
            bq_abort(
              "bq_error_analysis_runtime",
              paste0(
                "Effect size for `", comparison_value, "` versus `",
                reference_value, "` failed: ", conditionMessage(error)
              ),
              analysis_id = context$analysis_id
            )
          }
        )
      }
    }

    estimate <- vapply(test_results, function(result) {
      unname(as.double(result$estimate[[1L]] - result$estimate[[2L]]))
    }, double(1))
    statistic <- vapply(test_results, function(result) {
      unname(as.double(result$statistic))
    }, double(1))
    std_error <- vapply(test_results, function(result) {
      unname(as.double(result$stderr))
    }, double(1))
    df <- vapply(test_results, function(result) {
      unname(as.double(result$parameter))
    }, double(1))
    p_value_raw <- vapply(test_results, function(result) {
      unname(as.double(result$p.value))
    }, double(1))
    conf_low <- vapply(test_results, function(result) {
      unname(as.double(result$conf.int[[1L]]))
    }, double(1))
    conf_high <- vapply(test_results, function(result) {
      unname(as.double(result$conf.int[[2L]]))
    }, double(1))
    p_value <- stats::p.adjust(
      p_value_raw,
      method = specification$p_adjust_method
    )
    effect_size_type <- rep(NA_character_, comparison_n)
    effect_size_value <- rep(NA_real_, comparison_n)
    effect_size_method <- rep(NA_character_, comparison_n)
    effect_size_correction <- rep(NA_character_, comparison_n)
    effect_conf_low <- rep(NA_real_, comparison_n)
    effect_conf_high <- rep(NA_real_, comparison_n)
    effect_conf_level <- rep(NA_real_, comparison_n)
    effect_interval_scope <- rep(NA_character_, comparison_n)
    effect_ci_method <- rep(NA_character_, comparison_n)
    if (specification$effect_size != "none") {
      effect_column <- if (specification$effect_size == "cohens_d") {
        "Cohens_d"
      } else {
        "Hedges_g"
      }
      effect_size_type <- rep(specification$effect_size, comparison_n)
      effect_size_value <- vapply(effect_results, function(result) {
        unname(as.double(result[[effect_column]]))
      }, double(1))
      effect_size_method <- rep(
        if (specification$var_equal) "pooled_sd" else "unpooled_sd",
        comparison_n
      )
      effect_size_correction <- rep(
        if (specification$effect_size == "hedges_g") {
          "small_sample_bias"
        } else {
          "none"
        },
        comparison_n
      )
      effect_conf_low <- vapply(effect_results, function(result) {
        unname(as.double(result$CI_low))
      }, double(1))
      effect_conf_high <- vapply(effect_results, function(result) {
        unname(as.double(result$CI_high))
      }, double(1))
      effect_conf_level <- rep(specification$conf_level, comparison_n)
      effect_interval_scope <- rep("individual_unadjusted", comparison_n)
      effect_ci_method <- rep("noncentral_t", comparison_n)
    }
    valid_result <- all(is.finite(c(
      estimate, std_error, statistic, df, p_value_raw, p_value,
      conf_low, conf_high
    ))) && all(std_error > 0) && all(df > 0) &&
      all(p_value_raw >= 0 & p_value_raw <= 1) &&
      all(p_value >= 0 & p_value <= 1) && all(conf_low <= conf_high)
    if (specification$effect_size != "none") {
      valid_result <- valid_result && all(is.finite(c(
        effect_size_value, effect_conf_low, effect_conf_high,
        effect_conf_level
      ))) && all(effect_conf_low <= effect_conf_high)
    }
    if (!valid_result) {
      bq_abort(
        "bq_error_analysis_runtime",
        "The t-test family produced a non-finite comparison result.",
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
      test = rep(
        if (specification$var_equal) "student_t" else "welch_t",
        comparison_n
      ),
      estimand = rep("mean_difference", comparison_n),
      estimate = estimate,
      std_error = std_error,
      effect_size_type = effect_size_type,
      effect_size = effect_size_value,
      effect_std_error = rep(NA_real_, comparison_n),
      effect_size_method = effect_size_method,
      effect_size_correction = effect_size_correction,
      effect_conf_low = effect_conf_low,
      effect_conf_high = effect_conf_high,
      effect_conf_level = effect_conf_level,
      effect_interval_scope = effect_interval_scope,
      effect_ci_method = effect_ci_method,
      statistic = statistic,
      statistic_type = rep("t", comparison_n),
      df = df,
      p_value = unname(as.double(p_value)),
      p_value_raw = p_value_raw,
      p_adjust_method = rep(specification$p_adjust_method, comparison_n),
      conf_low = conf_low,
      conf_high = conf_high,
      conf_level = rep(specification$conf_level, comparison_n),
      interval_scope = rep("individual_unadjusted", comparison_n),
      ci_method = rep("student_t_individual", comparison_n),
      inference = rep("analytical", comparison_n),
      variance_assumption = rep(
        if (specification$var_equal) "equal" else "unequal",
        comparison_n
      ),
      exact_requested = rep(NA_character_, comparison_n),
      exact_used = rep(NA, comparison_n),
      has_ties = rep(NA, comparison_n),
      continuity_correction = rep(NA, comparison_n),
      engine = rep("stats::t.test", comparison_n)
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
    class = c("bq_t_family", "bq_analysis_function", "function")
  )
}
