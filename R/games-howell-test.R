#' Declare a Games-Howell all-pairs test
#'
#' Creates a terminal analytic function for Games-Howell comparisons through
#' `PMCMRplus::gamesHowellTest()`. The method is an all-pairs procedure for
#' unequal variances; its studentized-range multiplicity policy is intrinsic
#' and cannot be replaced with an unrelated p-value adjustment. No omnibus
#' test is computed or returned.
#'
#' @return A `bq_games_howell_test` analytic function.
#' @export
games_howell_test <- function() {
  check_dependency("PMCMRplus", "`games_howell_test()`", "1.9.12")

  specification <- list(
    kind = "games_howell_test",
    family = "pairwise",
    estimand = "mean_difference",
    p_adjust_method = "games_howell_studentized_range"
  )
  capabilities <- list(
    outcome_types = "continuous",
    group_min_levels = 2L,
    group_max_levels = NA_integer_,
    supplied_results = "comparison_family",
    suggested_dependencies = "PMCMRplus (>= 1.9.12)"
  )
  analysis_function <- function(data, context) {
    prepared <- prepare_engine_input(data, context, "games_howell_test")
    group_values <- prepared$group_values
    pairs <- compile_comparison_family(group_values, "pairwise")
    comparison_n <- nrow(pairs)
    used <- prepared$used
    engine_result <- tryCatch(
      PMCMRplus::gamesHowellTest(
        x = data$.outcome[used],
        g = droplevels(data$.group[used])
      ),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`games_howell_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )
    statistic <- p_value_adjusted <- numeric(comparison_n)
    for (position in seq_len(comparison_n)) {
      later_value <- pairs$comparison_value[[position]]
      earlier_value <- pairs$reference_value[[position]]
      statistic[[position]] <- unname(as.double(
        engine_result$statistic[later_value, earlier_value]
      ))
      p_value_adjusted[[position]] <- unname(as.double(
        engine_result$p.value[later_value, earlier_value]
      ))
    }
    group_mean <- group_variance <- group_n <- numeric(length(group_values))
    names(group_mean) <- names(group_variance) <- names(group_n) <- group_values
    for (value in group_values) {
      observed <- data$.outcome[data$.group == value & !is.na(data$.outcome)]
      group_mean[[value]] <- mean(observed)
      group_variance[[value]] <- stats::var(observed)
      group_n[[value]] <- length(observed)
    }
    estimate <- unname(
      group_mean[pairs$comparison_value] - group_mean[pairs$reference_value]
    )
    df <- vapply(seq_len(comparison_n), function(position) {
      first <- pairs$reference_value[[position]]
      second <- pairs$comparison_value[[position]]
      first_term <- group_variance[[first]] / group_n[[first]]
      second_term <- group_variance[[second]] / group_n[[second]]
      (first_term + second_term)^2 / (
        first_term^2 / (group_n[[first]] - 1) +
          second_term^2 / (group_n[[second]] - 1)
      )
    }, double(1))
    valid_result <- all(is.finite(c(
      estimate, statistic, df, p_value_adjusted
    ))) && all(df > 0) && all(p_value_adjusted >= 0 & p_value_adjusted <= 1)
    if (!valid_result) {
      bq_abort(
        "bq_error_analysis_runtime",
        "Games-Howell comparisons produced a non-finite result.",
        analysis_id = context$analysis_id
      )
    }

    comparisons <- tibble::tibble(
      comparison_id = pairs$comparison_id,
      analysis_id = rep(context$analysis_id, comparison_n),
      outcome_var_id = rep(context$outcome_var_id, comparison_n),
      group_var_id = rep(context$group_var_id, comparison_n),
      family = rep("pairwise", comparison_n),
      family_size = rep(as.integer(comparison_n), comparison_n),
      reference_value = pairs$reference_value,
      comparison_value = pairs$comparison_value,
      direction = pairs$direction,
      test = rep("games_howell", comparison_n),
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
      df = df,
      p_value = rep(NA_real_, comparison_n),
      p_value_adjusted = p_value_adjusted,
      p_adjust_method = rep(
        "games_howell_studentized_range", comparison_n
      ),
      conf_low = rep(NA_real_, comparison_n),
      conf_high = rep(NA_real_, comparison_n),
      conf_level = rep(NA_real_, comparison_n),
      interval_scope = rep("not_computed", comparison_n),
      ci_method = rep("not_computed", comparison_n),
      ci_clamped = rep(NA, comparison_n),
      inference = rep("analytical", comparison_n),
      variance_assumption = rep("unequal", comparison_n),
      exact_requested = rep(NA_character_, comparison_n),
      exact_used = rep(NA, comparison_n),
      has_ties = rep(NA, comparison_n),
      continuity_correction = rep(NA, comparison_n),
      engine = rep("PMCMRplus::gamesHowellTest", comparison_n)
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
    class = c("bq_games_howell_test", "bq_analysis_function", "function")
  )
}
