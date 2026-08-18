#' Declare an independent two-sample t-test
#'
#' Creates an analytic function with its variance assumption and confidence
#' level fixed before analysis. The returned function is executed later by an
#' analysis plan with data and a compiled analysis context.
#'
#' The analytic function supplies only the test result. It does not return a
#' fitted model and cannot supply within-group estimates or extractors.
#'
#' @param var_equal Whether to assume equal group variances. `FALSE` declares
#'   Welch's two-sample t-test; `TRUE` declares Student's two-sample t-test.
#' @param hypothesis Hypothesis type. One of `"two_sided"`, `"equivalence"`,
#'   `"noninferiority"` and `"superiority"`.
#' @param margin Hypothesis margin in outcome units. Must be `NULL` for a
#'   two-sided test, one positive number for noninferiority or equivalence, one
#'   non-negative number for superiority, or a named `c(lower, upper)` vector
#'   spanning zero for asymmetric equivalence bounds.
#' @param benefit Which outcome direction is beneficial for noninferiority and
#'   superiority: `"higher"` or `"lower"`. Must be `NULL` for two-sided and
#'   equivalence tests.
#' @param effect_size Effect size to report. One of `"none"`, `"cohens_d"`
#'   and `"hedges_g"`. Student's test uses a pooled standard deviation and
#'   Welch's test uses an unpooled standard deviation.
#' @param conf_level Confidence level for the mean-difference interval. Must be
#'   one finite number strictly between zero and one.
#'
#' @return A `bq_t_test` analytic function.
#' @export
#' @examples
#' analysis <- t_test(
#'   var_equal = FALSE,
#'   hypothesis = "two_sided",
#'   effect_size = "none",
#'   conf_level = 0.95
#' )
#' analysis
t_test <- function(
  var_equal = FALSE,
  hypothesis = "two_sided",
  margin = NULL,
  benefit = NULL,
  effect_size = "none",
  conf_level = 0.95
) {
  if (
    !is.logical(var_equal) || length(var_equal) != 1L || is.na(var_equal)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`var_equal` must be either TRUE or FALSE."
    )
  }

  allowed_hypotheses <- c(
    "two_sided", "equivalence", "noninferiority", "superiority"
  )
  if (
    !is.character(hypothesis) || length(hypothesis) != 1L ||
      is.na(hypothesis) || !hypothesis %in% allowed_hypotheses
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`hypothesis` must be one of \"two_sided\", \"equivalence\", ",
        "\"noninferiority\" and \"superiority\"."
      )
    )
  }

  margin_lower <- NA_real_
  margin_upper <- NA_real_
  if (hypothesis == "two_sided") {
    if (!is.null(margin)) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        "`margin` must be NULL for a two-sided test."
      )
    }
  } else if (hypothesis == "equivalence") {
    valid_scalar_margin <- is.numeric(margin) && length(margin) == 1L &&
      !is.na(margin) && is.finite(margin) && margin > 0
    valid_bounds <- is.numeric(margin) && length(margin) == 2L &&
      identical(names(margin), c("lower", "upper")) &&
      !anyNA(margin) && all(is.finite(margin)) &&
      margin[["lower"]] < 0 && margin[["upper"]] > 0
    if (!valid_scalar_margin && !valid_bounds) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        paste0(
          "Equivalence `margin` must be one positive number or a named ",
          "`c(lower, upper)` vector with `lower < 0 < upper`."
        )
      )
    }
    if (valid_scalar_margin) {
      margin_lower <- -as.double(margin)
      margin_upper <- as.double(margin)
    } else {
      margin_lower <- as.double(margin[["lower"]])
      margin_upper <- as.double(margin[["upper"]])
    }
  } else if (hypothesis == "noninferiority") {
    if (
      !is.numeric(margin) || length(margin) != 1L || is.na(margin) ||
        !is.finite(margin) || margin <= 0
    ) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        "Noninferiority `margin` must be one positive finite number."
      )
    }
    margin_lower <- -as.double(margin)
  } else {
    if (
      !is.numeric(margin) || length(margin) != 1L || is.na(margin) ||
        !is.finite(margin) || margin < 0
    ) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        "Superiority `margin` must be one non-negative finite number."
      )
    }
    margin_lower <- as.double(margin)
  }

  directional_hypothesis <- hypothesis %in% c("noninferiority", "superiority")
  if (directional_hypothesis) {
    if (
      !is.character(benefit) || length(benefit) != 1L || is.na(benefit) ||
        !benefit %in% c("higher", "lower")
    ) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        paste0(
          "`benefit` must be \"higher\" or \"lower\" for ", hypothesis,
          "."
        )
      )
    }
  } else if (!is.null(benefit)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`benefit` must be NULL for two-sided and equivalence tests."
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
        "The requested `t_test()` effect size requires the suggested ",
        "package `effectsize`; install it with ",
        "`install.packages(\"effectsize\")`."
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
  if (hypothesis == "equivalence" && conf_level <= 0.5) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`conf_level` must be greater than 0.5 for an equivalence test."
    )
  }

  specification <- list(
    kind = "t_test",
    var_equal = var_equal,
    hypothesis = hypothesis,
    margin_lower = margin_lower,
    margin_upper = margin_upper,
    benefit = if (is.null(benefit)) NA_character_ else benefit,
    effect_size = effect_size,
    conf_level = as.double(conf_level)
  )
  supplied_results <- "test"
  suggested_dependencies <- character()
  if (effect_size != "none") {
    supplied_results <- c(supplied_results, "effect_size")
    suggested_dependencies <- "effectsize"
  }
  capabilities <- list(
    outcome_types = "continuous",
    outcomes_per_analysis = 1L,
    requires_group = TRUE,
    group_min_levels = 2L,
    group_max_levels = 2L,
    max_strata = 0L,
    supports_covariates = FALSE,
    supports_weights = FALSE,
    supports_clusters = FALSE,
    supports_matched_sets = FALSE,
    provides_fits = FALSE,
    supplied_results = supplied_results,
    supplied_extractors = character(),
    suggested_dependencies = suggested_dependencies
  )

  analysis_function <- function(data, context) {
    if (
      !tibble::is_tibble(data) ||
        !identical(names(data), c(".row_id", ".outcome", ".group"))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`data` for `t_test()` must be a tibble with columns `.row_id`, ",
          "`.outcome` and `.group`, in that order."
        )
      )
    }

    if (
      anyNA(data$.row_id) || anyDuplicated(data$.row_id) ||
        !is.atomic(data$.row_id) || !is.null(dim(data$.row_id))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`.row_id` must contain unique, non-missing atomic values."
      )
    }

    if (
      !is.numeric(data$.outcome) || is.object(data$.outcome) ||
        !is.null(dim(data$.outcome))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`.outcome` must be one plain numeric vector."
      )
    }

    if (!is.factor(data$.group) || anyNA(data$.group)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`.group` must be a factor without missing values."
      )
    }

    required_context <- c(
      "analysis_id", "test_id", "estimate_id", "outcome_var_id", "group_var_id",
      "strata_var_id", "reference_value", "group_levels"
    )
    if (!is.list(context) || !identical(names(context), required_context)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`context` for `t_test()` must contain `analysis_id`, `test_id`, `estimate_id`, ",
          "`outcome_var_id`, `group_var_id`, `strata_var_id`, ",
          "`reference_value` and `group_levels`, in that order."
        )
      )
    }

    identifiers <- context[c(
      "analysis_id", "test_id", "outcome_var_id", "group_var_id"
    )]
    valid_identifiers <- vapply(
      identifiers,
      function(value) {
        is.character(value) && length(value) == 1L && !is.na(value) &&
          nzchar(value)
      },
      logical(1)
    )
    if (!all(valid_identifiers)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "Analysis, test and variable IDs must be non-empty character scalars."
      )
    }

    valid_estimate_id <- is.character(context$estimate_id) &&
      length(context$estimate_id) == 1L &&
      if (specification$effect_size == "none") {
        is.na(context$estimate_id)
      } else {
        !is.na(context$estimate_id) && nzchar(context$estimate_id)
      }
    if (!valid_estimate_id) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`estimate_id` must be NA when no effect size is requested and a ",
          "non-empty character scalar otherwise."
        )
      )
    }

    if (
      !is.character(context$strata_var_id) ||
        length(context$strata_var_id) != 1L ||
        !is.na(context$strata_var_id)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`strata_var_id` must be NA because `t_test()` does not support strata."
      )
    }

    if (
      !tibble::is_tibble(context$group_levels) ||
        !identical(names(context$group_levels), c("var_id", "value", "position")) ||
        !is.character(context$group_levels$var_id) ||
        !is.character(context$group_levels$value) ||
        !is.integer(context$group_levels$position) ||
        anyNA(context$group_levels) ||
        !identical(
          context$group_levels$var_id,
          rep(context$group_var_id, nrow(context$group_levels))
        ) ||
        !identical(
          context$group_levels$position,
          seq_len(nrow(context$group_levels))
        ) ||
        !identical(context$group_levels$value, levels(data$.group))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`group_levels` must describe every `.group` level once, in factor ",
          "order, for `group_var_id`."
        )
      )
    }

    if (nlevels(data$.group) != 2L) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`t_test()` requires exactly two declared group levels."
      )
    }

    if (
      !is.character(context$reference_value) ||
        length(context$reference_value) != 1L ||
        is.na(context$reference_value) ||
        !context$reference_value %in% levels(data$.group)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`reference_value` must identify one declared `.group` level."
      )
    }

    group_values <- levels(data$.group)
    comparison_value <- setdiff(group_values, context$reference_value)
    missing_outcome <- is.na(data$.outcome)
    n_total <- vapply(
      group_values,
      function(value) sum(data$.group == value),
      integer(1)
    )
    n_missing <- vapply(
      group_values,
      function(value) sum(data$.group == value & missing_outcome),
      integer(1)
    )
    n_used <- n_total - n_missing

    if (specification$var_equal) {
      enough_observations <- all(n_used >= 1L) && sum(n_used) >= 3L
    } else {
      enough_observations <- all(n_used >= 2L)
    }
    if (!enough_observations) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`t_test()` has too few observed outcome values for the declared ",
          if (specification$var_equal) "Student" else "Welch",
          " test."
        )
      )
    }

    comparison <- data$.outcome[
      data$.group == comparison_value & !missing_outcome
    ]
    reference <- data$.outcome[
      data$.group == context$reference_value & !missing_outcome
    ]
    raw_estimate <- mean(comparison) - mean(reference)
    benefit_sign <- if (
      specification$hypothesis %in% c("noninferiority", "superiority") &&
        specification$benefit == "lower"
    ) {
      -1
    } else {
      1
    }
    test_comparison <- benefit_sign * comparison
    test_reference <- benefit_sign * reference

    test_results <- tryCatch(
      if (specification$hypothesis == "two_sided") {
        list(
          primary = stats::t.test(
            x = comparison,
            y = reference,
            var.equal = specification$var_equal,
            conf.level = specification$conf_level
          )
        )
      } else if (specification$hypothesis %in% c("noninferiority", "superiority")) {
        list(
          primary = stats::t.test(
            x = test_comparison,
            y = test_reference,
            alternative = "greater",
            mu = specification$margin_lower,
            var.equal = specification$var_equal,
            conf.level = specification$conf_level
          )
        )
      } else {
        list(
          lower = stats::t.test(
            x = comparison,
            y = reference,
            alternative = "greater",
            mu = specification$margin_lower,
            var.equal = specification$var_equal,
            conf.level = specification$conf_level
          ),
          upper = stats::t.test(
            x = comparison,
            y = reference,
            alternative = "less",
            mu = specification$margin_upper,
            var.equal = specification$var_equal,
            conf.level = specification$conf_level
          ),
          interval = stats::t.test(
            x = comparison,
            y = reference,
            var.equal = specification$var_equal,
            conf.level = 2 * specification$conf_level - 1
          )
        )
      },
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`t_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )

    if (specification$hypothesis == "equivalence") {
      estimate <- raw_estimate
      benefit_estimate <- NA_real_
      std_error <- unname(as.double(test_results$lower$stderr))
      statistic <- NA_real_
      statistic_lower <- unname(as.double(test_results$lower$statistic))
      statistic_upper <- unname(as.double(test_results$upper$statistic))
      df <- unname(as.double(test_results$lower$parameter))
      p_lower <- unname(as.double(test_results$lower$p.value))
      p_upper <- unname(as.double(test_results$upper$p.value))
      p_value <- max(p_lower, p_upper)
      conf_low <- unname(as.double(test_results$interval$conf.int[1L]))
      conf_high <- unname(as.double(test_results$interval$conf.int[2L]))
      interval_conf_level <- 2 * specification$conf_level - 1
    } else {
      primary <- test_results$primary
      directional <- specification$hypothesis %in% c(
        "noninferiority", "superiority"
      )
      estimate <- if (directional) benefit_sign * raw_estimate else raw_estimate
      benefit_estimate <- if (directional) estimate else NA_real_
      std_error <- unname(as.double(primary$stderr))
      statistic <- unname(as.double(primary$statistic))
      statistic_lower <- NA_real_
      statistic_upper <- NA_real_
      df <- unname(as.double(primary$parameter))
      p_lower <- if (directional) unname(as.double(primary$p.value)) else NA_real_
      p_upper <- NA_real_
      p_value <- unname(as.double(primary$p.value))
      conf_low <- unname(as.double(primary$conf.int[1L]))
      conf_high <- unname(as.double(primary$conf.int[2L]))
      interval_conf_level <- specification$conf_level
    }

    tests <- tibble::tibble(
      test_id = context$test_id,
      analysis_id = context$analysis_id,
      outcome_var_id = context$outcome_var_id,
      test = if (specification$var_equal) "student_t_test" else "welch_t_test",
      reference_value = context$reference_value,
      comparison_value = comparison_value,
      hypothesis = specification$hypothesis,
      benefit = specification$benefit,
      margin_lower = specification$margin_lower,
      margin_upper = specification$margin_upper,
      raw_estimate = unname(as.double(raw_estimate)),
      benefit_estimate = unname(as.double(benefit_estimate)),
      estimate = unname(as.double(estimate)),
      std_error = std_error,
      statistic = statistic,
      statistic_lower = statistic_lower,
      statistic_upper = statistic_upper,
      df = df,
      conf_low = conf_low,
      conf_high = conf_high,
      p_value = p_value,
      p_lower = p_lower,
      p_upper = p_upper,
      requested_conf_level = specification$conf_level,
      interval_conf_level = interval_conf_level
    )
    if (specification$effect_size == "none") {
      estimates <- tibble::tibble(
        estimate_id = character(),
        analysis_id = character(),
        outcome_var_id = character(),
        estimand = character(),
        reference_value = character(),
        comparison_value = character(),
        standardizer = character(),
        estimate = double(),
        std_error = double(),
        conf_low = double(),
        conf_high = double(),
        ci_method = character()
      )
    } else {
      effect_result <- tryCatch(
        if (specification$effect_size == "cohens_d") {
          effectsize::cohens_d(
            comparison,
            reference,
            pooled_sd = specification$var_equal,
            ci = specification$conf_level,
            verbose = FALSE
          )
        } else {
          effectsize::hedges_g(
            comparison,
            reference,
            pooled_sd = specification$var_equal,
            ci = specification$conf_level,
            verbose = FALSE
          )
        },
        error = function(error) {
          bq_abort(
            "bq_error_analysis_runtime",
            paste0("`t_test()` failed to compute its effect size: ", conditionMessage(error)),
            analysis_id = context$analysis_id
          )
        }
      )
      effect_column <- if (specification$effect_size == "cohens_d") {
        "Cohens_d"
      } else {
        "Hedges_g"
      }
      estimates <- tibble::tibble(
        estimate_id = context$estimate_id,
        analysis_id = context$analysis_id,
        outcome_var_id = context$outcome_var_id,
        estimand = specification$effect_size,
        reference_value = context$reference_value,
        comparison_value = comparison_value,
        standardizer = if (specification$var_equal) "pooled_sd" else "unpooled_sd",
        estimate = unname(as.double(effect_result[[effect_column]])),
        std_error = NA_real_,
        conf_low = unname(as.double(effect_result$CI_low)),
        conf_high = unname(as.double(effect_result$CI_high)),
        ci_method = "noncentral_t"
      )
    }
    sample_flow <- tibble::tibble(
      analysis_id = rep(context$analysis_id, length(group_values)),
      outcome_var_id = rep(context$outcome_var_id, length(group_values)),
      group_value = group_values,
      n_total = unname(n_total),
      n_missing = unname(n_missing),
      n_used = unname(n_used)
    )

    list(
      tests = tests,
      estimates = estimates,
      sample_flow = sample_flow
    )
  }

  structure(
    analysis_function,
    specification = specification,
    capabilities = capabilities,
    class = c("bq_t_test", "bq_analysis_function", "function")
  )
}
