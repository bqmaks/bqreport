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
#' @param inference `"analytical"` for the usual t distribution or
#'   `"permutation"` for a studentized randomization test.
#' @param permutation A [permutation_control()] specification when
#'   `inference = "permutation"`; otherwise `NULL`.
#' @param bootstrap `NULL` for the test-engine interval or a
#'   [bootstrap_control()] specification for ordinary or fractional weighted
#'   bootstrap intervals around the mean difference and any requested effect
#'   size.
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
  inference = "analytical",
  permutation = NULL,
  bootstrap = NULL,
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
  valid_bootstrap <- is.null(bootstrap) || (
    identical(class(bootstrap), "bq_bootstrap_control") &&
      identical(
        names(bootstrap),
        c("method", "engine", "iterations", "conf_type", "seed", "weight_type")
      ) &&
      is.character(bootstrap$method) && length(bootstrap$method) == 1L &&
      !is.na(bootstrap$method) &&
      bootstrap$method %in% c("ordinary", "fractional") &&
      identical(
        bootstrap$engine,
        if (identical(bootstrap$method, "ordinary")) "boot" else "fwb"
      ) &&
      is.integer(bootstrap$iterations) && length(bootstrap$iterations) == 1L &&
      !is.na(bootstrap$iterations) && bootstrap$iterations > 0L &&
      is.character(bootstrap$conf_type) && length(bootstrap$conf_type) == 1L &&
      !is.na(bootstrap$conf_type) &&
      bootstrap$conf_type %in% c("bca", "percentile", "basic") &&
      (is.null(bootstrap$seed) || (
        is.integer(bootstrap$seed) && length(bootstrap$seed) == 1L &&
          !is.na(bootstrap$seed) && bootstrap$seed >= 0L
      )) &&
      if (identical(bootstrap$method, "ordinary")) {
        is.null(bootstrap$weight_type)
      } else {
        identical(bootstrap$weight_type, "exponential")
      }
  )
  if (!valid_bootstrap) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`bootstrap` must be NULL or a valid specification from ",
        "`bootstrap_control()`."
      )
    )
  }
  if (
    !is.null(bootstrap) && bootstrap$method == "ordinary" &&
      !requireNamespace("boot", quietly = TRUE)
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      "Ordinary bootstrap requires the suggested package `boot`."
    )
  }
  if (
    !is.null(bootstrap) && bootstrap$method == "fractional" &&
      !requireNamespace("fwb", quietly = TRUE)
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      "Fractional weighted bootstrap requires the suggested package `fwb`."
    )
  }

  if (
    !is.character(inference) || length(inference) != 1L || is.na(inference) ||
      !inference %in% c("analytical", "permutation")
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`inference` must be either \"analytical\" or \"permutation\"."
    )
  }
  valid_permutation <- is.null(permutation) || (
    identical(class(permutation), "bq_permutation_control") &&
      identical(
        names(permutation), c("sampling", "iterations", "p_method", "seed")
      ) &&
      identical(permutation$sampling, "random") &&
      is.integer(permutation$iterations) && length(permutation$iterations) == 1L &&
      !is.na(permutation$iterations) && permutation$iterations > 0L &&
      identical(permutation$p_method, "plusone") &&
      (is.null(permutation$seed) || (
        is.integer(permutation$seed) && length(permutation$seed) == 1L &&
          !is.na(permutation$seed) && permutation$seed >= 0L
      ))
  )
  if (!valid_permutation) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`permutation` must be NULL or a valid `permutation_control()` specification."
    )
  }
  if (inference == "permutation" && is.null(permutation)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`permutation` is required when `inference = \"permutation\"`."
    )
  }
  if (inference != "permutation" && !is.null(permutation)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`permutation` must be NULL unless `inference = \"permutation\"`."
    )
  }
  if (
    inference == "permutation" &&
      (!requireNamespace("TOSTER", quietly = TRUE) ||
        utils::packageVersion("TOSTER") < "0.9.0")
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      "Permutation t-tests require the suggested package `TOSTER` 0.9.0 or later."
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
    inference = inference,
    permutation = permutation,
    bootstrap = bootstrap,
    conf_level = as.double(conf_level)
  )
  supplied_results <- "test"
  suggested_dependencies <- character()
  if (effect_size != "none") {
    supplied_results <- c(supplied_results, "effect_size")
    suggested_dependencies <- "effectsize"
  }
  if (inference == "permutation") {
    suggested_dependencies <- unique(c(
      suggested_dependencies, "TOSTER (>= 0.9.0)"
    ))
  }
  if (!is.null(bootstrap)) {
    suggested_dependencies <- unique(c(
      suggested_dependencies,
      bootstrap$engine
    ))
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

    permutation_result <- NULL
    if (specification$inference == "permutation") {
      possible_partitions <- choose(
        length(comparison) + length(reference), length(comparison)
      )
      if (specification$permutation$iterations >= possible_partitions) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0(
            "The requested number of random permutations reaches all ",
            "possible group partitions; reduce `iterations` because exact ",
            "enumeration is not part of `permutation_control()`."
          ),
          analysis_id = context$analysis_id
        )
      }
      alternative <- if (specification$hypothesis == "two_sided") {
        "two.sided"
      } else if (specification$hypothesis == "equivalence") {
        "equivalence"
      } else {
        "greater"
      }
      null_value <- if (specification$hypothesis == "equivalence") {
        c(specification$margin_lower, specification$margin_upper)
      } else if (specification$hypothesis %in% c("noninferiority", "superiority")) {
        specification$margin_lower
      } else {
        0
      }
      permutation_result <- with_resampling_seed(
        specification$permutation$seed,
        tryCatch(
          suppressMessages(TOSTER::perm_t_test(
            test_comparison, test_reference, alternative = alternative,
            mu = null_value, var.equal = specification$var_equal,
            alpha = 1 - specification$conf_level,
            R = specification$permutation$iterations,
            p_method = specification$permutation$p_method,
            keep_perm = FALSE
          )),
          error = function(error) {
            bq_abort(
              "bq_error_analysis_runtime",
              paste0(
                "Permutation `t_test()` failed: ",
                conditionMessage(error)
              ),
              analysis_id = context$analysis_id
            )
          }
        )
      )
    }

    test_results <- if (specification$inference == "permutation") NULL else tryCatch(
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

    if (specification$inference == "permutation") {
      directional <- specification$hypothesis %in% c(
        "noninferiority", "superiority"
      )
      estimate <- if (directional) benefit_sign * raw_estimate else raw_estimate
      benefit_estimate <- if (directional) estimate else NA_real_
      std_error <- unname(as.double(permutation_result$stderr))
      statistic <- unname(as.double(permutation_result$statistic))
      statistic_lower <- statistic_upper <- NA_real_
      df <- unname(as.double(permutation_result$parameter))
      p_value <- unname(as.double(permutation_result$p.value))
      p_lower <- if (directional) p_value else NA_real_
      p_upper <- NA_real_
      conf_low <- unname(as.double(permutation_result$conf.int[1L]))
      conf_high <- unname(as.double(permutation_result$conf.int[2L]))
      interval_conf_level <- unname(as.double(
        attr(permutation_result$conf.int, "conf.level")
      ))
    } else if (specification$hypothesis == "equivalence") {
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

    test_ci_method <- if (specification$inference == "permutation") {
      "TOSTER_permutation"
    } else {
      "t_distribution"
    }
    bootstrap_iterations_valid <- NA_integer_
    bootstrap_effect_std_error <- NA_real_
    bootstrap_effect_conf_low <- bootstrap_effect_conf_high <- NA_real_
    if (!is.null(specification$bootstrap)) {
      bootstrap_data <- data.frame(
        outcome = c(comparison, reference),
        group = factor(
          c(rep("comparison", length(comparison)), rep("reference", length(reference))),
          levels = c("comparison", "reference")
        )
      )
      weighted_statistics <- function(x, y, x_weights, y_weights) {
        x_weights <- x_weights / sum(x_weights)
        y_weights <- y_weights / sum(y_weights)
        x_mean <- sum(x_weights * x)
        y_mean <- sum(y_weights * y)
        difference <- x_mean - y_mean
        reported_difference <- if (benefit_sign < 0) -difference else difference
        if (specification$effect_size == "none") return(reported_difference)
        s1_squared <- sum(x_weights * (x - x_mean)^2) *
          length(x) / (length(x) - 1)
        s2_squared <- sum(y_weights * (y - y_mean)^2) *
          length(y) / (length(y) - 1)
        if (specification$var_equal) {
          effect_sd <- sqrt(
            ((length(x) - 1) * s1_squared +
              (length(y) - 1) * s2_squared) /
              (length(x) + length(y) - 2)
          )
          effect_df <- length(x) + length(y) - 2
        } else {
          effect_sd <- sqrt((s1_squared + s2_squared) / 2)
          se1_squared <- s1_squared / length(x)
          se2_squared <- s2_squared / length(y)
          effect_df <- (se1_squared + se2_squared)^2 /
            (se1_squared^2 / (length(x) - 1) +
              se2_squared^2 / (length(y) - 1))
        }
        effect <- difference / effect_sd
        if (specification$effect_size == "hedges_g") {
          correction <- exp(
            lgamma(effect_df / 2) - log(sqrt(effect_df / 2)) -
              lgamma((effect_df - 1) / 2)
          )
          effect <- effect * correction
        }
        c(reported_difference, effect)
      }
      if (specification$bootstrap$method == "ordinary") {
        ordinary_statistic <- function(bootstrap_data, indices) {
          sampled <- bootstrap_data[indices, , drop = FALSE]
          comparison_rows <- sampled$group == "comparison"
          x <- sampled$outcome[comparison_rows]
          y <- sampled$outcome[!comparison_rows]
          weighted_statistics(x, y, rep(1, length(x)), rep(1, length(y)))
        }
        bootstrap_result <- with_resampling_seed(
          specification$bootstrap$seed,
          tryCatch(
            boot::boot(
              bootstrap_data, ordinary_statistic,
              R = specification$bootstrap$iterations,
              sim = "ordinary", stype = "i", strata = bootstrap_data$group
            ),
            error = function(error) {
              bq_abort(
                "bq_error_analysis_runtime",
                paste0(
                  "Ordinary bootstrap for `t_test()` failed: ",
                  conditionMessage(error)
                ),
                analysis_id = context$analysis_id
              )
            }
          )
        )
      } else {
        fractional_statistic <- function(bootstrap_data, weights) {
          comparison_rows <- bootstrap_data$group == "comparison"
          weighted_statistics(
            bootstrap_data$outcome[comparison_rows],
            bootstrap_data$outcome[!comparison_rows],
            weights[comparison_rows], weights[!comparison_rows]
          )
        }
        bootstrap_result <- with_resampling_seed(
          specification$bootstrap$seed,
          tryCatch(
            fwb::fwb(
              bootstrap_data, fractional_statistic,
              R = specification$bootstrap$iterations,
              wtype = "exp", verbose = FALSE
            ),
            error = function(error) {
              bq_abort(
                "bq_error_analysis_runtime",
                paste0(
                  "Fractional weighted bootstrap for `t_test()` failed: ",
                  conditionMessage(error)
                ),
                analysis_id = context$analysis_id
              )
            }
          )
        )
      }
      bootstrap_values <- as.matrix(bootstrap_result$t)
      bootstrap_iterations_valid <- as.integer(sum(apply(
        bootstrap_values, 1L, function(values) all(is.finite(values))
      )))
      if (bootstrap_iterations_valid < 2L) {
        bq_abort(
          "bq_error_analysis_runtime",
          "Bootstrap produced fewer than two finite estimates.",
          analysis_id = context$analysis_id
        )
      }
      bootstrap_ci_type <- switch(
        specification$bootstrap$conf_type,
        bca = "bca", percentile = "perc", basic = "basic"
      )
      extract_interval <- function(index) {
        if (specification$bootstrap$method == "ordinary") {
          interval <- boot::boot.ci(
            bootstrap_result, conf = specification$conf_level,
            type = bootstrap_ci_type, index = index
          )
          values <- switch(
            specification$bootstrap$conf_type,
            bca = interval$bca, percentile = interval$percent,
            basic = interval$basic
          )
          if (is.null(values) || anyNA(values[1L, 4:5])) {
            stop("the requested interval could not be computed")
          }
          as.double(values[1L, 4:5])
        } else {
          interval <- fwb::fwb.ci(
            bootstrap_result, conf = specification$conf_level,
            type = bootstrap_ci_type, index = index
          )
          values <- fwb::get_ci(
            interval, type = bootstrap_ci_type
          )[[bootstrap_ci_type]]
          if (length(values) != 2L || anyNA(values)) {
            stop("the requested interval could not be computed")
          }
          unname(as.double(values))
        }
      }
      intervals <- tryCatch(
        lapply(seq_len(ncol(bootstrap_values)), extract_interval),
        error = function(error) {
          bq_abort(
            "bq_error_analysis_runtime",
            paste0("Bootstrap interval failed: ", conditionMessage(error)),
            analysis_id = context$analysis_id
          )
        }
      )
      std_error <- stats::sd(bootstrap_values[, 1L], na.rm = TRUE)
      conf_low <- intervals[[1L]][1L]
      conf_high <- intervals[[1L]][2L]
      interval_conf_level <- specification$conf_level
      test_ci_method <- paste0(
        if (specification$bootstrap$method == "fractional") {
          "fractional_bootstrap_"
        } else {
          "bootstrap_"
        },
        specification$bootstrap$conf_type
      )
      if (specification$effect_size != "none") {
        bootstrap_effect_std_error <- stats::sd(
          bootstrap_values[, 2L], na.rm = TRUE
        )
        bootstrap_effect_conf_low <- intervals[[2L]][1L]
        bootstrap_effect_conf_high <- intervals[[2L]][2L]
      }
    }

    tests <- tibble::tibble(
      test_id = context$test_id,
      analysis_id = context$analysis_id,
      outcome_var_id = context$outcome_var_id,
      test = paste0(
        if (specification$var_equal) "student" else "welch",
        if (specification$inference == "permutation") "_permutation" else "",
        "_t_test"
      ),
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
      interval_conf_level = interval_conf_level,
      ci_method = test_ci_method,
      bootstrap_method = if (is.null(specification$bootstrap)) NA_character_ else
        specification$bootstrap$method,
      bootstrap_engine = if (is.null(specification$bootstrap)) NA_character_ else
        specification$bootstrap$engine,
      bootstrap_weight_type = if (
        is.null(specification$bootstrap) ||
          specification$bootstrap$method == "ordinary"
      ) NA_character_ else specification$bootstrap$weight_type,
      bootstrap_iterations_requested = if (is.null(specification$bootstrap)) NA_integer_ else
        specification$bootstrap$iterations,
      bootstrap_iterations_valid = bootstrap_iterations_valid,
      bootstrap_seed = if (
        is.null(specification$bootstrap) || is.null(specification$bootstrap$seed)
      ) NA_integer_ else specification$bootstrap$seed,
      inference = specification$inference,
      permutation_sampling = if (specification$inference == "permutation") {
        specification$permutation$sampling
      } else NA_character_,
      permutation_p_method = if (specification$inference == "permutation") {
        specification$permutation$p_method
      } else NA_character_,
      permutation_iterations_requested = if (
        specification$inference == "permutation"
      ) specification$permutation$iterations else NA_integer_,
      permutation_iterations_performed = if (
        specification$inference == "permutation"
      ) as.integer(permutation_result$R.used) else NA_integer_,
      permutation_seed = if (
        specification$inference == "permutation" &&
          !is.null(specification$permutation$seed)
      ) specification$permutation$seed else NA_integer_
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
        ci_method = character(),
        bootstrap_method = character(), bootstrap_engine = character(),
        bootstrap_weight_type = character(),
        bootstrap_iterations_requested = integer(),
        bootstrap_iterations_valid = integer(), bootstrap_seed = integer()
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
        std_error = if (is.null(specification$bootstrap)) NA_real_ else
          bootstrap_effect_std_error,
        conf_low = if (is.null(specification$bootstrap)) {
          unname(as.double(effect_result$CI_low))
        } else bootstrap_effect_conf_low,
        conf_high = if (is.null(specification$bootstrap)) {
          unname(as.double(effect_result$CI_high))
        } else bootstrap_effect_conf_high,
        ci_method = if (is.null(specification$bootstrap)) {
          "noncentral_t"
        } else {
          paste0(
            if (specification$bootstrap$method == "fractional") {
              "fractional_bootstrap_"
            } else {
              "bootstrap_"
            },
            specification$bootstrap$conf_type
          )
        },
        bootstrap_method = if (is.null(specification$bootstrap)) NA_character_ else
          specification$bootstrap$method,
        bootstrap_engine = if (is.null(specification$bootstrap)) NA_character_ else
          specification$bootstrap$engine,
        bootstrap_weight_type = if (
          is.null(specification$bootstrap) ||
            specification$bootstrap$method == "ordinary"
        ) NA_character_ else specification$bootstrap$weight_type,
        bootstrap_iterations_requested = if (is.null(specification$bootstrap)) NA_integer_ else
          specification$bootstrap$iterations,
        bootstrap_iterations_valid = bootstrap_iterations_valid,
        bootstrap_seed = if (
          is.null(specification$bootstrap) || is.null(specification$bootstrap$seed)
        ) NA_integer_ else specification$bootstrap$seed
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
