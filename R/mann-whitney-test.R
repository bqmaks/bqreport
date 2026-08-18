#' Declare a Mann-Whitney test
#'
#' Creates an analytic function for an independent two-group Mann-Whitney test
#' with its exactness policy, continuity correction and confidence level fixed
#' before analysis. The returned function is executed later by an analysis
#' plan with data and a compiled analysis context.
#'
#' The analytic function supplies only the test result available from
#' [stats::wilcox.test()]. It does not return a fitted model, a separate effect
#' size or within-group estimates.
#'
#' @param exact Exactness policy. Use `"auto"` to choose explicitly from the
#'   observed sample sizes and ties before calling [stats::wilcox.test()],
#'   `TRUE` to require an exact test or `FALSE` to require an asymptotic test.
#' @param continuity_correction Whether to apply the continuity correction for
#'   an asymptotic test.
#' @param hypothesis Hypothesis type. One of `"two_sided"`, `"equivalence"`,
#'   `"noninferiority"` and `"superiority"`.
#' @param margin Hypothesis margin on the Hodges-Lehmann location-shift scale.
#'   The rules are the same as for [t_test()].
#' @param benefit Which outcome direction is beneficial for noninferiority and
#'   superiority: `"higher"` or `"lower"`.
#' @param inference `"analytical"` for [stats::wilcox.test()] or
#'   `"permutation"` for a randomization Hodges-Lehmann test.
#' @param permutation A [permutation_control()] specification when
#'   `inference = "permutation"`; otherwise `NULL`.
#' @param bootstrap `NULL` for the inference-engine interval or an ordinary
#'   [bootstrap_control()] specification for the Hodges-Lehmann interval.
#' @param conf_level Confidence level for the Hodges-Lehmann location-shift
#'   interval. Must be one finite number strictly between zero and one.
#'
#' @return A `bq_mann_whitney_test` analytic function.
#' @export
#' @examples
#' analysis <- mann_whitney_test(
#'   exact = "auto",
#'   continuity_correction = TRUE,
#'   conf_level = 0.95
#' )
#' analysis
mann_whitney_test <- function(
  exact = "auto",
  continuity_correction = TRUE,
  hypothesis = "two_sided",
  margin = NULL,
  benefit = NULL,
  inference = "analytical",
  permutation = NULL,
  bootstrap = NULL,
  conf_level = 0.95
) {
  valid_exact <-
    is.logical(exact) && length(exact) == 1L && !is.na(exact) ||
      is.character(exact) && length(exact) == 1L &&
        !is.na(exact) && identical(exact, "auto")
  if (!valid_exact) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`exact` must be \"auto\", TRUE or FALSE."
    )
  }

  if (
    !is.logical(continuity_correction) ||
      length(continuity_correction) != 1L ||
      is.na(continuity_correction)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`continuity_correction` must be either TRUE or FALSE."
    )
  }
  valid_bootstrap <- is.null(bootstrap) || (
    identical(class(bootstrap), "bq_bootstrap_control") &&
      identical(
        names(bootstrap),
        c("method", "engine", "iterations", "conf_type", "seed", "weight_type")
      ) &&
      identical(bootstrap$method, "ordinary") &&
      identical(bootstrap$engine, "boot") &&
      is.integer(bootstrap$iterations) && length(bootstrap$iterations) == 1L &&
      !is.na(bootstrap$iterations) && bootstrap$iterations > 0L &&
      is.character(bootstrap$conf_type) && length(bootstrap$conf_type) == 1L &&
      !is.na(bootstrap$conf_type) &&
      bootstrap$conf_type %in% c("bca", "percentile", "basic") &&
      (is.null(bootstrap$seed) || (
        is.integer(bootstrap$seed) && length(bootstrap$seed) == 1L &&
          !is.na(bootstrap$seed) && bootstrap$seed >= 0L
      )) && is.null(bootstrap$weight_type)
  )
  if (!valid_bootstrap) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`bootstrap` must be NULL or an ordinary specification from ",
        "`bootstrap_control()`; fractional bootstrap is not supported for ",
        "the Hodges-Lehmann estimator."
      )
    )
  }
  if (!is.null(bootstrap) && !requireNamespace("boot", quietly = TRUE)) {
    bq_abort(
      "bq_error_missing_dependency",
      "Ordinary bootstrap requires the suggested package `boot`."
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
  if (inference == "permutation" && hypothesis == "equivalence") {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "Permutation Hodges-Lehmann inference is not valid for equivalence ",
        "bounds; use `inference = \"analytical\"`."
      )
    )
  }
  if (
    inference == "permutation" &&
      (!requireNamespace("TOSTER", quietly = TRUE) ||
        utils::packageVersion("TOSTER") < "0.9.0")
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "Permutation Mann-Whitney inference requires the suggested package ",
        "`TOSTER` 0.9.0 or later."
      )
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
        paste0("`benefit` must be \"higher\" or \"lower\" for ", hypothesis, ".")
      )
    }
  } else if (!is.null(benefit)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`benefit` must be NULL for two-sided and equivalence tests."
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
    kind = "mann_whitney_test",
    exact = exact,
    continuity_correction = continuity_correction,
    hypothesis = hypothesis,
    margin_lower = margin_lower,
    margin_upper = margin_upper,
    benefit = if (is.null(benefit)) NA_character_ else benefit,
    inference = inference,
    permutation = permutation,
    bootstrap = bootstrap,
    conf_level = as.double(conf_level)
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
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
    supplied_results = "test",
    supplied_extractors = character(),
    suggested_dependencies = c(
      if (inference == "permutation") "TOSTER (>= 0.9.0)" else character(),
      if (!is.null(bootstrap)) "boot" else character()
    )
  )

  analysis_function <- function(data, context) {
    if (
      !tibble::is_tibble(data) ||
        !identical(names(data), c(".row_id", ".outcome", ".group"))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`data` for `mann_whitney_test()` must be a tibble with columns ",
          "`.row_id`, `.outcome` and `.group`, in that order."
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
        paste0(
          "`.outcome` must be one plain numeric vector; ordinal outcomes ",
          "must be compiled to their declared order scores."
        )
      )
    }

    if (!is.factor(data$.group) || anyNA(data$.group)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`.group` must be a factor without missing values."
      )
    }

    required_context <- c(
      "analysis_id", "test_id", "outcome_var_id", "group_var_id",
      "strata_var_id", "reference_value", "group_levels"
    )
    if (!is.list(context) || !identical(names(context), required_context)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`context` for `mann_whitney_test()` must contain `analysis_id`, ",
          "`test_id`, `outcome_var_id`, `group_var_id`, `strata_var_id`, ",
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

    if (
      !is.character(context$strata_var_id) ||
        length(context$strata_var_id) != 1L ||
        !is.na(context$strata_var_id)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`strata_var_id` must be NA because `mann_whitney_test()` does not ",
          "support strata."
        )
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
        "`mann_whitney_test()` requires exactly two declared group levels."
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

    if (any(n_used == 0L)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`mann_whitney_test()` requires an observed outcome in both groups."
      )
    }

    comparison <- data$.outcome[
      data$.group == comparison_value & !missing_outcome
    ]
    reference <- data$.outcome[
      data$.group == context$reference_value & !missing_outcome
    ]
    has_ties <- anyDuplicated(c(comparison, reference)) > 0L
    if (
      specification$inference == "analytical" &&
        isTRUE(specification$exact) && has_ties
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "Exact `mann_whitney_test()` is not supported when observed values ",
          "contain ties; use `exact = FALSE` or `exact = \"auto\"`."
        )
      )
    }
    exact_used <- if (specification$inference == "permutation") {
      NA
    } else if (identical(specification$exact, "auto")) {
      length(comparison) < 50L && length(reference) < 50L && !has_ties
    } else {
      specification$exact
    }

    benefit_sign <- if (
      specification$hypothesis %in% c("noninferiority", "superiority") &&
        specification$benefit == "lower"
    ) -1 else 1
    test_comparison <- benefit_sign * comparison
    test_reference <- benefit_sign * reference
    run_test <- function(x, y, alternative, mu, conf_level) {
      stats::wilcox.test(
        x = x, y = y, alternative = alternative, mu = mu,
        exact = exact_used, correct = specification$continuity_correction,
        conf.int = TRUE, conf.level = conf_level
      )
    }
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
      seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (seed_exists) {
        previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      }
      if (!is.null(specification$permutation$seed)) {
        on.exit({
          if (seed_exists) {
            assign(".Random.seed", previous_seed, envir = .GlobalEnv)
          } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
          }
        }, add = TRUE)
        set.seed(specification$permutation$seed)
      }
      alternative <- if (specification$hypothesis == "two_sided") {
        "two.sided"
      } else {
        "greater"
      }
      null_value <- if (
        specification$hypothesis %in% c("noninferiority", "superiority")
      ) specification$margin_lower else 0
      permutation_result <- tryCatch(
        suppressMessages(TOSTER::hodges_lehmann(
          test_comparison, test_reference, alternative = alternative,
          mu = null_value, alpha = 1 - specification$conf_level,
          R = specification$permutation$iterations,
          p_method = specification$permutation$p_method,
          keep_perm = FALSE
        )),
        error = function(error) {
          bq_abort(
            "bq_error_analysis_runtime",
            paste0("Permutation `mann_whitney_test()` failed: ", conditionMessage(error)),
            analysis_id = context$analysis_id
          )
        }
      )
    }
    test_results <- if (specification$inference == "permutation") NULL else tryCatch(
      if (specification$hypothesis == "two_sided") {
        list(primary = run_test(
          comparison, reference, "two.sided", 0, specification$conf_level
        ))
      } else if (specification$hypothesis %in% c("noninferiority", "superiority")) {
        list(primary = run_test(
          test_comparison, test_reference, "greater",
          specification$margin_lower, specification$conf_level
        ))
      } else {
        list(
          lower = run_test(
            comparison, reference, "greater", specification$margin_lower,
            specification$conf_level
          ),
          upper = run_test(
            comparison, reference, "less", specification$margin_upper,
            specification$conf_level
          ),
          interval = run_test(
            comparison, reference, "two.sided", 0,
            2 * specification$conf_level - 1
          )
        )
      },
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`mann_whitney_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )

    if (specification$inference == "permutation") {
      directional <- specification$hypothesis %in% c(
        "noninferiority", "superiority"
      )
      estimate <- unname(as.double(permutation_result$estimate))
      raw_estimate <- if (directional) benefit_sign * estimate else estimate
      benefit_estimate <- if (directional) estimate else NA_real_
      statistic <- unname(as.double(permutation_result$statistic))
      statistic_lower <- statistic_upper <- NA_real_
      p_value <- unname(as.double(permutation_result$p.value))
      p_lower <- if (directional) p_value else NA_real_
      p_upper <- NA_real_
      conf_low <- unname(as.double(permutation_result$conf.int[1L]))
      conf_high <- unname(as.double(permutation_result$conf.int[2L]))
      interval_conf_level <- unname(as.double(
        attr(permutation_result$conf.int, "conf.level")
      ))
    } else if (specification$hypothesis == "equivalence") {
      raw_estimate <- unname(as.double(test_results$interval$estimate))
      benefit_estimate <- NA_real_
      estimate <- raw_estimate
      statistic <- NA_real_
      statistic_lower <- unname(as.double(test_results$lower$statistic))
      statistic_upper <- unname(as.double(test_results$upper$statistic))
      p_lower <- unname(as.double(test_results$lower$p.value))
      p_upper <- unname(as.double(test_results$upper$p.value))
      p_value <- max(p_lower, p_upper)
      conf_low <- unname(as.double(test_results$interval$conf.int[1L]))
      conf_high <- unname(as.double(test_results$interval$conf.int[2L]))
      interval_conf_level <- unname(as.double(
        attr(test_results$interval$conf.int, "conf.level")
      ))
    } else {
      primary <- test_results$primary
      directional <- specification$hypothesis %in% c(
        "noninferiority", "superiority"
      )
      estimate <- unname(as.double(primary$estimate))
      raw_estimate <- if (directional) benefit_sign * estimate else estimate
      benefit_estimate <- if (directional) estimate else NA_real_
      statistic <- unname(as.double(primary$statistic))
      statistic_lower <- NA_real_
      statistic_upper <- NA_real_
      p_value <- unname(as.double(primary$p.value))
      p_lower <- if (directional) p_value else NA_real_
      p_upper <- NA_real_
      conf_low <- unname(as.double(primary$conf.int[1L]))
      conf_high <- unname(as.double(primary$conf.int[2L]))
      interval_conf_level <- unname(as.double(attr(primary$conf.int, "conf.level")))
    }

    std_error <- NA_real_
    ci_method <- if (specification$inference == "permutation") {
      "TOSTER_permutation"
    } else {
      if (exact_used) "wilcox_exact" else "wilcox_asymptotic"
    }
    bootstrap_iterations_valid <- NA_integer_
    if (!is.null(specification$bootstrap)) {
      seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (seed_exists) {
        previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      }
      if (!is.null(specification$bootstrap$seed)) {
        if (
          specification$inference != "permutation" ||
            is.null(specification$permutation$seed)
        ) {
          on.exit({
            if (seed_exists) {
              assign(".Random.seed", previous_seed, envir = .GlobalEnv)
            } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
              rm(".Random.seed", envir = .GlobalEnv)
            }
          }, add = TRUE)
        }
        set.seed(specification$bootstrap$seed)
      }
      bootstrap_data <- data.frame(
        outcome = c(comparison, reference),
        group = factor(
          c(rep("comparison", length(comparison)), rep("reference", length(reference))),
          levels = c("comparison", "reference")
        )
      )
      ordinary_statistic <- function(bootstrap_data, indices) {
        sampled <- bootstrap_data[indices, , drop = FALSE]
        x <- sampled$outcome[sampled$group == "comparison"]
        y <- sampled$outcome[sampled$group == "reference"]
        shift <- stats::median(as.vector(outer(x, y, "-")))
        if (benefit_sign < 0) -shift else shift
      }
      bootstrap_result <- tryCatch(
        boot::boot(
          bootstrap_data, ordinary_statistic,
          R = specification$bootstrap$iterations,
          sim = "ordinary", stype = "i", strata = bootstrap_data$group
        ),
        error = function(error) {
          bq_abort(
            "bq_error_analysis_runtime",
            paste0("Ordinary Hodges-Lehmann bootstrap failed: ", conditionMessage(error)),
            analysis_id = context$analysis_id
          )
        }
      )
      bootstrap_values <- as.double(bootstrap_result$t[, 1L])
      bootstrap_iterations_valid <- as.integer(sum(is.finite(bootstrap_values)))
      if (bootstrap_iterations_valid < 2L) {
        bq_abort(
          "bq_error_analysis_runtime",
          "Ordinary bootstrap produced fewer than two finite estimates.",
          analysis_id = context$analysis_id
        )
      }
      std_error <- stats::sd(bootstrap_values[is.finite(bootstrap_values)])
      boot_ci_type <- switch(
        specification$bootstrap$conf_type,
        bca = "bca", percentile = "perc", basic = "basic"
      )
      interval <- tryCatch(
        boot::boot.ci(
          bootstrap_result, conf = specification$conf_level,
          type = boot_ci_type
        ),
        error = function(error) {
          bq_abort(
            "bq_error_analysis_runtime",
            paste0("Ordinary Hodges-Lehmann bootstrap interval failed: ", conditionMessage(error)),
            analysis_id = context$analysis_id
          )
        }
      )
      interval_values <- switch(
        specification$bootstrap$conf_type,
        bca = interval$bca, percentile = interval$percent,
        basic = interval$basic
      )
      if (is.null(interval_values) || anyNA(interval_values[1L, 4:5])) {
        bq_abort(
          "bq_error_analysis_runtime",
          "The requested Hodges-Lehmann bootstrap interval could not be computed.",
          analysis_id = context$analysis_id
        )
      }
      conf_low <- as.double(interval_values[1L, 4L])
      conf_high <- as.double(interval_values[1L, 5L])
      interval_conf_level <- specification$conf_level
      ci_method <- paste0("bootstrap_", specification$bootstrap$conf_type)
    }

    exact_requested <- if (specification$inference == "permutation") {
      NA_character_
    } else if (identical(specification$exact, "auto")) {
      "auto"
    } else if (specification$exact) {
      "exact"
    } else {
      "asymptotic"
    }
    tests <- tibble::tibble(
      test_id = context$test_id,
      analysis_id = context$analysis_id,
      outcome_var_id = context$outcome_var_id,
      test = if (specification$inference == "permutation") {
        "hodges_lehmann_permutation_test"
      } else "mann_whitney_test",
      reference_value = context$reference_value,
      comparison_value = comparison_value,
      hypothesis = specification$hypothesis,
      benefit = specification$benefit,
      margin_lower = specification$margin_lower,
      margin_upper = specification$margin_upper,
      raw_estimate = raw_estimate,
      benefit_estimate = benefit_estimate,
      estimate = estimate,
      std_error = std_error,
      statistic = statistic,
      statistic_lower = statistic_lower,
      statistic_upper = statistic_upper,
      p_value = p_value,
      p_lower = p_lower,
      p_upper = p_upper,
      conf_low = conf_low,
      conf_high = conf_high,
      requested_conf_level = specification$conf_level,
      interval_conf_level = interval_conf_level,
      ci_method = ci_method,
      bootstrap_method = if (is.null(specification$bootstrap)) NA_character_ else
        specification$bootstrap$method,
      bootstrap_engine = if (is.null(specification$bootstrap)) NA_character_ else
        specification$bootstrap$engine,
      bootstrap_iterations_requested = if (is.null(specification$bootstrap)) NA_integer_ else
        specification$bootstrap$iterations,
      bootstrap_iterations_valid = bootstrap_iterations_valid,
      bootstrap_seed = if (
        is.null(specification$bootstrap) || is.null(specification$bootstrap$seed)
      ) NA_integer_ else specification$bootstrap$seed,
      inference = specification$inference,
      exact_requested = exact_requested,
      exact_used = exact_used,
      has_ties = has_ties,
      continuity_correction = if (specification$inference == "permutation") {
        NA
      } else !exact_used && specification$continuity_correction,
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
    estimates <- tibble::tibble(
      estimate_id = character(),
      analysis_id = character(),
      outcome_var_id = character(),
      estimand = character(),
      estimate = double(),
      std_error = double(),
      conf_low = double(),
      conf_high = double()
    )
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
    class = c(
      "bq_mann_whitney_test",
      "bq_analysis_function",
      "function"
    )
  )
}
