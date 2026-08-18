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
      identical(exact, "auto")
  if (!valid_exact) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`exact` must be \"auto\", TRUE or FALSE."
    )
  }
  check_flag(continuity_correction, "continuity_correction")
  check_bootstrap_control(bootstrap)
  if (!is.null(bootstrap) && bootstrap$method != "ordinary") {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`bootstrap` must be an ordinary specification; fractional ",
        "bootstrap is not supported for the Hodges-Lehmann estimator."
      )
    )
  }
  check_choice(inference, "inference", c("analytical", "permutation"))
  check_permutation_control(permutation, inference)
  resolved <- resolve_hypothesis(hypothesis, margin, benefit)
  margin_lower <- resolved$margin_lower
  margin_upper <- resolved$margin_upper
  benefit <- resolved$benefit
  if (inference == "permutation" && hypothesis == "equivalence") {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "Permutation Hodges-Lehmann inference is not valid for equivalence ",
        "bounds; use `inference = \"analytical\"`."
      )
    )
  }
  if (inference == "permutation") {
    check_dependency(
      "TOSTER", "Permutation Mann-Whitney inference", "0.9.0"
    )
  }
  conf_level <- check_conf_level(conf_level, hypothesis)

  specification <- list(
    kind = "mann_whitney_test",
    exact = exact,
    continuity_correction = continuity_correction,
    hypothesis = hypothesis,
    margin_lower = margin_lower,
    margin_upper = margin_upper,
    benefit = benefit,
    inference = inference,
    permutation = permutation,
    bootstrap = bootstrap,
    conf_level = conf_level
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
    group_min_levels = 2L,
    group_max_levels = 2L,
    supplied_results = "test",
    suggested_dependencies = c(
      if (inference == "permutation") "TOSTER (>= 0.9.0)" else character(),
      if (!is.null(bootstrap)) "boot" else character()
    )
  )

  analysis_function <- function(data, context) {
    prepared <- prepare_engine_input(
      data, context, "mann_whitney_test",
      group_levels = c(2L, 2L), reference = TRUE
    )
    group_values <- prepared$group_values
    comparison_value <- prepared$comparison_value
    missing_outcome <- prepared$missing_outcome
    n_used <- prepared$n_used

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
      alternative <- if (specification$hypothesis == "two_sided") {
        "two.sided"
      } else {
        "greater"
      }
      null_value <- if (
        specification$hypothesis %in% c("noninferiority", "superiority")
      ) specification$margin_lower else 0
      permutation_result <- with_resampling_seed(
        specification$permutation$seed,
        tryCatch(
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
              paste0(
                "Permutation `mann_whitney_test()` failed: ",
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
                "Ordinary Hodges-Lehmann bootstrap failed: ",
                conditionMessage(error)
              ),
              analysis_id = context$analysis_id
            )
          }
        )
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
        "hodges_lehmann"
      } else "mann_whitney",
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
    sample_flow <- prepared$sample_flow

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
