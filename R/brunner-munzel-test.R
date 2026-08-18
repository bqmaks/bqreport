#' Declare a Brunner-Munzel test
#'
#' Creates a terminal analytic function for an independent two-group
#' Brunner-Munzel test. The relative effect is oriented as the probability
#' that a comparison-group value exceeds a reference-group value, with half
#' credit for ties.
#'
#' @param hypothesis One of `"two_sided"`, `"equivalence"`,
#'   `"noninferiority"` and `"superiority"`.
#' @param margin Relevant deviation from the neutral relative effect `0.5`.
#'   Must be `NULL` for a two-sided test, a positive scalar for equivalence or
#'   noninferiority, a non-negative scalar for superiority, or named
#'   `c(lower, upper)` deviations for asymmetric equivalence bounds.
#' @param benefit Beneficial outcome direction, `"higher"` or `"lower"`, for
#'   noninferiority and superiority. Must otherwise be `NULL`.
#' @param inference `"asymptotic"` for the t approximation, `"logit"` for
#'   logit-scale inference with a range-preserving interval, or `"permutation"`
#'   for a studentized randomization test.
#' @param permutation A [permutation_control()] specification when
#'   `inference = "permutation"`; otherwise `NULL`.
#' @param bootstrap `NULL` for the interval supplied by the test engine, or a
#'   [bootstrap_control()] specification for a resampling interval around the
#'   relative effect. Bootstrap does not change the test p-value.
#' @param conf_level Requested confidence level. Equivalence uses the
#'   corresponding `2 * conf_level - 1` TOST interval.
#'
#' @return A `bq_brunner_munzel_test` analytic function.
#' @export
brunner_munzel_test <- function(
  hypothesis = "two_sided",
  margin = NULL,
  benefit = NULL,
  inference = "asymptotic",
  permutation = NULL,
  bootstrap = NULL,
  conf_level = 0.95
) {
  if (
    !requireNamespace("TOSTER", quietly = TRUE) ||
      utils::packageVersion("TOSTER") < "0.9.0"
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "`brunner_munzel_test()` requires the suggested package ",
        "`TOSTER` version 0.9.0 or later."
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
  if (
    !is.character(inference) || length(inference) != 1L ||
      is.na(inference) ||
      !inference %in% c("asymptotic", "logit", "permutation")
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`inference` must be \"asymptotic\", \"logit\" or ",
        "\"permutation\"."
      )
    )
  }
  valid_permutation <- is.null(permutation) || (
    identical(class(permutation), "bq_permutation_control") &&
      identical(
        names(permutation),
        c("sampling", "iterations", "p_method", "seed")
      ) &&
      identical(permutation$sampling, "random") &&
      is.integer(permutation$iterations) &&
      length(permutation$iterations) == 1L &&
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
      paste0(
        "`permutation` must be NULL or a valid specification from ",
        "`permutation_control()`."
      )
    )
  }
  if (inference == "permutation" && is.null(permutation)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`permutation` must be supplied by `permutation_control()` when ",
        "`inference = \"permutation\"`."
      )
    )
  }
  if (inference != "permutation" && !is.null(permutation)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`permutation` must be NULL unless `inference = \"permutation\"`."
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
      "`bootstrap` must be NULL or a valid specification from `bootstrap_control()`."
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
    !is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1 ||
      hypothesis == "equivalence" && conf_level <= 0.5
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`conf_level` must be finite and strictly between zero and one, ",
        "and greater than 0.5 for equivalence."
      )
    )
  }

  margin_lower <- margin_upper <- NA_real_
  if (hypothesis == "two_sided") {
    if (!is.null(margin)) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        "`margin` must be NULL for a two-sided test."
      )
    }
  } else if (hypothesis == "equivalence") {
    scalar <- is.numeric(margin) && length(margin) == 1L &&
      !is.na(margin) && is.finite(margin) && margin > 0 && margin < 0.5
    bounds <- is.numeric(margin) && length(margin) == 2L &&
      identical(names(margin), c("lower", "upper")) && !anyNA(margin) &&
      all(is.finite(margin)) && margin[["lower"]] < 0 &&
      margin[["upper"]] > 0 && margin[["lower"]] > -0.5 &&
      margin[["upper"]] < 0.5
    if (!scalar && !bounds) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        paste0(
          "Equivalence `margin` must define deviations that place both ",
          "bounds strictly between zero and one."
        )
      )
    }
    margin_lower <- if (scalar) -as.double(margin) else margin[["lower"]]
    margin_upper <- if (scalar) as.double(margin) else margin[["upper"]]
  } else {
    positive_required <- hypothesis == "noninferiority"
    valid_margin <- is.numeric(margin) && length(margin) == 1L &&
      !is.na(margin) && is.finite(margin) && margin < 0.5 &&
      if (positive_required) margin > 0 else margin >= 0
    if (!valid_margin) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        paste0(
          "Directional `margin` must be one ",
          if (positive_required) "positive" else "non-negative",
          " finite number smaller than 0.5."
        )
      )
    }
    margin_lower <- if (positive_required) -as.double(margin) else as.double(margin)
  }

  directional <- hypothesis %in% c("noninferiority", "superiority")
  if (directional) {
    if (
      !is.character(benefit) || length(benefit) != 1L || is.na(benefit) ||
        !benefit %in% c("higher", "lower")
    ) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        "`benefit` must be \"higher\" or \"lower\" for a directional test."
      )
    }
  } else if (!is.null(benefit)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`benefit` must be NULL for two-sided and equivalence tests."
    )
  }

  specification <- list(
    kind = "brunner_munzel_test", hypothesis = hypothesis,
    margin_lower = margin_lower, margin_upper = margin_upper,
    benefit = if (is.null(benefit)) NA_character_ else benefit,
    inference = inference, permutation = permutation, bootstrap = bootstrap,
    conf_level = as.double(conf_level)
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"), outcomes_per_analysis = 1L,
    requires_group = TRUE, group_min_levels = 2L, group_max_levels = 2L,
    max_strata = 0L, supports_covariates = FALSE, supports_weights = FALSE,
    supports_clusters = FALSE, supports_matched_sets = FALSE,
    provides_fits = FALSE, supplied_results = "test",
    supplied_extractors = character(),
    suggested_dependencies = c(
      "TOSTER (>= 0.9.0)",
      if (!is.null(bootstrap)) bootstrap$engine else character()
    )
  )

  analysis_function <- function(data, context) {
    if (
      !tibble::is_tibble(data) ||
        !identical(names(data), c(".row_id", ".outcome", ".group")) ||
        anyNA(data$.row_id) || anyDuplicated(data$.row_id) ||
        !is.numeric(data$.outcome) || is.object(data$.outcome) ||
        !is.null(dim(data$.outcome)) || !is.factor(data$.group) ||
        anyNA(data$.group)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`data` for `brunner_munzel_test()` must contain valid `.row_id`, ",
          "plain numeric `.outcome` and factor `.group` columns."
        )
      )
    }
    required_context <- c(
      "analysis_id", "test_id", "outcome_var_id", "group_var_id",
      "strata_var_id", "reference_value", "group_levels"
    )
    if (!is.list(context) || !identical(names(context), required_context)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`context` for `brunner_munzel_test()` has an invalid schema."
      )
    }
    scalar_ids <- context[c(
      "analysis_id", "test_id", "outcome_var_id", "group_var_id",
      "reference_value"
    )]
    if (
      !all(vapply(scalar_ids, function(value) {
        is.character(value) && length(value) == 1L &&
          !is.na(value) && nzchar(value)
      }, logical(1))) ||
        !is.character(context$strata_var_id) ||
        length(context$strata_var_id) != 1L || !is.na(context$strata_var_id)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "IDs and the reference group in `context` are invalid."
      )
    }
    group_values <- levels(data$.group)
    if (
      length(group_values) != 2L ||
        !context$reference_value %in% group_values ||
        !tibble::is_tibble(context$group_levels) ||
        !identical(names(context$group_levels), c("var_id", "value", "position")) ||
        !identical(context$group_levels$value, group_values) ||
        !identical(context$group_levels$position, 1:2) ||
        !identical(context$group_levels$var_id, rep(context$group_var_id, 2L))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`brunner_munzel_test()` requires exactly two consistently declared groups."
      )
    }
    comparison_value <- setdiff(group_values, context$reference_value)
    missing_outcome <- is.na(data$.outcome)
    n_total <- vapply(group_values, function(value) {
      sum(data$.group == value)
    }, integer(1))
    n_missing <- vapply(group_values, function(value) {
      sum(data$.group == value & missing_outcome)
    }, integer(1))
    n_used <- n_total - n_missing
    if (any(n_used == 0L)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`brunner_munzel_test()` requires an observed outcome in both groups."
      )
    }
    comparison <- data$.outcome[
      data$.group == comparison_value & !missing_outcome
    ]
    reference <- data$.outcome[
      data$.group == context$reference_value & !missing_outcome
    ]
    raw_estimate <- weighted_relative_effect(
      comparison, reference, rep(1, length(comparison)), rep(1, length(reference))
    )
    benefit_estimate <- if (!directional) {
      NA_real_
    } else if (specification$benefit == "higher") {
      raw_estimate
    } else {
      1 - raw_estimate
    }
    if (specification$inference == "logit" && raw_estimate %in% c(0, 1)) {
      bq_abort(
        "bq_error_analysis_runtime",
        paste0(
          "Logit Brunner-Munzel inference is undefined at a relative effect ",
          "of exactly zero or one; choose `inference = \"asymptotic\"`."
        ),
        analysis_id = context$analysis_id
      )
    }

    test_x <- if (directional && specification$benefit == "lower") reference else comparison
    test_y <- if (directional && specification$benefit == "lower") comparison else reference
    alternative <- if (specification$hypothesis == "two_sided") {
      "two.sided"
    } else if (specification$hypothesis == "equivalence") {
      "equivalence"
    } else {
      "greater"
    }
    null_value <- if (specification$hypothesis == "equivalence") {
      0.5 + c(specification$margin_lower, specification$margin_upper)
    } else if (specification$hypothesis == "noninferiority") {
      0.5 + specification$margin_lower
    } else if (specification$hypothesis == "superiority") {
      0.5 + specification$margin_lower
    } else {
      0.5
    }
    if (specification$inference == "permutation") {
      possible_partitions <- choose(
        length(test_x) + length(test_y), length(test_x)
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
    }

    seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (seed_exists) {
      previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    if (
      specification$inference == "permutation" &&
        !is.null(specification$permutation$seed)
    ) {
      on.exit({
        if (seed_exists) {
          assign(".Random.seed", previous_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
      set.seed(specification$permutation$seed)
    }
    test_method <- switch(
      specification$inference,
      asymptotic = "t", logit = "logit", permutation = "perm"
    )
    test_arguments <- list(
      x = test_x, y = test_y, alternative = alternative, mu = null_value,
      alpha = 1 - specification$conf_level, scale = "probability",
      test_method = test_method
    )
    if (specification$inference == "permutation") {
      test_arguments$R <- specification$permutation$iterations
      test_arguments$p_method <- specification$permutation$p_method
    }
    result <- tryCatch(
      suppressMessages(do.call(TOSTER::brunner_munzel, test_arguments)),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`brunner_munzel_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )
    estimate <- unname(as.double(result$estimate))
    estimate_std_error <- unname(as.double(result$stderr))
    conf_low <- unname(as.double(result$conf.int[1L]))
    conf_high <- unname(as.double(result$conf.int[2L]))
    interval_conf_level <- unname(as.double(attr(result$conf.int, "conf.level")))
    ci_method <- paste0("TOSTER_", specification$inference)
    bootstrap_iterations_valid <- NA_integer_

    if (!is.null(specification$bootstrap)) {
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
      if (specification$bootstrap$method == "ordinary") {
        ordinary_statistic <- function(bootstrap_data, indices) {
          sampled <- bootstrap_data[indices, , drop = FALSE]
          x <- sampled$outcome[sampled$group == "comparison"]
          y <- sampled$outcome[sampled$group == "reference"]
          if (length(x) == 0L || length(y) == 0L) return(NA_real_)
          value <- weighted_relative_effect(x, y, rep(1, length(x)), rep(1, length(y)))
          if (directional && specification$benefit == "lower") 1 - value else value
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
              paste0("Ordinary bootstrap failed: ", conditionMessage(error)),
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
        estimate_std_error <- stats::sd(bootstrap_values[is.finite(bootstrap_values)])
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
              paste0("Ordinary bootstrap interval failed: ", conditionMessage(error)),
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
            "The requested ordinary bootstrap interval could not be computed.",
            analysis_id = context$analysis_id
          )
        }
        conf_low <- as.double(interval_values[1L, 4L])
        conf_high <- as.double(interval_values[1L, 5L])
        interval_conf_level <- specification$conf_level
        ci_method <- paste0("bootstrap_", specification$bootstrap$conf_type)
      } else {
        fractional_statistic <- function(bootstrap_data, weights) {
          comparison_rows <- bootstrap_data$group == "comparison"
          value <- weighted_relative_effect(
            bootstrap_data$outcome[comparison_rows],
            bootstrap_data$outcome[!comparison_rows],
            weights[comparison_rows], weights[!comparison_rows]
          )
          if (directional && specification$benefit == "lower") 1 - value else value
        }
        bootstrap_result <- tryCatch(
          fwb::fwb(
            bootstrap_data, fractional_statistic,
            R = specification$bootstrap$iterations,
            wtype = "exp", verbose = FALSE
          ),
          error = function(error) {
            bq_abort(
              "bq_error_analysis_runtime",
              paste0("Fractional weighted bootstrap failed: ", conditionMessage(error)),
              analysis_id = context$analysis_id
            )
          }
        )
        bootstrap_values <- as.double(bootstrap_result$t[, 1L])
        bootstrap_iterations_valid <- as.integer(sum(is.finite(bootstrap_values)))
        if (bootstrap_iterations_valid < 2L) {
          bq_abort(
            "bq_error_analysis_runtime",
            "Fractional weighted bootstrap produced fewer than two finite estimates.",
            analysis_id = context$analysis_id
          )
        }
        estimate_std_error <- stats::sd(bootstrap_values[is.finite(bootstrap_values)])
        fwb_ci_type <- switch(
          specification$bootstrap$conf_type,
          bca = "bca", percentile = "perc", basic = "basic"
        )
        interval <- tryCatch(
          fwb::fwb.ci(
            bootstrap_result, conf = specification$conf_level,
            type = fwb_ci_type
          ),
          error = function(error) {
            bq_abort(
              "bq_error_analysis_runtime",
              paste0("Fractional weighted bootstrap interval failed: ", conditionMessage(error)),
              analysis_id = context$analysis_id
            )
          }
        )
        interval_values <- fwb::get_ci(interval, type = fwb_ci_type)[[fwb_ci_type]]
        if (length(interval_values) != 2L || anyNA(interval_values)) {
          bq_abort(
            "bq_error_analysis_runtime",
            "The requested fractional weighted bootstrap interval could not be computed.",
            analysis_id = context$analysis_id
          )
        }
        conf_low <- unname(as.double(interval_values[[1L]]))
        conf_high <- unname(as.double(interval_values[[2L]]))
        interval_conf_level <- specification$conf_level
        ci_method <- paste0(
          "fractional_bootstrap_", specification$bootstrap$conf_type
        )
      }
    }
    tests <- tibble::tibble(
      test_id = context$test_id, analysis_id = context$analysis_id,
      outcome_var_id = context$outcome_var_id, test = "brunner_munzel",
      reference_value = context$reference_value,
      comparison_value = comparison_value, hypothesis = specification$hypothesis,
      benefit = specification$benefit, margin_lower = specification$margin_lower,
      margin_upper = specification$margin_upper, raw_estimate = raw_estimate,
      benefit_estimate = benefit_estimate, estimate = estimate,
      std_error = estimate_std_error,
      statistic = unname(as.double(result$statistic)),
      df = if (specification$inference == "permutation") NA_real_ else
        unname(as.double(result$parameter)),
      p_value = unname(as.double(result$p.value)),
      conf_low = conf_low, conf_high = conf_high,
      requested_conf_level = specification$conf_level,
      interval_conf_level = interval_conf_level,
      inference = specification$inference,
      ci_method = ci_method,
      ci_clamped = is.null(specification$bootstrap) && any(result$conf.int %in% c(0, 1)),
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
      permutation_sampling = if (specification$inference == "permutation") {
        specification$permutation$sampling
      } else {
        NA_character_
      },
      permutation_p_method = if (specification$inference == "permutation") {
        specification$permutation$p_method
      } else {
        NA_character_
      },
      permutation_iterations_requested = if (
        specification$inference == "permutation"
      ) specification$permutation$iterations else NA_integer_,
      permutation_iterations_performed = if (
        specification$inference == "permutation"
      ) as.integer(result$parameter) else NA_integer_,
      permutation_seed = if (
        specification$inference == "permutation" &&
          !is.null(specification$permutation$seed)
      ) specification$permutation$seed else NA_integer_
    )
    estimates <- tibble::tibble(
      estimate_id = character(), analysis_id = character(),
      outcome_var_id = character(), estimand = character(),
      estimate = double(), std_error = double(), conf_low = double(),
      conf_high = double()
    )
    sample_flow <- tibble::tibble(
      analysis_id = rep(context$analysis_id, 2L),
      outcome_var_id = rep(context$outcome_var_id, 2L),
      group_value = group_values, n_total = unname(n_total),
      n_missing = unname(n_missing), n_used = unname(n_used)
    )
    list(tests = tests, estimates = estimates, sample_flow = sample_flow)
  }

  structure(
    analysis_function, specification = specification,
    capabilities = capabilities,
    class = c("bq_brunner_munzel_test", "bq_analysis_function", "function")
  )
}
