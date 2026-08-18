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
  check_dependency("TOSTER", "`brunner_munzel_test()`", "0.9.0")
  check_choice(
    inference, "inference", c("asymptotic", "logit", "permutation")
  )
  check_permutation_control(permutation, inference)
  check_bootstrap_control(bootstrap)
  # The relative effect lives in (0, 1), so any margin around 0.5 must keep
  # both bounds strictly inside that interval.
  resolved <- resolve_hypothesis(hypothesis, margin, benefit, max_margin = 0.5)
  margin_lower <- resolved$margin_lower
  margin_upper <- resolved$margin_upper
  benefit <- resolved$benefit
  directional <- hypothesis %in% c("noninferiority", "superiority")
  conf_level <- check_conf_level(conf_level, hypothesis)

  specification <- list(
    kind = "brunner_munzel_test", hypothesis = hypothesis,
    margin_lower = margin_lower, margin_upper = margin_upper,
    benefit = benefit,
    inference = inference, permutation = permutation, bootstrap = bootstrap,
    conf_level = conf_level
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
    group_min_levels = 2L,
    group_max_levels = 2L,
    supplied_results = "test",
    suggested_dependencies = c(
      "TOSTER (>= 0.9.0)",
      if (!is.null(bootstrap)) bootstrap$engine else character()
    )
  )

  analysis_function <- function(data, context) {
    prepared <- prepare_engine_input(
      data, context, "brunner_munzel_test",
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
    sample_flow <- prepared$sample_flow
    list(tests = tests, estimates = estimates, sample_flow = sample_flow)
  }

  structure(
    analysis_function, specification = specification,
    capabilities = capabilities,
    class = c("bq_brunner_munzel_test", "bq_analysis_function", "function")
  )
}
