#' Declare a Kruskal-Wallis test
#'
#' Creates a terminal analytic function that supplies an omnibus
#' Kruskal-Wallis test and, optionally, rank epsilon squared. It does not
#' return a fitted model or support extractors.
#'
#' @param effect_size Effect size to report: `"none"` or
#'   `"rank_epsilon_squared"`.
#' @param inference `"analytical"` for the chi-squared approximation or
#'   `"permutation"` for a randomization test of the Kruskal-Wallis statistic.
#' @param permutation A [permutation_control()] specification when
#'   `inference = "permutation"`; otherwise `NULL`.
#' @param conf_level Confidence level for a bootstrap interval. It is recorded
#'   but not used when `bootstrap` is `NULL`.
#' @param bootstrap `NULL` for a point estimate only, or a specification from
#'   [bootstrap_control()] for a nonparametric confidence interval. Bootstrap
#'   requires `effect_size = "rank_epsilon_squared"`.
#'
#' @return A `bq_kruskal_wallis_test` analytic function.
#' @export
kruskal_wallis_test <- function(
  effect_size = "none",
  inference = "analytical",
  permutation = NULL,
  conf_level = 0.95,
  bootstrap = NULL
) {
  check_choice(effect_size, "effect_size", c("none", "rank_epsilon_squared"))
  check_choice(inference, "inference", c("analytical", "permutation"))
  check_permutation_control(permutation, inference)
  conf_level <- check_conf_level(conf_level)
  check_bootstrap_control(bootstrap)
  if (!is.null(bootstrap) && bootstrap$method == "fractional") {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`kruskal_wallis_test()` does not support fractional weighted ",
        "bootstrap; use `bootstrap_control(method = \"ordinary\")`."
      )
    )
  }
  if (!is.null(bootstrap) && effect_size == "none") {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`bootstrap` requires `effect_size = \"rank_epsilon_squared\"`."
    )
  }

  specification <- list(
    kind = "kruskal_wallis_test",
    effect_size = effect_size,
    inference = inference,
    permutation = permutation,
    conf_level = conf_level,
    bootstrap = bootstrap
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
    group_min_levels = 2L,
    group_max_levels = NA_integer_,
    supplied_results = if (effect_size == "none") "omnibus_test" else c("omnibus_test", "effect_size"),
    suggested_dependencies = if (is.null(bootstrap)) character() else "boot"
  )

  analysis_function <- function(data, context) {
    prepared <- prepare_engine_input(
      data, context, "kruskal_wallis_test",
      estimate_id = if (effect_size == "none") "missing" else "required"
    )
    group_values <- prepared$group_values
    missing_outcome <- prepared$missing_outcome
    n_used <- prepared$n_used
    used <- !missing_outcome
    test_result <- tryCatch(
      stats::kruskal.test(data$.outcome[used], data$.group[used]),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`kruskal_wallis_test()` failed: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )
    test_values <- c(
      statistic = unname(as.double(test_result$statistic)),
      df = unname(as.double(test_result$parameter)),
      p_value = unname(as.double(test_result$p.value))
    )
    if (
      length(test_values) != 3L || any(!is.finite(test_values)) ||
        test_values[["df"]] <= 0 || test_values[["p_value"]] < 0 ||
        test_values[["p_value"]] > 1
    ) {
      bq_abort(
        "bq_error_analysis_runtime",
        paste0(
          "`kruskal_wallis_test()` could not compute a finite omnibus test; ",
          "the observed outcomes must contain rank variation."
        ),
        analysis_id = context$analysis_id
      )
    }
    permutation_p_value <- NA_real_
    permutation_iterations_performed <- NA_integer_
    if (specification$inference == "permutation") {
      log_partitions <- lgamma(sum(n_used) + 1) - sum(lgamma(n_used + 1))
      if (log(specification$permutation$iterations) >= log_partitions - 1e-12) {
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
      observed_statistic <- unname(as.double(test_result$statistic))
      used_outcome <- data$.outcome[used]
      used_group <- data$.group[used]
      exceedances <- 0L
      for (iteration in seq_len(specification$permutation$iterations)) {
        permuted_group <- sample(used_group, replace = FALSE)
        permuted_statistic <- unname(as.double(
          stats::kruskal.test(used_outcome, permuted_group)$statistic
        ))
        if (permuted_statistic >= observed_statistic) {
          exceedances <- exceedances + 1L
        }
      }
      permutation_p_value <- (exceedances + 1) /
        (specification$permutation$iterations + 1)
      permutation_iterations_performed <- specification$permutation$iterations
    }
    tests <- tibble::tibble(
      test_id = context$test_id,
      analysis_id = context$analysis_id,
      outcome_var_id = context$outcome_var_id,
      test = "kruskal_wallis",
      statistic = unname(as.double(test_result$statistic)),
      df = if (specification$inference == "permutation") NA_real_ else
        unname(as.double(test_result$parameter)),
      p_value = if (specification$inference == "permutation") {
        permutation_p_value
      } else unname(as.double(test_result$p.value)),
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
      permutation_iterations_performed = permutation_iterations_performed,
      permutation_seed = if (
        specification$inference == "permutation" &&
          !is.null(specification$permutation$seed)
      ) specification$permutation$seed else NA_integer_
    )

    if (effect_size == "none") {
      estimates <- tibble::tibble(
        estimate_id = character(), analysis_id = character(),
        outcome_var_id = character(), estimand = character(),
        estimate = double(), std_error = double(), conf_low = double(),
        conf_high = double(), conf_level = double(), estimator = character(),
        ci_method = character(), bootstrap_engine = character(),
        bootstrap_iterations_requested = integer(),
        bootstrap_iterations_valid = integer(), bootstrap_seed = integer()
      )
    } else {
      estimate <- unname(as.double(test_result$statistic)) / (sum(used) - 1)
      std_error <- conf_low <- conf_high <- NA_real_
      ci_method <- "not_computed"
      bootstrap_iterations_valid <- NA_integer_

      if (!is.null(bootstrap)) {
        bootstrap_data <- data.frame(
          outcome = data$.outcome[used],
          group = data$.group[used]
        )
        statistic <- function(bootstrap_data, indices) {
          sampled <- bootstrap_data[indices, , drop = FALSE]
          observed_groups <- unique(sampled$group[!is.na(sampled$outcome)])
          if (length(observed_groups) < 2L) {
            return(NA_real_)
          }
          result <- stats::kruskal.test(sampled$outcome, sampled$group)
          unname(as.double(result$statistic)) / (nrow(sampled) - 1)
        }

        seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        if (seed_exists) {
          previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        }
        if (!is.null(bootstrap$seed)) {
          on.exit({
            if (seed_exists) {
              assign(".Random.seed", previous_seed, envir = .GlobalEnv)
            } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
              rm(".Random.seed", envir = .GlobalEnv)
            }
          }, add = TRUE)
          set.seed(bootstrap$seed)
        }

        bootstrap_result <- tryCatch(
          boot::boot(
            data = bootstrap_data,
            statistic = statistic,
            R = bootstrap$iterations,
            sim = "ordinary",
            stype = "i",
            strata = bootstrap_data$group
          ),
          error = function(error) {
            bq_abort(
              "bq_error_analysis_runtime",
              paste0("Bootstrap for rank epsilon squared failed: ", conditionMessage(error)),
              analysis_id = context$analysis_id
            )
          }
        )
        bootstrap_values <- as.double(bootstrap_result$t[, 1L])
        bootstrap_iterations_valid <- as.integer(sum(is.finite(bootstrap_values)))
        if (bootstrap_iterations_valid < 2L) {
          bq_abort(
            "bq_error_analysis_runtime",
            "Bootstrap produced fewer than two finite rank epsilon squared estimates.",
            analysis_id = context$analysis_id
          )
        }
        std_error <- stats::sd(bootstrap_values[is.finite(bootstrap_values)])
        boot_ci_type <- switch(
          bootstrap$conf_type,
          bca = "bca",
          percentile = "perc",
          basic = "basic"
        )
        interval <- tryCatch(
          boot::boot.ci(
            bootstrap_result,
            conf = conf_level,
            type = boot_ci_type
          ),
          error = function(error) {
            bq_abort(
              "bq_error_analysis_runtime",
              paste0("Bootstrap confidence interval failed: ", conditionMessage(error)),
              analysis_id = context$analysis_id
            )
          }
        )
        interval_values <- switch(
          bootstrap$conf_type,
          bca = interval$bca,
          percentile = interval$percent,
          basic = interval$basic
        )
        if (is.null(interval_values) || anyNA(interval_values[1L, 4:5])) {
          bq_abort(
            "bq_error_analysis_runtime",
            "The requested bootstrap confidence interval could not be computed.",
            analysis_id = context$analysis_id
          )
        }
        conf_low <- as.double(interval_values[1L, 4L])
        conf_high <- as.double(interval_values[1L, 5L])
        ci_method <- paste0("bootstrap_", bootstrap$conf_type)
      }

      estimates <- tibble::tibble(
        estimate_id = context$estimate_id,
        analysis_id = context$analysis_id,
        outcome_var_id = context$outcome_var_id,
        estimand = "rank_epsilon_squared",
        estimate = estimate,
        std_error = std_error, conf_low = conf_low, conf_high = conf_high,
        conf_level = if (is.null(bootstrap)) NA_real_ else conf_level,
        estimator = "kruskal_wallis_H/(n-1)", ci_method = ci_method,
        bootstrap_engine = if (is.null(bootstrap)) NA_character_ else "boot::boot",
        bootstrap_iterations_requested = if (is.null(bootstrap)) NA_integer_ else bootstrap$iterations,
        bootstrap_iterations_valid = bootstrap_iterations_valid,
        bootstrap_seed = if (is.null(bootstrap) || is.null(bootstrap$seed)) NA_integer_ else bootstrap$seed
      )
    }
    sample_flow <- prepared$sample_flow
    list(tests = tests, estimates = estimates, sample_flow = sample_flow)
  }

  structure(
    analysis_function,
    specification = specification,
    capabilities = capabilities,
    class = c("bq_kruskal_wallis_test", "bq_analysis_function", "function")
  )
}
