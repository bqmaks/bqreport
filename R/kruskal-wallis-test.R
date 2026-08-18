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
  if (
    !is.character(effect_size) || length(effect_size) != 1L ||
      is.na(effect_size) ||
      !effect_size %in% c("none", "rank_epsilon_squared")
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`effect_size` must be either \"none\" or \"rank_epsilon_squared\"."
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
    !is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`conf_level` must be one finite number strictly between zero and one."
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
      "`bootstrap` must be NULL or a specification from `bootstrap_control()`."
    )
  }
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
  if (!is.null(bootstrap) && !requireNamespace("boot", quietly = TRUE)) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "Bootstrap confidence intervals require the suggested package ",
        "`boot`; install it with `install.packages(\"boot\")`."
      )
    )
  }

  specification <- list(
    kind = "kruskal_wallis_test",
    effect_size = effect_size,
    inference = inference,
    permutation = permutation,
    conf_level = as.double(conf_level),
    bootstrap = bootstrap
  )
  capabilities <- list(
    outcome_types = c("continuous", "ordinal"),
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
    supplied_results = if (effect_size == "none") "omnibus_test" else c("omnibus_test", "effect_size"),
    supplied_extractors = character(),
    suggested_dependencies = if (is.null(bootstrap)) character() else "boot"
  )

  analysis_function <- function(data, context) {
    if (
      !tibble::is_tibble(data) ||
        !identical(names(data), c(".row_id", ".outcome", ".group"))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`data` for `kruskal_wallis_test()` must be a tibble with columns ",
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
      "analysis_id", "test_id", "estimate_id", "outcome_var_id",
      "group_var_id", "strata_var_id", "group_levels"
    )
    if (!is.list(context) || !identical(names(context), required_context)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`context` for `kruskal_wallis_test()` must contain ",
          "`analysis_id`, `test_id`, `estimate_id`, `outcome_var_id`, ",
          "`group_var_id`, `strata_var_id` and `group_levels`, in that order."
        )
      )
    }
    ids <- context[c("analysis_id", "test_id", "outcome_var_id", "group_var_id")]
    valid_ids <- vapply(ids, function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    }, logical(1))
    if (!all(valid_ids)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "Analysis, test and variable IDs must be non-empty character scalars."
      )
    }
    valid_estimate_id <- is.character(context$estimate_id) &&
      length(context$estimate_id) == 1L &&
      if (effect_size == "none") is.na(context$estimate_id) else
        !is.na(context$estimate_id) && nzchar(context$estimate_id)
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
        length(context$strata_var_id) != 1L || !is.na(context$strata_var_id)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`strata_var_id` must be NA because `kruskal_wallis_test()` does not support strata."
      )
    }
    if (
      !tibble::is_tibble(context$group_levels) ||
        !identical(names(context$group_levels), c("var_id", "value", "position")) ||
        !is.character(context$group_levels$var_id) ||
        !is.character(context$group_levels$value) ||
        !is.integer(context$group_levels$position) ||
        anyNA(context$group_levels) ||
        !identical(context$group_levels$var_id, rep(context$group_var_id, nrow(context$group_levels))) ||
        !identical(context$group_levels$position, seq_len(nrow(context$group_levels))) ||
        !identical(context$group_levels$value, levels(data$.group))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`group_levels` must describe every `.group` level once, in factor order, for `group_var_id`."
      )
    }
    if (nlevels(data$.group) < 2L) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`kruskal_wallis_test()` requires at least two declared group levels."
      )
    }

    group_values <- levels(data$.group)
    missing_outcome <- is.na(data$.outcome)
    n_total <- vapply(group_values, function(x) sum(data$.group == x), integer(1))
    n_missing <- vapply(group_values, function(x) sum(data$.group == x & missing_outcome), integer(1))
    n_used <- n_total - n_missing
    if (any(n_used == 0L)) {
      group_value <- group_values[which(n_used == 0L)[1L]]
      bq_abort(
        "bq_error_invalid_analysis_input",
        sprintf(
          "Group level `%s` has no observed outcome values; provide data for every declared level.",
          group_value
        )
      )
    }

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
      test = if (specification$inference == "permutation") {
        "kruskal_wallis_permutation"
      } else "kruskal_wallis",
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
    sample_flow <- tibble::tibble(
      analysis_id = rep(context$analysis_id, length(group_values)),
      outcome_var_id = rep(context$outcome_var_id, length(group_values)),
      group_value = group_values,
      n_total = unname(n_total), n_missing = unname(n_missing),
      n_used = unname(n_used)
    )
    list(tests = tests, estimates = estimates, sample_flow = sample_flow)
  }

  structure(
    analysis_function,
    specification = specification,
    capabilities = capabilities,
    class = c("bq_kruskal_wallis_test", "bq_analysis_function", "function")
  )
}
