#' Declare a one-way ANOVA
#'
#' Creates an analytic function with its method, effect size and confidence
#' level fixed before analysis. The returned function is executed later by an
#' analysis plan with data and a compiled analysis context.
#'
#' The analytic function supplies only the omnibus test and, when requested,
#' an effect size. Its internal model is not returned and cannot be used to
#' extract within-group estimates or contrasts.
#'
#' @param var_equal Whether to use the classical equal-variance ANOVA. When
#'   `FALSE`, Welch's heteroscedastic ANOVA is used.
#' @param effect_size Effect size to report. One of `"none"`,
#'   `"eta_squared"` and `"omega_squared"`.
#' @param inference `"analytical"` for the parametric F test or
#'   `"permutation"` for a randomization test of the selected F statistic.
#' @param permutation A [permutation_control()] specification when
#'   `inference = "permutation"`; otherwise `NULL`.
#' @param conf_level Confidence level for interval estimates. Must be one
#'   finite number strictly between zero and one.
#' @param bootstrap `NULL` for a point estimate only, or a specification from
#'   [bootstrap_control()] for an ordinary or fractional weighted bootstrap
#'   confidence interval. Requires an effect size.
#'
#' @return A `bq_oneway_anova` analytic function.
#' @export
#' @examples
#' analysis <- oneway_anova(
#'   var_equal = TRUE,
#'   effect_size = "omega_squared",
#'   conf_level = 0.95
#' )
#' analysis
oneway_anova <- function(
  var_equal = TRUE,
  effect_size = "omega_squared",
  inference = "analytical",
  permutation = NULL,
  conf_level = 0.95,
  bootstrap = NULL
) {
  if (
    !is.logical(var_equal) || length(var_equal) != 1L || is.na(var_equal)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`var_equal` must be either TRUE or FALSE."
    )
  }

  allowed_effect_sizes <- c("none", "eta_squared", "omega_squared")
  if (
    !is.character(effect_size) || length(effect_size) != 1L ||
      is.na(effect_size) || !effect_size %in% allowed_effect_sizes
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`effect_size` must be one of \"none\", \"eta_squared\" and ",
        "\"omega_squared\"."
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
  if (!is.null(bootstrap) && effect_size == "none") {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`bootstrap` requires `effect_size` to request an ANOVA effect size."
    )
  }
  if (
    effect_size != "none" &&
      !requireNamespace("effectsize", quietly = TRUE)
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "ANOVA effect sizes require the suggested package `effectsize`; ",
        "install it with `install.packages(\"effectsize\")`."
      )
    )
  }
  if (
    !is.null(bootstrap) && bootstrap$method == "ordinary" &&
      !requireNamespace("boot", quietly = TRUE)
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "Ordinary bootstrap requires the suggested package `boot`; ",
        "install it with `install.packages(\"boot\")`."
      )
    )
  }
  if (
    !is.null(bootstrap) && bootstrap$method == "fractional" &&
      !requireNamespace("fwb", quietly = TRUE)
  ) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "Fractional weighted bootstrap requires the suggested package `fwb`; ",
        "install it with `install.packages(\"fwb\")`."
      )
    )
  }

  specification <- list(
    kind = "oneway_anova",
    var_equal = var_equal,
    effect_size = effect_size,
    inference = inference,
    permutation = permutation,
    conf_level = as.double(conf_level),
    bootstrap = bootstrap
  )
  supplied_results <- "omnibus_test"
  suggested_dependencies <- character()
  if (effect_size != "none") {
    supplied_results <- c(supplied_results, "effect_size")
    suggested_dependencies <- "effectsize"
  }
  if (!is.null(bootstrap)) {
    suggested_dependencies <- unique(c(
      suggested_dependencies,
      if (bootstrap$method == "ordinary") "boot" else "fwb"
    ))
  }
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
          "`data` for `oneway_anova()` must be a tibble with columns ",
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
      "analysis_id", "test_id", "estimate_id", "outcome_var_id", "group_var_id",
      "strata_var_id", "group_levels"
    )
    if (!is.list(context) || !identical(names(context), required_context)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`context` for `oneway_anova()` must contain `analysis_id`, ",
          "`test_id`, `estimate_id`, `outcome_var_id`, `group_var_id`, ",
          "`strata_var_id` and `group_levels`, in that order."
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
        "`strata_var_id` must be NA because `oneway_anova()` does not support strata."
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

    if (nlevels(data$.group) < 2L) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`oneway_anova()` requires at least two declared group levels."
      )
    }

    group_values <- levels(data$.group)
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
      group_value <- group_values[which(n_used == 0L)[1L]]
      bq_abort(
        "bq_error_invalid_analysis_input",
        sprintf(
          "Group level `%s` has no observed outcome values; provide data for every declared level.",
          group_value
        )
      )
    }

    if (
      (specification$var_equal && sum(n_used) <= length(group_values)) ||
        (!specification$var_equal && any(n_used < 2L))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`oneway_anova()` has too few observed outcome values for the ",
          if (specification$var_equal) "classical" else "Welch",
          " test."
        )
      )
    }

    model_data <- data.frame(
      .outcome = data$.outcome[!missing_outcome],
      .group = data$.group[!missing_outcome]
    )
    if (length(unique(model_data$.outcome)) < 2L) {
      bq_abort(
        "bq_error_analysis_runtime",
        paste0(
          "`oneway_anova()` could not compute a finite F test; each group ",
          "must provide enough outcome variation for the declared variance ",
          "assumption."
        ),
        analysis_id = context$analysis_id
      )
    }
    test_result <- suppressWarnings(tryCatch(
      if (specification$var_equal) {
        fit <- stats::lm(.outcome ~ .group, data = model_data)
        table <- stats::anova(fit)
        list(
          statistic = table[["F value"]][1L],
          parameter = c(table[["Df"]][1L], table[["Df"]][2L]),
          p.value = table[["Pr(>F)"]][1L],
          effect_model = fit
        )
      } else {
        result <- stats::oneway.test(
          .outcome ~ .group, data = model_data, var.equal = FALSE
        )
        result$effect_model <- result
        result
      },
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`oneway_anova()` failed to compute its F test: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    ))
    test_values <- c(
      statistic = unname(as.double(test_result$statistic)),
      df1 = unname(as.double(test_result$parameter[[1L]])),
      df2 = unname(as.double(test_result$parameter[[2L]])),
      p_value = unname(as.double(test_result$p.value))
    )
    if (
      length(test_values) != 4L || any(!is.finite(test_values)) ||
        test_values[["df1"]] <= 0 || test_values[["df2"]] <= 0 ||
        test_values[["p_value"]] < 0 || test_values[["p_value"]] > 1
    ) {
      bq_abort(
        "bq_error_analysis_runtime",
        paste0(
          "`oneway_anova()` could not compute a finite F test; each group ",
          "must provide enough outcome variation for the declared variance ",
          "assumption."
        ),
        analysis_id = context$analysis_id
      )
    }

    restore_rng <- (
      specification$inference == "permutation" &&
        !is.null(specification$permutation$seed)
    ) || (
      !is.null(specification$bootstrap) &&
        !is.null(specification$bootstrap$seed)
    )
    if (restore_rng) {
      analysis_seed_exists <- exists(
        ".Random.seed", envir = .GlobalEnv, inherits = FALSE
      )
      if (analysis_seed_exists) {
        analysis_previous_seed <- get(
          ".Random.seed", envir = .GlobalEnv, inherits = FALSE
        )
      }
      on.exit({
        if (analysis_seed_exists) {
          assign(".Random.seed", analysis_previous_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
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
      if (!is.null(specification$permutation$seed)) {
        set.seed(specification$permutation$seed)
      }
      observed_statistic <- unname(as.double(test_result$statistic))
      exceedances <- 0L
      for (iteration in seq_len(specification$permutation$iterations)) {
        permuted_data <- model_data
        permuted_data$.group <- sample(model_data$.group, replace = FALSE)
        permuted_statistic <- if (specification$var_equal) {
          permuted_fit <- stats::lm(.outcome ~ .group, data = permuted_data)
          unname(as.double(stats::anova(permuted_fit)[["F value"]][1L]))
        } else {
          unname(as.double(stats::oneway.test(
            .outcome ~ .group, data = permuted_data, var.equal = FALSE
          )$statistic))
        }
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
      test = paste0(
        if (specification$var_equal) "oneway_anova" else "welch_anova",
        if (specification$inference == "permutation") "_permutation" else ""
      ),
      statistic = unname(as.double(test_result$statistic)),
      df1 = unname(as.double(test_result$parameter[[1L]])),
      df2 = unname(as.double(test_result$parameter[[2L]])),
      p_value = if (specification$inference == "permutation") {
        permutation_p_value
      } else unname(as.double(test_result$p.value)),
      variance_assumption = if (specification$var_equal) "equal" else "unequal",
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
    if (specification$effect_size == "none") {
      estimates <- tibble::tibble(
        estimate_id = character(), analysis_id = character(),
        outcome_var_id = character(), estimand = character(),
        estimate = double(), std_error = double(), conf_low = double(),
        conf_high = double(), conf_level = double(), estimator = character(),
        ci_method = character(), bootstrap_method = character(),
        bootstrap_engine = character(), bootstrap_weight_type = character(),
        bootstrap_iterations_requested = integer(),
        bootstrap_iterations_valid = integer(), bootstrap_seed = integer()
      )
    } else {
      effect_result <- tryCatch(
        if (specification$effect_size == "eta_squared") {
          effectsize::eta_squared(
            test_result$effect_model, ci = NULL, verbose = FALSE
          )
        } else {
          effectsize::omega_squared(
            test_result$effect_model, ci = NULL, verbose = FALSE
          )
        },
        error = function(error) {
          bq_abort(
            "bq_error_analysis_runtime",
            paste0(
              "Effect size for `oneway_anova()` failed: ",
              conditionMessage(error)
            ),
            analysis_id = context$analysis_id
          )
        }
      )
      estimate <- unname(as.double(
        effect_result[[if (specification$effect_size == "eta_squared") {
          "Eta2"
        } else {
          "Omega2"
        }]][1L]
      ))
      std_error <- conf_low <- conf_high <- NA_real_
      interval_conf_level <- NA_real_
      ci_method <- "not_computed"
      bootstrap_iterations_valid <- NA_integer_

      effect_from_f <- function(f_statistic, df1, df2) {
        if (!all(is.finite(c(f_statistic, df1, df2))) || df1 <= 0 || df2 <= 0) {
          return(NA_real_)
        }
        value <- if (specification$effect_size == "eta_squared") {
          f_statistic * df1 / (f_statistic * df1 + df2)
        } else {
          (f_statistic - 1) * df1 / (f_statistic * df1 + df2 + 1)
        }
        if (specification$effect_size == "omega_squared") max(0, value) else value
      }
      weighted_effect <- function(bootstrap_data, weights) {
        groups <- levels(bootstrap_data$group)
        group_rows <- lapply(groups, function(value) bootstrap_data$group == value)
        group_weight <- vapply(
          group_rows, function(rows) sum(weights[rows]), double(1)
        )
        group_weight_sq <- vapply(
          group_rows, function(rows) sum(weights[rows]^2), double(1)
        )
        if (any(group_weight <= 0) || any(group_weight_sq <= 0)) return(NA_real_)
        group_mean <- vapply(
          group_rows,
          function(rows) stats::weighted.mean(
            bootstrap_data$outcome[rows], weights[rows]
          ),
          double(1)
        )
        k <- length(groups)
        df1 <- k - 1
        if (specification$var_equal) {
          grand_mean <- stats::weighted.mean(
            bootstrap_data$outcome, weights
          )
          ss_between <- sum(group_weight * (group_mean - grand_mean)^2)
          ss_within <- sum(vapply(
            seq_along(groups),
            function(index) {
              rows <- group_rows[[index]]
              sum(weights[rows] *
                (bootstrap_data$outcome[rows] - group_mean[index])^2)
            },
            double(1)
          ))
          df2 <- sum(weights) - k
          f_statistic <- (ss_between / df1) / (ss_within / df2)
        } else {
          effective_n <- group_weight^2 / group_weight_sq
          if (any(effective_n <= 1)) return(NA_real_)
          group_variance <- vapply(
            seq_along(groups),
            function(index) {
              rows <- group_rows[[index]]
              numerator <- sum(weights[rows] *
                (bootstrap_data$outcome[rows] - group_mean[index])^2)
              denominator <- group_weight[index] -
                group_weight_sq[index] / group_weight[index]
              numerator / denominator
            },
            double(1)
          )
          if (any(!is.finite(group_variance)) || any(group_variance <= 0)) {
            return(NA_real_)
          }
          precision <- effective_n / group_variance
          precision_sum <- sum(precision)
          weighted_mean <- sum(precision * group_mean) / precision_sum
          correction_sum <- sum(
            (1 - precision / precision_sum)^2 / (effective_n - 1)
          )
          correction <- 1 + 2 * (k - 2) / (k^2 - 1) * correction_sum
          f_statistic <- sum(
            precision * (group_mean - weighted_mean)^2
          ) / df1 / correction
          df2 <- (k^2 - 1) / (3 * correction_sum)
        }
        effect_from_f(f_statistic, df1, df2)
      }

      if (!is.null(specification$bootstrap)) {
        if (!is.null(specification$bootstrap$seed)) {
          set.seed(specification$bootstrap$seed)
        }
        bootstrap_data <- data.frame(
          outcome = model_data$.outcome,
          group = model_data$.group
        )
        if (specification$bootstrap$method == "ordinary") {
          ordinary_statistic <- function(bootstrap_data, indices) {
            sampled <- bootstrap_data[indices, , drop = FALSE]
            weighted_effect(sampled, rep(1, nrow(sampled)))
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
                paste0("Ordinary ANOVA bootstrap failed: ", conditionMessage(error)),
                analysis_id = context$analysis_id
              )
            }
          )
        } else {
          fractional_statistic <- function(bootstrap_data, weights) {
            weighted_effect(bootstrap_data, weights)
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
                paste0(
                  "Fractional weighted ANOVA bootstrap failed: ",
                  conditionMessage(error)
                ),
                analysis_id = context$analysis_id
              )
            }
          )
        }
        bootstrap_values <- as.double(bootstrap_result$t[, 1L])
        bootstrap_iterations_valid <- as.integer(sum(is.finite(bootstrap_values)))
        if (bootstrap_iterations_valid < 2L) {
          bq_abort(
            "bq_error_analysis_runtime",
            "ANOVA bootstrap produced fewer than two finite effect-size estimates.",
            analysis_id = context$analysis_id
          )
        }
        std_error <- stats::sd(bootstrap_values[is.finite(bootstrap_values)])
        bootstrap_ci_type <- switch(
          specification$bootstrap$conf_type,
          bca = "bca", percentile = "perc", basic = "basic"
        )
        interval <- tryCatch(
          if (specification$bootstrap$method == "ordinary") {
            boot::boot.ci(
              bootstrap_result, conf = specification$conf_level,
              type = bootstrap_ci_type
            )
          } else {
            fwb::fwb.ci(
              bootstrap_result, conf = specification$conf_level,
              type = bootstrap_ci_type
            )
          },
          error = function(error) {
            bq_abort(
              "bq_error_analysis_runtime",
              paste0("ANOVA bootstrap interval failed: ", conditionMessage(error)),
              analysis_id = context$analysis_id
            )
          }
        )
        interval_values <- if (specification$bootstrap$method == "ordinary") {
          values <- switch(
            specification$bootstrap$conf_type,
            bca = interval$bca, percentile = interval$percent,
            basic = interval$basic
          )
          if (is.null(values) || anyNA(values[1L, 4:5])) NULL else
            as.double(values[1L, 4:5])
        } else {
          values <- fwb::get_ci(interval, type = bootstrap_ci_type)[[bootstrap_ci_type]]
          if (length(values) != 2L || anyNA(values)) NULL else
            unname(as.double(values))
        }
        if (is.null(interval_values)) {
          bq_abort(
            "bq_error_analysis_runtime",
            "The requested ANOVA bootstrap interval could not be computed.",
            analysis_id = context$analysis_id
          )
        }
        conf_low <- interval_values[1L]
        conf_high <- interval_values[2L]
        interval_conf_level <- specification$conf_level
        ci_method <- paste0(
          if (specification$bootstrap$method == "fractional") {
            "fractional_bootstrap_"
          } else "bootstrap_",
          specification$bootstrap$conf_type
        )
      }

      estimates <- tibble::tibble(
        estimate_id = context$estimate_id,
        analysis_id = context$analysis_id,
        outcome_var_id = context$outcome_var_id,
        estimand = specification$effect_size,
        estimate = estimate,
        std_error = std_error, conf_low = conf_low, conf_high = conf_high,
        conf_level = interval_conf_level,
        estimator = if (specification$var_equal) {
          paste0("effectsize::", specification$effect_size, "_classical_f")
        } else {
          paste0("effectsize::", specification$effect_size, "_welch_f_approximation")
        },
        ci_method = ci_method,
        bootstrap_method = if (is.null(specification$bootstrap)) {
          NA_character_
        } else specification$bootstrap$method,
        bootstrap_engine = if (is.null(specification$bootstrap)) {
          NA_character_
        } else specification$bootstrap$engine,
        bootstrap_weight_type = if (
          is.null(specification$bootstrap) ||
            specification$bootstrap$method == "ordinary"
        ) NA_character_ else specification$bootstrap$weight_type,
        bootstrap_iterations_requested = if (is.null(specification$bootstrap)) {
          NA_integer_
        } else specification$bootstrap$iterations,
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
    class = c("bq_oneway_anova", "bq_analysis_function", "function")
  )
}
