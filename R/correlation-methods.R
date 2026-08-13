# Correlation method constructors, output contracts, and resampling.

#' Construct correlation methods
#'
#' Pearson intervals use the Fisher z transformation; Spearman intervals use
#' the Bonett-Wright standard error on the Fisher z scale. Kendall intervals
#' rely on a coarse normal approximation on the raw tau scale, clipped to
#' `[-1, 1]`; when the Kendall interval matters, prefer wrapping the method
#' in [resampled_correlation()] for bootstrap inference.
#'
#' @return A concrete `correlation_method_spec`.
#' @export
pearson_correlation <- function() {
  new_correlation_method("pearson", "fisher_z", compute = function(context) {
    compute_builtin_correlation(context, "pearson")
  })
}

#' @rdname pearson_correlation
#' @export
spearman_correlation <- function() {
  new_correlation_method(
    "spearman", "fisher_z_bonett_wright", supports_interaction = FALSE,
    compute = function(context) compute_builtin_correlation(context, "spearman")
  )
}

#' @rdname pearson_correlation
#' @export
kendall_correlation <- function() {
  new_correlation_method(
    "kendall", "normal_approximation", supports_partial = FALSE,
    supports_interaction = FALSE,
    compute = function(context) compute_builtin_correlation(context, "kendall")
  )
}

#' Construct weighted and repeated-measures correlation methods
#' @return A concrete `correlation_method_spec`.
#' @export
weighted_pearson_correlation <- function() {
  method <- new_correlation_method(
    "weighted_pearson", "fisher_z_effective_n",
    compute = compute_weighted_pearson,
    effect_measure = "weighted_pearson_correlation",
    supports_partial = FALSE, supports_interaction = FALSE
  )
  method$requires_weights <- TRUE
  method$supports_weights <- TRUE
  method
}

#' @rdname weighted_pearson_correlation
#' @export
repeated_measures_correlation <- function() {
  method <- new_correlation_method(
    "repeated_measures", "fisher_z_within_subject",
    compute = compute_repeated_measures_correlation,
    effect_measure = "repeated_measures_correlation",
    supports_partial = FALSE, supports_interaction = FALSE
  )
  method$requires_id <- TRUE
  method$supports_id <- TRUE
  method
}

#' Construct robust and latent-correlation methods
#'
#' `biweight_correlation()` reports a point estimate only: it has no
#' analytic standard error, confidence interval, or p-value. Validated plan
#' rows using it therefore receive status `review` and must either be
#' approved explicitly with [approve_plan()] or wrapped in
#' [resampled_correlation()] to obtain bootstrap inference.
#'
#' @return A concrete `correlation_method_spec`.
#' @export
biweight_correlation <- function() {
  method <- new_correlation_method(
    "biweight", "resampling_recommended", compute = compute_biweight_correlation,
    effect_measure = "biweight_midcorrelation", supports_partial = FALSE,
    supports_interaction = FALSE
  )
  method$provides_inference <- FALSE
  method
}

#' @rdname biweight_correlation
#' @export
polychoric_correlation <- function() {
  method <- new_correlation_method(
    "polychoric", "backend_standard_error", compute = compute_polychoric_correlation,
    effect_measure = "polychoric_correlation", supports_partial = FALSE,
    supports_interaction = FALSE, required_packages = "polycor"
  )
  method$input_type <- "ordered_categorical"
  method
}

#' @rdname biweight_correlation
#' @export
tetrachoric_correlation <- function() {
  method <- polychoric_correlation()
  method$id <- method$function_id <- "tetrachoric"
  method$effect_measure <- "tetrachoric_correlation"
  method$input_type <- "binary_categorical"
  method
}

new_correlation_method <- function(
  id, ci_method, compute, effect_measure = paste0(id, "_correlation"),
  scale = "minus_one_to_one", supports_partial = TRUE,
  supports_strata = TRUE, supports_interaction = TRUE,
  required_packages = "stats", function_hash = NA_character_
) {
  structure(list(
    id = id, estimator = "correlation_coefficient", ci_method = ci_method,
    effect_measure = effect_measure, scale = scale,
    required_packages = required_packages, compute = compute,
    supports_partial = supports_partial, supports_strata = supports_strata,
    supports_interaction = supports_interaction,
    supports_weights = FALSE, supports_id = FALSE,
    requires_weights = FALSE, requires_id = FALSE,
    provides_inference = TRUE,
    input_type = "numeric",
    function_id = id, function_hash = function_hash
  ), class = "correlation_method_spec")
}

#' Construct a custom correlation method
#' @param id Stable method identifier.
#' @param compute Function accepting a read-only `correlation_context` and
#'   returning a value constructed by `correlation_output()`.
#' @param effect_measure Declared effect measure.
#' @param scale Output scale.
#' @param ci_method Confidence-interval method identifier.
#' @param supports_partial,supports_strata,supports_interaction Declared method
#'   capabilities checked before compilation.
#' @param supports_weights,supports_id Whether optional weights or subject IDs
#'   may be supplied to the method.
#' @param required_packages Optional packages checked during preflight.
#' @return A validated `correlation_method_spec`.
#' @export
correlation_method <- function(
  id, compute, effect_measure, scale = "minus_one_to_one", ci_method,
  supports_partial = FALSE, supports_strata = TRUE,
  supports_interaction = FALSE, supports_weights = FALSE, supports_id = FALSE,
  required_packages = character()
) {
  for (value in list(id = id, effect_measure = effect_measure, scale = scale,
                     ci_method = ci_method)) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop_invalid_correlation("Correlation method identifiers must be non-empty strings.")
    }
  }
  if (!is.function(compute)) stop_invalid_correlation("`compute` must be a function.")
  capabilities <- c(
    supports_partial, supports_strata, supports_interaction,
    supports_weights, supports_id
  )
  if (!is.logical(capabilities) || length(capabilities) != 5L || anyNA(capabilities)) {
    stop_invalid_correlation("Correlation method capabilities must be TRUE or FALSE.")
  }
  output <- new_correlation_method(
    id, ci_method, compute, effect_measure, scale, supports_partial,
    supports_strata, supports_interaction, required_packages,
    function_hash = digest::digest(compute)
  )
  output$supports_weights <- supports_weights
  output$supports_id <- supports_id
  output
}

#' Add bootstrap intervals and permutation inference to a correlation method
#'
#' Bootstrap replicates resample observations jointly with their analysis
#' weights. For subject-identified methods whole subjects are resampled
#' (cluster bootstrap) and permutation replicates permute outcome values
#' within subjects only, preserving the within-subject estimand.
#'
#' @param method A correlation method specification.
#' @param bootstrap Number of bootstrap replicates, or zero.
#' @param permutations Number of permutation replicates, or zero.
#' @param seed Required integer seed used without changing global RNG state.
#' @return A resampling `correlation_method_spec`.
#' @examples
#' data <- as_bq_data(tibble::tibble(
#'   x = c(1, 4, 2, 8, 5, 9, 3, 7, 6, 10),
#'   y = c(2, 1, 5, 4, 8, 7, 3, 9, 6, 10)
#' ))
#' method <- resampled_correlation(
#'   pearson_correlation(), bootstrap = 199, permutations = 199, seed = 42
#' )
#' result <- plan_correlations(data, x, with = y, method = method) |>
#'   validate_plan(data) |>
#'   run_analysis(data)
#' correlations(result)[, c("estimate", "conf_low", "conf_high", "p_value")]
#' @export
resampled_correlation <- function(
  method = pearson_correlation(), bootstrap = 999L,
  permutations = 999L, seed
) {
  if (!inherits(method, "correlation_method_spec")) {
    stop_invalid_correlation("`method` must be a correlation method specification.")
  }
  check_replicates <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        value != as.integer(value) || (value != 0L && value < 10L)) {
      stop_invalid_correlation(paste0("`", name, "` must be zero or an integer >= 10."))
    }
    as.integer(value)
  }
  bootstrap <- check_replicates(bootstrap, "bootstrap")
  permutations <- check_replicates(permutations, "permutations")
  if (bootstrap == 0L && permutations == 0L) {
    stop_invalid_correlation("At least one resampling procedure must be requested.")
  }
  if (missing(seed) || !is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      seed != as.integer(seed)) {
    stop_invalid_correlation("`seed` must be an explicitly supplied integer.")
  }
  seed <- as.integer(seed)
  base_method <- method
  compute <- function(context) compute_resampled_correlation(
    context, base_method, bootstrap, permutations, seed
  )
  output <- new_correlation_method(
    id = paste0(method$id, "_resampled"),
    ci_method = if (bootstrap) "bootstrap_percentile" else method$ci_method,
    compute = compute, effect_measure = method$effect_measure,
    scale = method$scale, supports_partial = method$supports_partial,
    supports_strata = method$supports_strata, supports_interaction = FALSE,
    required_packages = method$required_packages,
    function_hash = digest::digest(list(method$function_hash, bootstrap, permutations, seed))
  )
  output$base_method <- method
  output$supports_weights <- method$supports_weights
  output$supports_id <- method$supports_id
  output$requires_weights <- method$requires_weights
  output$requires_id <- method$requires_id
  output$bootstrap_replicates <- bootstrap
  output$permutation_replicates <- permutations
  output$resampling_seed <- seed
  output
}

compute_resampled_correlation <- function(
  context, method, bootstrap, permutations, seed
) {
  with_local_seed(seed, {
    observed <- method$compute(context)
    bootstrap_values <- if (bootstrap) replicate(bootstrap, {
      sampled <- resample_correlation_context(context)
      tryCatch(method$compute(sampled)$estimate, error = function(e) NA_real_)
    }) else numeric()
    permutation_values <- if (permutations) replicate(permutations, {
      permuted <- permute_correlation_context(context)
      tryCatch(method$compute(permuted)$estimate, error = function(e) NA_real_)
    }) else numeric()
    bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]
    permutation_values <- permutation_values[is.finite(permutation_values)]
    if (bootstrap && length(bootstrap_values) < 2L) {
      stop_invalid_correlation_output("Too few successful bootstrap replicates.")
    }
    if (permutations && !length(permutation_values)) {
      stop_invalid_correlation_output("No successful permutation replicates.")
    }
    if (bootstrap) {
      alpha <- (1 - context$confidence_level) / 2
      ci <- stats::quantile(
        bootstrap_values, c(alpha, 1 - alpha), names = FALSE, type = 6
      )
      std_error <- stats::sd(bootstrap_values)
      se_scale <- "correlation"
    } else {
      ci <- c(observed$conf_low, observed$conf_high)
      std_error <- observed$std_error
      se_scale <- observed$std_error_scale
    }
    p_value <- if (permutations) {
      (1 + sum(abs(permutation_values) >= abs(observed$estimate))) /
        (length(permutation_values) + 1)
    } else observed$p_value
    output <- correlation_output(
      observed$estimate, std_error, se_scale, ci[[1]], ci[[2]],
      observed$statistic, observed$df, p_value
    )
    attr(output, "bootstrap_successful") <- as.integer(length(bootstrap_values))
    attr(output, "permutation_successful") <- as.integer(length(permutation_values))
    output
  })
}

resample_correlation_context <- function(context) {
  sampled <- context
  n <- length(context$x)
  if (is.null(context$id)) {
    index <- sample.int(n, n, replace = TRUE)
  } else {
    # Cluster bootstrap: resample whole subjects and relabel the draws so
    # that a subject drawn twice contributes two distinct clusters.
    subjects <- unique(context$id)
    drawn <- subjects[
      sample.int(length(subjects), length(subjects), replace = TRUE)
    ]
    rows <- lapply(drawn, function(subject) which(context$id == subject))
    index <- unlist(rows, use.names = FALSE)
    sampled$id <- rep(seq_along(rows), lengths(rows))
  }
  sampled$x <- context$x[index]
  sampled$y <- context$y[index]
  sampled$adjustment <- context$adjustment[index, , drop = FALSE]
  if (!is.null(context$weights)) sampled$weights <- context$weights[index]
  sampled
}

permute_correlation_context <- function(context) {
  permuted <- context
  if (is.null(context$id)) {
    permuted$y <- sample(context$y, length(context$y), replace = FALSE)
    return(permuted)
  }
  # Within-subject permutation preserves subject means, matching the
  # within-subject estimand of repeated-measures correlation.
  y <- context$y
  for (rows in split(seq_along(y), context$id)) {
    y[rows] <- y[rows][sample.int(length(rows))]
  }
  permuted$y <- y
  permuted
}

with_local_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else if (
      exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    ) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

#' Construct custom correlation output
#' @param estimate,std_error,conf_low,conf_high,statistic,df,p_value Numeric scalars.
#' @param std_error_scale Scale on which `std_error` is defined.
#' @return A validated `correlation_method_output`.
#' @export
correlation_output <- function(
  estimate, std_error, std_error_scale, conf_low, conf_high,
  statistic, df, p_value
) {
  values <- c(estimate, std_error, conf_low, conf_high, statistic, df, p_value)
  if (!is.numeric(values) || length(values) != 7L) {
    stop_invalid_correlation_output("Correlation output values must be numeric scalars.")
  }
  if (!is.character(std_error_scale) || length(std_error_scale) != 1L ||
      is.na(std_error_scale) || !nzchar(std_error_scale)) {
    stop_invalid_correlation_output("`std_error_scale` must be one non-empty string.")
  }
  structure(list(
    estimate = as.numeric(estimate), std_error = as.numeric(std_error),
    std_error_scale = std_error_scale, conf_low = as.numeric(conf_low),
    conf_high = as.numeric(conf_high), statistic = as.numeric(statistic),
    df = as.numeric(df), p_value = as.numeric(p_value)
  ), class = "correlation_method_output")
}

#' Construct a correlation comparator
#' @param id Stable comparator identifier.
#' @param compare Function accepting a read-only comparison context and
#'   returning `correlation_comparison_output()`.
#' @param methods Correlation method identifiers supported by the comparator.
#' @param required_packages Optional packages checked during preflight.
#' @return A `correlation_comparator_spec`.
#' @export
correlation_comparator <- function(
  id, compare, methods, required_packages = character()
) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id) ||
      !is.function(compare) || !is.character(methods) || !length(methods) ||
      anyNA(methods)) {
    stop_invalid_correlation("Invalid correlation comparator contract.")
  }
  structure(list(
    id = id, compare = compare, methods = unique(methods),
    required_packages = required_packages, function_id = id,
    function_hash = digest::digest(compare)
  ), class = "correlation_comparator_spec")
}

#' Construct correlation comparison output
#' @param statistic,df,p_value Omnibus test values.
#' @param contrasts A data frame containing pairwise comparison estimates.
#' @return A `correlation_comparison_output`.
#' @export
correlation_comparison_output <- function(statistic, df, p_value, contrasts) {
  values <- c(statistic, df, p_value)
  if (!is.numeric(values) || length(values) != 3L ||
      !inherits(contrasts, "data.frame")) {
    stop_invalid_correlation_output("Invalid correlation comparison output.")
  }
  structure(list(
    statistic = as.numeric(statistic), df = as.numeric(df),
    p_value = as.numeric(p_value), contrasts = tibble::as_tibble(contrasts)
  ), class = "correlation_comparison_output")
}

fisher_z_comparator <- function() {
  correlation_comparator(
    "fisher_z_heterogeneity", methods = "pearson",
    compare = compute_fisher_z_comparison
  )
}


stop_invalid_correlation <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_correlation", "error", "condition")))
}

stop_invalid_correlation_output <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)), class = c(
    "bq_error_invalid_correlation_output", "error", "condition"
  )))
}
