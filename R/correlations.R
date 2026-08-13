#' Construct correlation methods
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
    "spearman", "fisher_z_approximation", supports_interaction = FALSE,
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
#' @param required_packages Optional packages checked during preflight.
#' @return A validated `correlation_method_spec`.
#' @export
correlation_method <- function(
  id, compute, effect_measure, scale = "minus_one_to_one", ci_method,
  supports_partial = FALSE, supports_strata = TRUE,
  supports_interaction = FALSE, required_packages = character()
) {
  for (value in list(id = id, effect_measure = effect_measure, scale = scale,
                     ci_method = ci_method)) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop_invalid_correlation("Correlation method identifiers must be non-empty strings.")
    }
  }
  if (!is.function(compute)) stop_invalid_correlation("`compute` must be a function.")
  capabilities <- c(supports_partial, supports_strata, supports_interaction)
  if (!is.logical(capabilities) || length(capabilities) != 3L || anyNA(capabilities)) {
    stop_invalid_correlation("Correlation method capabilities must be TRUE or FALSE.")
  }
  new_correlation_method(
    id, ci_method, compute, effect_measure, scale, supports_partial,
    supports_strata, supports_interaction, required_packages,
    function_hash = digest::digest(compute)
  )
}

#' Add bootstrap intervals and permutation inference to a correlation method
#' @param method A correlation method specification.
#' @param bootstrap Number of bootstrap replicates, or zero.
#' @param permutations Number of permutation replicates, or zero.
#' @param seed Required integer seed used without changing global RNG state.
#' @return A resampling `correlation_method_spec`.
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
    n <- length(context$x)
    bootstrap_values <- if (bootstrap) replicate(bootstrap, {
      index <- sample.int(n, n, replace = TRUE)
      sampled <- context
      sampled$x <- context$x[index]; sampled$y <- context$y[index]
      sampled$adjustment <- context$adjustment[index, , drop = FALSE]
      tryCatch(method$compute(sampled)$estimate, error = function(e) NA_real_)
    }) else numeric()
    permutation_values <- if (permutations) replicate(permutations, {
      permuted <- context
      permuted$y <- sample(context$y, n, replace = FALSE)
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

#' Compile a correlation analysis plan
#'
#' @param .data A `bq_data` object.
#' @param variables Numeric variables selected with tidyselect.
#' @param with Optional second variable set. If omitted, unique pairs within
#'   `variables` are compiled.
#' @param method A correlation method specification.
#' @param adjust_for Optional numeric covariates for partial correlation.
#' @param strata Optional variables defining independent correlation strata.
#' @param interaction_test Whether to test equality of Pearson correlations
#'   across strata and compute pairwise Fisher z contrasts.
#' @param comparator Optional comparator used when `interaction_test = TRUE`.
#' @param missing Pairwise or common complete-case analysis.
#' @param confidence_level Confidence level.
#' @param adjust Multiplicity adjustment accepted by [stats::p.adjust()].
#' @return An `analysis_plan` with one row per unique variable pair.
#' @export
plan_correlations <- function(
  .data, variables = where_continuous(), with = NULL,
  adjust_for = tidyselect::any_of(character()),
  strata = tidyselect::any_of(character()),
  interaction_test = FALSE,
  comparator = NULL,
  method = pearson_correlation(), missing = c("pairwise", "complete"),
  confidence_level = 0.95, adjust = "none"
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  missing <- match.arg(missing)
  if (!inherits(method, "correlation_method_spec")) {
    stop_invalid_correlation("`method` must be a correlation method specification.")
  }
  if (!is.character(adjust) || length(adjust) != 1L ||
      !adjust %in% stats::p.adjust.methods) {
    stop_invalid_correlation("`adjust` is not supported.")
  }
  left <- names(tidyselect::eval_select(rlang::enquo(variables), .data))
  with_quo <- rlang::enquo(with)
  right <- if (rlang::quo_is_null(with_quo)) left else
    names(tidyselect::eval_select(with_quo, .data))
  adjustment_names <- names(tidyselect::eval_select(
    rlang::enquo(adjust_for), .data
  ))
  strata_names <- names(tidyselect::eval_select(
    rlang::enquo(strata), .data
  ))
  if (!is.logical(interaction_test) || length(interaction_test) != 1L ||
      is.na(interaction_test)) {
    stop_invalid_correlation("`interaction_test` must be `TRUE` or `FALSE`.")
  }
  if (interaction_test && !length(strata_names)) {
    stop_invalid_correlation("`interaction_test = TRUE` requires `strata`.")
  }
  if (interaction_test && is.null(comparator)) comparator <- fisher_z_comparator()
  if (!is.null(comparator) && !inherits(comparator, "correlation_comparator_spec")) {
    stop_invalid_correlation("`comparator` must be a correlation comparator.")
  }
  if (interaction_test && !method$id %in% comparator$methods) {
    stop_invalid_correlation(paste0(
      "Comparator `", comparator$id, "` does not support method `", method$id, "`."
    ))
  }
  if (length(adjustment_names) && !isTRUE(method$supports_partial)) {
    stop_invalid_correlation(paste0(
      "Correlation method `", method$id, "` does not support partial correlation."
    ))
  }
  if (length(strata_names) && !isTRUE(method$supports_strata)) {
    stop_invalid_correlation(paste0(
      "Correlation method `", method$id, "` does not support strata."
    ))
  }
  overlap <- intersect(unique(c(left, right)), adjustment_names)
  if (length(overlap)) stop_invalid_correlation(paste0(
    "Adjustment variables must differ from correlated variables: ",
    paste(overlap, collapse = ", "), "."
  ))
  pairs <- if (identical(left, right) && rlang::quo_is_null(with_quo)) {
    if (length(left) < 2L) matrix(character(), nrow = 2L) else utils::combn(left, 2L)
  } else {
    grid <- expand.grid(x = left, y = right, stringsAsFactors = FALSE)
    grid <- grid[grid$x != grid$y, , drop = FALSE]
    keys <- vapply(seq_len(nrow(grid)), function(i) {
      paste(sort(c(grid$x[[i]], grid$y[[i]])), collapse = "\r")
    }, character(1))
    grid <- grid[!duplicated(keys), , drop = FALSE]
    rbind(grid$x, grid$y)
  }
  if (ncol(pairs) == 0L) return(empty_analysis_plan())
  registry <- tibble::as_tibble(
    attr(.data, "variable_registry", exact = TRUE)
  )
  selected_names <- unique(c(left, right, adjustment_names))
  selected_ids <- registry$var_id[match(selected_names, registry$name)]
  adjustment_ids <- registry$var_id[match(adjustment_names, registry$name)]
  stratum_specs <- compile_stratum_specs(.data, strata_names, registry)
  task_index <- expand.grid(
    pair = seq_len(ncol(pairs)), stratum = seq_along(stratum_specs),
    KEEP.OUT.ATTRS = FALSE
  )
  family_ids <- paste0(
    "correlation_family_", uuid::UUIDgenerate(n = length(stratum_specs))
  )
  interaction_ids <- paste0(
    "correlation_interaction_", uuid::UUIDgenerate(n = ncol(pairs))
  )
  rows <- lapply(seq_len(nrow(task_index)), function(task) {
    i <- task_index$pair[[task]]
    stratum_i <- task_index$stratum[[task]]
    stratum_spec <- stratum_specs[[stratum_i]]
    x_spec <- registry[match(pairs[1, i], registry$name), , drop = FALSE]
    y_spec <- registry[match(pairs[2, i], registry$name), , drop = FALSE]
    row <- analysis_plan_row(
      x_spec, y_spec, method = NULL, status = "ready", reason = NA_character_,
      confidence_level = confidence_level, stratum_spec = stratum_spec
    )
    row$analysis_type <- "correlation"
    row$variable_x_id <- x_spec$var_id[[1]]
    row$variable_y_id <- y_spec$var_id[[1]]
    row$variable_x <- x_spec$name[[1]]
    row$variable_y <- y_spec$name[[1]]
    transformation_ids <- unique(c(
      x_spec$var_id[[1]], y_spec$var_id[[1]], adjustment_ids
    ))
    transformations <- lapply(transformation_ids, function(id) {
      registry$transformation[[match(id, registry$var_id)]]
    })
    names(transformations) <- transformation_ids
    row$transformation_specs <- list(transformations)
    row$correlation_family_id <- family_ids[[stratum_i]]
    row$correlation_interaction_id <- interaction_ids[[i]]
    row$interaction_test <- interaction_test
    row$correlation_comparator <- list(comparator)
    row$correlation_comparator_id <- if (is.null(comparator)) NA_character_ else comparator$id
    row$correlation_comparator_hash <- if (is.null(comparator)) NA_character_ else
      comparator$function_hash
    row$correlation_variable_ids <- list(selected_ids)
    row$adjustment_ids <- list(adjustment_ids)
    row$adjustment_variables <- list(adjustment_names)
    row$estimand <- if (length(adjustment_ids)) "partial_correlation" else "correlation"
    row$method <- method$id
    row$engine <- "stats_cor_test"
    row$estimator <- method$estimator
    row$ci_method <- method$ci_method
    row$effect_measure <- method$effect_measure
    row$model_scale <- method$scale
    row$scale <- method$scale
    row$missing_policy <- missing
    row$adjust_method <- adjust
    row$required_packages <- list(unique(c(
      method$required_packages,
      if (is.null(comparator)) character() else comparator$required_packages
    )))
    row$function_id <- method$function_id
    row$function_hash <- method$function_hash
    row$method_object <- list(method)
    row$bootstrap_replicates <- if (is.null(method$bootstrap_replicates)) 0L else
      method$bootstrap_replicates
    row$permutation_replicates <- if (is.null(method$permutation_replicates)) 0L else
      method$permutation_replicates
    row$resampling_seed <- if (is.null(method$resampling_seed)) NA_integer_ else
      method$resampling_seed
    row$formula <- list(NULL)
    row$validated <- FALSE
    row
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_correlation_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  x_row <- match(plan$variable_x_id[[i]], registry$var_id)
  y_row <- match(plan$variable_y_id[[i]], registry$var_id)
  issues <- character()
  stratum_rows <- match(plan$stratum_ids[[i]], registry$var_id)
  if (anyNA(stratum_rows)) {
    issues <- c(issues, "A correlation stratification variable is absent.")
    analysis_data <- data[0, , drop = FALSE]
  } else {
    current_strata <- registry$name[stratum_rows]
    plan$strata[[i]] <- current_strata
    names(plan$stratum_values[[i]]) <- current_strata
    analysis_data <- data[stratum_mask(
      data, current_strata, plan$stratum_values[[i]]
    ), , drop = FALSE]
  }
  if (is.na(x_row) || is.na(y_row)) {
    issues <- c(issues, "A correlation variable referenced by stable id is absent.")
  } else {
    plan$variable_x[[i]] <- registry$name[[x_row]]
    plan$variable_y[[i]] <- registry$name[[y_row]]
    x <- correlation_analysis_vector(analysis_data, plan[i, , drop = FALSE],
      registry$name[[x_row]], plan$variable_x_id[[i]])
    y <- correlation_analysis_vector(analysis_data, plan[i, , drop = FALSE],
      registry$name[[y_row]], plan$variable_y_id[[i]])
    if (inherits(x, "error")) issues <- c(issues, conditionMessage(x))
    if (inherits(y, "error")) issues <- c(issues, conditionMessage(y))
    if (!inherits(x, "error") && !inherits(y, "error")) {
      adjustment_values <- lapply(plan$adjustment_ids[[i]], function(id) {
        row <- match(id, registry$var_id)
        if (is.na(row)) return(simpleError(
          "An adjustment variable referenced by stable id is absent."
        ))
        correlation_analysis_vector(
          analysis_data, plan[i, , drop = FALSE], registry$name[[row]], id
        )
      })
      adjustment_errors <- vapply(adjustment_values, inherits, logical(1), "error")
      if (any(adjustment_errors)) {
        issues <- c(issues, vapply(
          adjustment_values[adjustment_errors], conditionMessage, character(1)
        ))
      }
      mask <- correlation_complete_mask(
        plan[i, , drop = FALSE], analysis_data, x, y
      )
      n <- sum(mask)
      plan$n_total[[i]] <- nrow(analysis_data)
      plan$n_eligible[[i]] <- nrow(analysis_data)
      plan$n_analyzed[[i]] <- n
      plan$n_missing_outcome[[i]] <- sum(is.na(x))
      plan$n_missing_predictor[[i]] <- sum(is.na(y))
      minimum <- if (plan$method[[i]] %in% c("pearson", "spearman")) 4L else 3L
      if (length(plan$adjustment_ids[[i]])) {
        minimum <- max(minimum, length(plan$adjustment_ids[[i]]) + 4L)
      }
      if (n < minimum) issues <- c(issues, paste0(
        "Correlation requires at least ", minimum, " complete observations."
      ))
      if (n > 0L && (n_distinct_values(x[mask]) < 2L ||
          n_distinct_values(y[mask]) < 2L)) {
        issues <- c(issues, "A correlation variable has no variation.")
      }
      if (length(plan$adjustment_ids[[i]]) &&
          n - length(plan$adjustment_ids[[i]]) - 2L <= 0L) {
        issues <- c(issues, "Partial correlation has no residual degrees of freedom.")
      }
      if (length(adjustment_values) && !any(adjustment_errors) && n > 0L) {
        adjustment_matrix <- do.call(cbind, lapply(adjustment_values, `[`, mask))
        if (qr(cbind(1, adjustment_matrix))$rank < ncol(adjustment_matrix) + 1L) {
          issues <- c(issues, "Adjustment variables are linearly dependent.")
        }
      }
    }
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

correlation_analysis_vector <- function(data, spec, name, id) {
  tryCatch({
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    if (!is.numeric(value)) stop_invalid_correlation(paste0(
      "Correlation variable `", name, "` must be numeric."
    ))
    apply_transformation_spec(
      value, spec$transformation_specs[[1]][[id]], name,
      spec$analysis_id[[1]]
    )
  }, error = function(condition) condition)
}

correlation_complete_mask <- function(spec, data, x, y) {
  if (spec$missing_policy[[1]] == "pairwise") {
    ids <- unique(c(
      spec$variable_x_id[[1]], spec$variable_y_id[[1]],
      spec$adjustment_ids[[1]]
    ))
  } else ids <- spec$correlation_variable_ids[[1]]
  registry <- variables(data)
  mask <- rep(TRUE, nrow(data))
  for (id in ids) {
    row <- match(id, registry$var_id)
    if (is.na(row)) return(rep(FALSE, nrow(data)))
    value <- correlation_analysis_vector(
      data, spec, registry$name[[row]], id
    )
    if (inherits(value, "error")) return(rep(FALSE, nrow(data)))
    mask <- mask & !is.na(value)
  }
  mask
}

execute_correlation <- function(spec, data) {
  data <- data[stratum_mask(
    data, spec$strata[[1]], spec$stratum_values[[1]]
  ), , drop = FALSE]
  registry <- variables(data)
  x <- correlation_analysis_vector(
    data, spec, spec$variable_x[[1]], spec$variable_x_id[[1]]
  )
  y <- correlation_analysis_vector(
    data, spec, spec$variable_y[[1]], spec$variable_y_id[[1]]
  )
  mask <- correlation_complete_mask(spec, data, x, y)
  x <- x[mask]; y <- y[mask]
  method <- spec$method[[1]]
  adjustment_ids <- spec$adjustment_ids[[1]]
  k <- length(adjustment_ids)
  adjustment <- if (k) {
    value <- vapply(adjustment_ids, function(id) {
      row <- match(id, registry$var_id)
      correlation_analysis_vector(data, spec, registry$name[[row]], id)[mask]
    }, numeric(length(x)))
    if (is.null(dim(value))) matrix(value, ncol = 1L) else value
  } else matrix(numeric(), nrow = length(x), ncol = 0L)
  context <- structure(list(
    analysis_id = spec$analysis_id[[1]], x = x, y = y,
    adjustment = adjustment, confidence_level = spec$confidence_level[[1]],
    estimand = spec$estimand[[1]], method = method,
    stratum_label = spec$stratum_label[[1]], missing_policy = spec$missing_policy[[1]]
  ), class = "correlation_context")
  method_object <- spec$method_object[[1]]
  output <- method_object$compute(context)
  output <- validate_correlation_method_output(output, method_object)
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    correlation_family_id = spec$correlation_family_id[[1]],
    variable_x_id = spec$variable_x_id[[1]], variable_y_id = spec$variable_y_id[[1]],
    variable_x = spec$variable_x[[1]], variable_y = spec$variable_y[[1]],
    stratum_label = spec$stratum_label[[1]],
    strata = list(spec$strata[[1]]),
    correlation_interaction_id = spec$correlation_interaction_id[[1]],
    interaction_test = spec$interaction_test[[1]],
    correlation_comparator = list(spec$correlation_comparator[[1]]),
    correlation_comparator_id = spec$correlation_comparator_id[[1]],
    transformation_x = transformation_id_for(spec, spec$variable_x_id[[1]]),
    transformation_y = transformation_id_for(spec, spec$variable_y_id[[1]]),
    adjustment_variables = list(spec$adjustment_variables[[1]]),
    n_adjustment = as.integer(k), estimand = spec$estimand[[1]],
    estimate = output$estimate, std_error = output$std_error,
    std_error_scale = output$std_error_scale, conf_low = output$conf_low,
    conf_high = output$conf_high, statistic = output$statistic,
    df = output$df, p_value = output$p_value, p_adjusted = NA_real_,
    adjust_method = spec$adjust_method[[1]], effect_measure = spec$effect_measure[[1]],
    scale = spec$scale[[1]], n = as.integer(length(x)), method = method,
    ci_method = spec$ci_method[[1]], missing_policy = spec$missing_policy[[1]],
    confidence_level = spec$confidence_level[[1]],
    bootstrap_replicates = spec$bootstrap_replicates[[1]],
    permutation_replicates = spec$permutation_replicates[[1]],
    resampling_seed = spec$resampling_seed[[1]],
    bootstrap_successful = if (is.null(attr(output, "bootstrap_successful"))) 0L else
      attr(output, "bootstrap_successful"),
    permutation_successful = if (is.null(attr(output, "permutation_successful"))) 0L else
      attr(output, "permutation_successful")
  )
}

compute_builtin_correlation <- function(context, method) {
  x <- context$x; y <- context$y
  adjustment <- context$adjustment
  k <- ncol(adjustment)
  if (k) {
    if (method == "spearman") {
      x <- rank(x); y <- rank(y)
      adjustment <- apply(adjustment, 2L, rank)
      if (is.null(dim(adjustment))) adjustment <- matrix(adjustment, ncol = 1L)
    }
    x <- stats::residuals(stats::lm.fit(cbind(1, adjustment), x))
    y <- stats::residuals(stats::lm.fit(cbind(1, adjustment), y))
    estimate <- stats::cor(x, y)
    df <- length(x) - k - 2L
    statistic <- estimate * sqrt(df / (1 - estimate^2))
    p_value <- 2 * stats::pt(abs(statistic), df, lower.tail = FALSE)
  } else {
    test <- suppressWarnings(stats::cor.test(
      x, y, method = method, exact = FALSE,
      conf.level = context$confidence_level
    ))
    estimate <- unname(test$estimate)
    statistic <- as.numeric(test$statistic)
    df <- if (is.null(test$parameter)) NA_real_ else as.numeric(test$parameter)
    p_value <- test$p.value
  }
  critical <- stats::qnorm((1 + context$confidence_level) / 2)
  if (method %in% c("pearson", "spearman")) {
    std_error <- 1 / sqrt(length(x) - k - 3)
    z <- atanh(estimate)
    ci <- tanh(z + c(-1, 1) * critical * std_error)
    se_scale <- if (method == "pearson") "fisher_z" else
      "fisher_z_approximation"
  } else {
    std_error <- if (is.finite(statistic) && statistic != 0) {
      abs(estimate / statistic)
    } else sqrt(2 * (2 * length(x) + 5) / (9 * length(x) * (length(x) - 1)))
    ci <- pmax(-1, pmin(1, estimate + c(-1, 1) * critical * std_error))
    se_scale <- "kendall_tau"
  }
  correlation_output(
    estimate, std_error, se_scale, ci[[1]], ci[[2]], statistic, df, p_value
  )
}

validate_correlation_method_output <- function(output, method) {
  if (!inherits(output, "correlation_method_output")) {
    stop_invalid_correlation_output(
      "A correlation method must return `correlation_output()`."
    )
  }
  finite_or_na <- function(value) is.na(value) || is.finite(value)
  if (!finite_or_na(output$estimate) || !finite_or_na(output$std_error) ||
      !finite_or_na(output$conf_low) || !finite_or_na(output$conf_high) ||
      !finite_or_na(output$p_value)) {
    stop_invalid_correlation_output(
      paste0("Correlation method `", method$id, "` returned non-finite output.")
    )
  }
  if (!is.na(output$p_value) && (output$p_value < 0 || output$p_value > 1)) {
    stop_invalid_correlation_output("Correlation p-value must lie in [0, 1].")
  }
  output
}

compute_correlation_interactions <- function(correlation_output) {
  requested <- correlation_output$interaction_test
  requested[is.na(requested)] <- FALSE
  output <- correlation_output[requested, , drop = FALSE]
  if (!nrow(output)) {
    return(list(tests = tests_prototype(), contrasts = contrasts_prototype()))
  }
  families <- split(output, output$correlation_interaction_id)
  computed <- lapply(families, compute_correlation_interaction_family)
  list(
    tests = do.call(vctrs::vec_rbind, lapply(computed, `[[`, "tests")),
    contrasts = do.call(vctrs::vec_rbind, lapply(computed, `[[`, "contrasts"))
  )
}

compute_correlation_interaction_family <- function(rows) {
  if (nrow(rows) < 2L) {
    return(list(tests = tests_prototype(), contrasts = contrasts_prototype()))
  }
  comparator <- rows$correlation_comparator[[1]]
  context <- structure(list(
    estimates = rows, confidence_level = rows$confidence_level[[1]],
    adjust_method = rows$adjust_method[[1]],
    variable_x = rows$variable_x[[1]], variable_y = rows$variable_y[[1]],
    strata = rows$strata[[1]]
  ), class = "correlation_comparison_context")
  output <- comparator$compare(context)
  normalize_correlation_comparison_output(output, rows, comparator)
}

compute_fisher_z_comparison <- function(context) {
  rows <- context$estimates
  z <- atanh(rows$estimate)
  variance <- rows$std_error^2
  weights <- 1 / variance
  weighted_mean <- sum(weights * z) / sum(weights)
  statistic <- sum(weights * (z - weighted_mean)^2)
  df <- nrow(rows) - 1L
  pairs <- utils::combn(seq_len(nrow(rows)), 2L)
  critical <- stats::qnorm((1 + rows$confidence_level[[1]]) / 2)
  contrast_rows <- lapply(seq_len(ncol(pairs)), function(i) {
    numerator_i <- pairs[1L, i]
    denominator_i <- pairs[2L, i]
    estimate <- z[[numerator_i]] - z[[denominator_i]]
    std_error <- sqrt(variance[[numerator_i]] + variance[[denominator_i]])
    statistic <- estimate / std_error
    tibble::tibble(
      numerator = rows$stratum_label[[numerator_i]],
      denominator = rows$stratum_label[[denominator_i]],
      estimate = estimate,
      std_error = std_error, std_error_scale = "fisher_z",
      conf_low = estimate - critical * std_error,
      conf_high = estimate + critical * std_error,
      p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      effect_measure = "difference_in_fisher_z", scale = "fisher_z"
    )
  })
  contrast <- do.call(vctrs::vec_rbind, contrast_rows)
  correlation_comparison_output(
    statistic, df, stats::pchisq(statistic, df, lower.tail = FALSE), contrast
  )
}

normalize_correlation_comparison_output <- function(output, rows, comparator) {
  if (!inherits(output, "correlation_comparison_output")) {
    stop_invalid_correlation_output(
      "A correlation comparator must return `correlation_comparison_output()`."
    )
  }
  required <- c(
    "numerator", "denominator", "estimate", "std_error", "std_error_scale",
    "conf_low", "conf_high", "p_value", "effect_measure", "scale"
  )
  if (length(setdiff(required, names(output$contrasts)))) {
    stop_invalid_correlation_output("Correlation comparator contrasts are malformed.")
  }
  interaction_id <- rows$correlation_interaction_id[[1]]
  test <- tibble::tibble(
    analysis_id = interaction_id, outcome = rows$variable_x[[1]],
    predictor = rows$variable_y[[1]], contrast = NA_character_,
    numerator = NA_character_, denominator = NA_character_,
    test = "correlation_interaction", statistic = output$statistic,
    df = output$df, p_value = output$p_value, p_adjusted = NA_real_,
    adjust_method = "none", method = comparator$id
  )
  x <- output$contrasts
  contrasts <- tibble::tibble(
    analysis_id = interaction_id, outcome = rows$variable_x[[1]],
    predictor = rows$variable_y[[1]],
    contrast_id = paste0("contrast_", uuid::UUIDgenerate(n = nrow(x))),
    contrast = paste0(x$numerator, " - ", x$denominator),
    numerator = as.character(x$numerator), denominator = as.character(x$denominator),
    modifier = paste(rows$strata[[1]], collapse = " + "),
    modifier_level = NA_character_, inner_contrast = NA_character_,
    outer_contrast = NA_character_, estimand = "correlation_difference",
    exponentiated = FALSE, estimate = as.numeric(x$estimate),
    std_error = as.numeric(x$std_error),
    std_error_scale = as.character(x$std_error_scale),
    conf_low = as.numeric(x$conf_low), conf_high = as.numeric(x$conf_high),
    p_value = as.numeric(x$p_value), p_adjusted = NA_real_,
    adjust_method = rows$adjust_method[[1]],
    effect_measure = as.character(x$effect_measure), scale = as.character(x$scale)
  )
  contrasts$p_adjusted <- stats::p.adjust(
    contrasts$p_value, method = contrasts$adjust_method[[1]]
  )
  list(tests = test, contrasts = contrasts)
}

transformation_id_for <- function(spec, id) {
  transformation <- spec$transformation_specs[[1]][[id]]
  if (is.null(transformation)) NA_character_ else transformation$id
}

correlations_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), correlation_family_id = character(),
    variable_x_id = character(), variable_y_id = character(),
    variable_x = character(), variable_y = character(),
    stratum_label = character(), strata = list(),
    correlation_interaction_id = character(), interaction_test = logical(),
    correlation_comparator = list(), correlation_comparator_id = character(),
    transformation_x = character(), transformation_y = character(),
    adjustment_variables = list(), n_adjustment = integer(), estimand = character(),
    estimate = double(), std_error = double(), std_error_scale = character(),
    conf_low = double(), conf_high = double(), statistic = double(), df = double(),
    p_value = double(), p_adjusted = double(), adjust_method = character(),
    effect_measure = character(), scale = character(), n = integer(),
    method = character(), ci_method = character(), missing_policy = character(),
    confidence_level = double(), bootstrap_replicates = integer(),
    permutation_replicates = integer(), resampling_seed = integer(),
    bootstrap_successful = integer(), permutation_successful = integer()
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
