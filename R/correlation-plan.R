# Correlation plan compilation and preflight validation.

#' Compile a correlation analysis plan
#'
#' @param .data A `bq_data` object.
#' @param variables Numeric variables selected with tidyselect.
#' @param with Optional second variable set. If omitted, unique pairs within
#'   `variables` are compiled.
#' @param method A correlation method specification.
#' @param adjust_for Optional numeric covariates for partial correlation.
#' @param strata Optional variables defining independent correlation strata.
#' @param weights Optional numeric analysis-weight column.
#' @param id Optional subject identifier for repeated-measures methods.
#' @param interaction_test Whether to test equality of Pearson correlations
#'   across strata and compute pairwise Fisher z contrasts.
#' @param comparator Optional comparator used when `interaction_test = TRUE`.
#' @param missing Pairwise or common complete-case analysis.
#' @param confidence_level Confidence level.
#' @param adjust Multiplicity adjustment accepted by [stats::p.adjust()].
#' @return An `analysis_plan` with one row per unique variable pair.
#' @examples
#' data <- as_bq_data(tibble::tibble(
#'   crp = c(1.2, 5.4, 2.8, 9.1, 4.4, 6.0),
#'   bmi = c(21.4, 27.9, 24.2, 30.1, 26.6, 23.0)
#' ))
#' result <- plan_correlations(data, crp, with = bmi) |>
#'   validate_plan(data) |>
#'   run_analysis(data)
#' correlations(result)
#' @export
plan_correlations <- function(
  .data, variables = where_continuous(), with = NULL,
  adjust_for = tidyselect::any_of(character()),
  strata = tidyselect::any_of(character()),
  weights = tidyselect::any_of(character()),
  id = tidyselect::any_of(character()),
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
  weight_names <- names(tidyselect::eval_select(rlang::enquo(weights), .data))
  id_names <- names(tidyselect::eval_select(rlang::enquo(id), .data))
  if (length(weight_names) > 1L || length(id_names) > 1L) {
    stop_invalid_correlation("Select at most one weight and one subject ID.")
  }
  if (isTRUE(method$requires_weights) && !length(weight_names)) {
    stop_invalid_correlation(paste0("Method `", method$id, "` requires `weights`."))
  }
  if (length(weight_names) && !isTRUE(method$supports_weights)) {
    stop_invalid_correlation(paste0("Method `", method$id, "` does not support weights."))
  }
  if (isTRUE(method$requires_id) && !length(id_names)) {
    stop_invalid_correlation(paste0("Method `", method$id, "` requires `id`."))
  }
  if (length(id_names) && !isTRUE(method$supports_id)) {
    stop_invalid_correlation(paste0("Method `", method$id, "` does not support subject IDs."))
  }
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
  selected_names <- unique(c(left, right, adjustment_names, weight_names, id_names))
  selected_ids <- registry$var_id[match(selected_names, registry$name)]
  adjustment_ids <- registry$var_id[match(adjustment_names, registry$name)]
  weight_id <- registry$var_id[match(weight_names, registry$name)]
  subject_id <- registry$var_id[match(id_names, registry$name)]
  stratum_specs <- compile_stratum_specs(.data, strata_names, registry)
  task_index <- expand.grid(
    pair = seq_len(ncol(pairs)), stratum = seq_along(stratum_specs),
    KEEP.OUT.ATTRS = FALSE
  )
  family_ids <- vapply(stratum_specs, function(stratum_spec) bq_id(
    "correlation_family", stratum_spec$label, stratum_spec$values,
    selected_ids, method$id, method$function_hash, adjust
  ), character(1))
  interaction_ids <- vapply(seq_len(ncol(pairs)), function(i) bq_id(
    "correlation_interaction", pairs[1L, i], pairs[2L, i], method$id,
    strata_names, if (is.null(comparator)) NA_character_ else comparator$id
  ), character(1))
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
    row$weight_id <- if (length(weight_id)) weight_id else NA_character_
    row$weight <- if (length(weight_names)) weight_names else NA_character_
    row$correlation_subject_id <- if (length(subject_id)) subject_id else NA_character_
    row$correlation_subject <- if (length(id_names)) id_names else NA_character_
    row$estimand <- if (isTRUE(method$requires_id)) "within_subject_correlation" else
      if (length(adjustment_ids)) "partial_correlation" else "correlation"
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
    row <- refine_analysis_id(
      row, row$variable_x_id[[1]], row$variable_y_id[[1]],
      row$stratum_label[[1]], stratum_spec$values, method$id,
      method$function_hash, adjustment_ids, row$weight_id[[1]],
      row$correlation_subject_id[[1]], missing, adjust, confidence_level,
      interaction_test, row$correlation_comparator_id[[1]]
    )
    row
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_correlation_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  x_row <- match(plan$variable_x_id[[i]], registry$var_id)
  y_row <- match(plan$variable_y_id[[i]], registry$var_id)
  issues <- character()
  missing_packages <- plan$required_packages[[i]][
    !vapply(plan$required_packages[[i]], requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) issues <- c(issues, paste0(
    "Missing packages required by correlation method: ",
    paste(missing_packages, collapse = ", "), "."
  ))
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
  if (!is.na(plan$weight_id[[i]])) {
    weight_row <- match(plan$weight_id[[i]], registry$var_id)
    if (is.na(weight_row)) {
      issues <- c(issues, "The correlation weight is absent.")
    } else {
      plan$weight[[i]] <- registry$name[[weight_row]]
      weight <- analysis_vector(analysis_data[[plan$weight[[i]]]])
      if (!is.numeric(weight) || any(!is.na(weight) & (!is.finite(weight) | weight <= 0))) {
        issues <- c(issues, "Correlation weights must be finite and positive.")
      }
    }
  }
  if (!is.na(plan$correlation_subject_id[[i]])) {
    subject_row <- match(plan$correlation_subject_id[[i]], registry$var_id)
    if (is.na(subject_row)) {
      issues <- c(issues, "The correlation subject identifier is absent.")
    } else {
      plan$correlation_subject[[i]] <- registry$name[[subject_row]]
    }
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
      method_object <- plan$method_object[[i]]
      base_method_id <- if (is.null(method_object$base_method)) {
        method_object$id
      } else {
        method_object$base_method$id
      }
      minimum <- if (base_method_id %in% c("pearson", "spearman")) 4L else 3L
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
      if (!is.na(plan$correlation_subject_id[[i]]) && n > 0L) {
        subject <- analysis_vector(
          analysis_data[[plan$correlation_subject[[i]]]]
        )[mask]
        counts <- table(subject)
        if (length(counts) < 2L || sum(counts >= 2L) < 2L) {
          issues <- c(issues,
            "Repeated-measures correlation requires at least two subjects with repeated observations.")
        }
      }
    }
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  } else if (!isTRUE(plan$method_object[[i]]$provides_inference) &&
             !isTRUE(plan$approved[[i]])) {
    plan$status[[i]] <- "review"
    plan$reason[[i]] <- paste0(
      "Correlation method `", plan$method[[i]], "` reports a point estimate ",
      "without confidence intervals or p-values. Approve the task with ",
      "`approve_plan()` or wrap the method in `resampled_correlation()`."
    )
  }
  plan
}

correlation_analysis_vector <- function(data, spec, name, id) {
  tryCatch({
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    input_type <- spec$method_object[[1]]$input_type
    if (input_type %in% c("ordered_categorical", "binary_categorical")) {
      if (!is.factor(original) && !is.numeric(value) && !is.character(value)) {
        stop_invalid_correlation(paste0(
          "Correlation variable `", name, "` must be ordered categorical."
        ))
      }
      value <- ordered(value)
      if (input_type == "binary_categorical" && nlevels(value) != 2L) {
        stop_invalid_correlation(paste0(
          "Tetrachoric variable `", name, "` must have two levels."
        ))
      }
      if (!is.null(spec$transformation_specs[[1]][[id]])) {
        stop_invalid_correlation("Categorical correlation inputs cannot be transformed.")
      }
      return(value)
    }
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
      spec$adjustment_ids[[1]], spec$weight_id[[1]],
      spec$correlation_subject_id[[1]]
    ))
  } else ids <- spec$correlation_variable_ids[[1]]
  registry <- variables(data)
  mask <- rep(TRUE, nrow(data))
  for (id in ids) {
    if (is.na(id)) next
    row <- match(id, registry$var_id)
    if (is.na(row)) return(rep(FALSE, nrow(data)))
    if (id %in% c(spec$weight_id[[1]], spec$correlation_subject_id[[1]])) {
      mask <- mask & !special_missing_mask(data[[registry$name[[row]]]])
      next
    }
    value <- correlation_analysis_vector(
      data, spec, registry$name[[row]], id
    )
    if (inherits(value, "error")) return(rep(FALSE, nrow(data)))
    mask <- mask & !is.na(value)
  }
  mask
}
