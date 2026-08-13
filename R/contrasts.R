#' Access registered or computed contrasts
#' @param x A `bq_data` or `analysis_result`.
#' @return A tidy tibble.
#' @export
contrasts <- function(x) UseMethod("contrasts")

#' @export
contrasts.bq_data <- function(x) {
  check_bq_data(x)
  tibble::as_tibble(attr(x, "contrast_registry", exact = TRUE))
}

#' Configure model coding
#' @param .data A `bq_data` object.
#' @param .cols Categorical predictors selected with tidyselect.
#' @param coding Currently only treatment coding is supported.
#' @param reference Reference value.
#' @return Updated `bq_data`.
#' @export
set_coding <- function(.data, .cols, coding = "treatment", reference) {
  check_bq_data(.data)
  if (!identical(coding, "treatment")) {
    stop_comparison("Only `treatment` coding is supported.", "bq_error_invalid_coding")
  }
  check_scalar_setting(reference, "reference", "bq_error_invalid_coding")
  selected <- names(tidyselect::eval_select(rlang::enquo(.cols), .data))
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected, registry$name)
  if (any(!registry$type[rows] %in% c("binary", "ordinal", "nominal"))) {
    stop_comparison("Coding requires categorical variables.", "bq_error_invalid_coding")
  }
  registry$coding[rows] <- coding
  for (row in rows) registry$reference[[row]] <- reference
  attr(.data, "variable_registry") <- registry
  .data
}

#' Compare levels against a reference
#' @param reference Reference value used as denominator.
#' @return A backend-independent `contrast_spec`.
#' @export
against_reference <- function(reference) {
  check_scalar_setting(reference, "reference", "bq_error_invalid_comparison")
  new_contrast_spec("against_reference", reference = reference)
}

#' Compare all pairs of levels
#' @return A backend-independent `contrast_spec`.
#' @export
all_pairwise <- function() {
  new_contrast_spec("all_pairwise")
}

#' Compare consecutive ordered levels
#'
#' Each level is compared with the level immediately preceding it in the
#' factor order (or observed order for non-factors).
#' @return A backend-independent `contrast_spec`.
#' @export
consecutive_comparisons <- function() {
  new_contrast_spec("consecutive")
}

#' Compare each level with the global mean
#'
#' @param exponentiate Whether to exponentiate the model-scale contrast. This
#'   argument is required so ratio-scale interpretation is never implicit.
#' @param weights How levels contribute to the global mean: equally or in
#'   proportion to their observed analysis counts.
#' @return A backend-independent `contrast_spec`.
#' @export
against_global_mean <- function(exponentiate, weights = c("equal", "observed")) {
  if (missing(exponentiate) || !is.logical(exponentiate) ||
      length(exponentiate) != 1L || is.na(exponentiate)) {
    stop_comparison(
      "`exponentiate` must be explicitly supplied as TRUE or FALSE.",
      "bq_error_invalid_comparison"
    )
  }
  weights <- match.arg(weights)
  new_contrast_spec(
    "against_global_mean", weights = weights, exponentiate = exponentiate
  )
}

#' Construct a custom comparison function
#' @param id Stable function identifier.
#' @param compute Function receiving `(model, context)` and returning a
#'   normalized contrasts data frame.
#' @param effect_measure Declared effect measure.
#' @param scale Final output scale.
#' @param model_scale Scale returned by `compute` before normalization.
#' @param exponentiate Whether normalization exponentiates estimate and limits.
#' @param required_packages Required packages checked during preflight.
#' @return A backend-independent `contrast_spec`.
#' @export
comparison_function <- function(id, compute, effect_measure, scale,
                                model_scale = scale, exponentiate = FALSE,
                                required_packages = character()) {
  check_contract_id(id)
  check_contract_id(effect_measure, "effect_measure")
  check_contract_id(scale, "scale")
  check_contract_id(model_scale, "model_scale")
  check_exponentiate(exponentiate)
  if (!is.function(compute)) {
    stop_comparison("`compute` must be a function.", "bq_error_invalid_comparison")
  }
  new_contrast_spec(
    "custom_function", compute = compute, effect_measure = effect_measure,
    scale = scale, model_scale = model_scale, exponentiate = exponentiate,
    required_packages = required_packages, function_id = id,
    function_hash = digest::digest(compute)
  )
}

new_contrast_spec <- function(type, reference = NULL, compute = NULL,
                              effect_measure = NA_character_, scale = NA_character_,
                              model_scale = scale, exponentiate = FALSE,
                              required_packages = character(), function_id = NA_character_,
                              function_hash = NA_character_, modifier = NULL,
                              weights = NULL) {
  structure(list(
    type = type, reference = reference, compute = compute,
    effect_measure = effect_measure, scale = scale, model_scale = model_scale,
    exponentiate = exponentiate, required_packages = required_packages,
    function_id = function_id, function_hash = function_hash,
    modifier = modifier, weights = weights
  ), class = "contrast_spec")
}

#' Estimate predictor effects within modifier levels
#' @param modifier Exactly one effect-modifier column selected when the spec is
#'   registered by [set_comparisons()].
#' @return A conditional-effect `contrast_spec`.
#' @export
within_levels <- function(modifier) {
  structure(list(
    type = "within_levels", modifier_quo = rlang::enquo(modifier),
    reference = NULL, compute = NULL, effect_measure = NA_character_,
    scale = NA_character_, model_scale = NA_character_, exponentiate = FALSE,
    required_packages = character(), function_id = NA_character_,
    function_hash = NA_character_
  ), class = "contrast_spec")
}

#' Compare conditional effects across modifier levels
#'
#' @param modifier Exactly one categorical effect modifier.
#' @param inner A contrast specification for levels of the main predictor.
#' @param outer A contrast specification for levels of `modifier`.
#' @param exponentiate Whether to exponentiate the model-scale contrast of
#'   contrasts. This must always be supplied explicitly.
#' @return A backend-independent contrast-of-contrasts specification.
#' @export
contrast_of_contrasts <- function(modifier, inner, outer, exponentiate) {
  if (missing(exponentiate) || !is.logical(exponentiate) ||
      length(exponentiate) != 1L || is.na(exponentiate)) {
    stop_comparison(
      "`exponentiate` must be explicitly supplied as TRUE or FALSE.",
      "bq_error_invalid_comparison"
    )
  }
  supported <- c(
    "against_reference", "all_pairwise", "consecutive", "against_global_mean"
  )
  if (!inherits(inner, "contrast_spec") || !inner$type %in% supported ||
      !inherits(outer, "contrast_spec") || !outer$type %in% supported) {
    stop_comparison(
      "`inner` and `outer` must be supported level contrast specifications.",
      "bq_error_invalid_comparison"
    )
  }
  structure(list(
    type = "contrast_of_contrasts", modifier_quo = rlang::enquo(modifier),
    inner = inner, outer = outer, exponentiate = exponentiate,
    reference = NULL, compute = NULL, effect_measure = NA_character_,
    scale = NA_character_, model_scale = NA_character_,
    required_packages = character(), function_id = NA_character_,
    function_hash = NA_character_
  ), class = "contrast_spec")
}

#' Register target comparisons
#' @param .data A `bq_data` object.
#' @param .cols Predictor columns selected with tidyselect.
#' @param comparisons A `contrast_spec`.
#' @param adjust A method accepted by [stats::p.adjust()].
#' @return Updated `bq_data`.
#' @export
set_comparisons <- function(.data, .cols, comparisons, adjust = "none") {
  check_bq_data(.data)
  if (!inherits(comparisons, "contrast_spec")) {
    stop_comparison("`comparisons` must be a contrast_spec.", "bq_error_invalid_comparison")
  }
  if (!is.character(adjust) || length(adjust) != 1L || !adjust %in% stats::p.adjust.methods) {
    stop_comparison("`adjust` is not supported.", "bq_error_invalid_comparison")
  }
  selected <- names(tidyselect::eval_select(rlang::enquo(.cols), .data))
  registry <- variables(.data)
  rows <- match(selected, registry$name)
  if (comparisons$type %in% c(
      "against_reference", "all_pairwise", "consecutive", "against_global_mean"
    ) &&
      any(!registry$type[rows] %in% c("binary", "ordinal", "nominal"))) {
    stop_comparison(
      "Target level comparisons require categorical predictors.",
      "bq_error_invalid_comparison"
    )
  }
  modifier_id <- modifier_name <- NA_character_
  if (comparisons$type %in% c("within_levels", "contrast_of_contrasts")) {
    modifier_selected <- names(tidyselect::eval_select(comparisons$modifier_quo, .data))
    if (length(modifier_selected) != 1L) {
      stop_comparison("The comparison requires exactly one modifier.",
        "bq_error_invalid_comparison")
    }
    modifier_name <- modifier_selected[[1]]
    modifier_id <- registry$var_id[match(modifier_name, registry$name)]
  }
  additions <- lapply(rows, function(row) tibble::tibble(
    contrast_id = paste0("contrast_", uuid::UUIDgenerate()),
    predictor_id = registry$var_id[[row]], predictor = registry$name[[row]],
    comparison_type = comparisons$type, reference = list(comparisons$reference),
    adjust_method = adjust, function_id = comparisons$function_id,
    function_hash = comparisons$function_hash,
    required_packages = list(comparisons$required_packages),
    comparison_object = list(comparisons), modifier_id = modifier_id,
    modifier = modifier_name
  ))
  current <- attr(.data, "contrast_registry", exact = TRUE)
  attr(.data, "contrast_registry") <- vctrs::vec_rbind(current, !!!additions)
  .data
}

stop_comparison <- function(message, class) {
  stop(structure(list(message = message, call = sys.call(-1L)), class = c(class, "error", "condition")))
}

contrast_level_pairs <- function(levels, comparison_type, reference = NULL) {
  levels <- as.character(levels)
  if (comparison_type == "against_reference") {
    reference <- as.character(reference)
    return(tibble::tibble(
      numerator = setdiff(levels, reference), denominator = reference
    ))
  }
  if (comparison_type == "consecutive") {
    return(tibble::tibble(
      numerator = levels[-1L], denominator = levels[-length(levels)]
    ))
  }
  if (comparison_type == "all_pairwise") {
    pairs <- utils::combn(levels, 2L)
    return(tibble::tibble(numerator = pairs[2, ], denominator = pairs[1, ]))
  }
  if (comparison_type == "against_global_mean") {
    return(tibble::tibble(
      numerator = levels, denominator = rep(".global_mean", length(levels))
    ))
  }
  tibble::tibble(numerator = character(), denominator = character())
}

compute_builtin_contrasts <- function(fit, spec, data) {
  registry <- contrasts(data)
  if (nrow(registry) == 0L) return(contrasts_prototype())
  requested <- spec$contrast_ids[[1]]
  if (length(requested) == 0L) return(contrasts_prototype())
  registered <- registry[
    registry$contrast_id %in% requested & registry$comparison_type %in%
      c("against_reference", "all_pairwise", "consecutive", "against_global_mean"),
    , drop = FALSE
  ]
  if (nrow(registered) == 0L) return(contrasts_prototype())
  model_frame <- stats::model.frame(fit)
  predictor <- model_frame[[spec$predictor[[1]]]]
  if (!is.factor(predictor)) return(contrasts_prototype())
  covariance <- covariance_for_contrasts(fit, spec)
  beta <- stats::coef(fit)
  base <- model_frame[1, -1L, drop = FALSE]
  for (name in names(base)) {
    column <- model_frame[[name]]
    if (is.factor(column)) {
      base[[name]] <- factor(levels(column)[[1]], levels = levels(column))
    } else if (is.numeric(column)) {
      base[[name]] <- mean(column, na.rm = TRUE)
    }
  }
  terms_object <- stats::delete.response(stats::terms(fit))
  outputs <- lapply(seq_len(nrow(registered)), function(i) {
    comparison <- registered[i, , drop = FALSE]
    pairs <- contrast_level_pairs(
      levels(predictor), comparison$comparison_type[[1]],
      comparison$reference[[1]]
    )
    level_weights <- if (comparison$comparison_type[[1]] == "against_global_mean" &&
      identical(comparison$comparison_object[[1]]$weights, "observed")) {
      as.numeric(table(predictor)[levels(predictor)]) / length(predictor)
    } else {
      rep(1 / length(levels(predictor)), length(levels(predictor)))
    }
    level_matrices <- lapply(levels(predictor), function(level) {
      level_data <- base
      level_data[[spec$predictor[[1]]]] <- factor(level, levels = levels(predictor))
      stats::model.matrix(terms_object, level_data, contrasts.arg = fit$contrasts)
    })
    global_matrix <- Reduce(`+`, Map(`*`, level_matrices, level_weights))
    rows <- lapply(seq_len(nrow(pairs)), function(j) {
      numerator_data <- denominator_data <- base
      numerator_data[[spec$predictor[[1]]]] <- factor(
        pairs$numerator[[j]], levels = levels(predictor)
      )
      if (pairs$denominator[[j]] != ".global_mean") {
        denominator_data[[spec$predictor[[1]]]] <- factor(
          pairs$denominator[[j]], levels = levels(predictor)
        )
      }
      numerator_matrix <- stats::model.matrix(
        terms_object, numerator_data, contrasts.arg = fit$contrasts
      )
      denominator_matrix <- if (pairs$denominator[[j]] == ".global_mean") {
        global_matrix
      } else stats::model.matrix(
        terms_object, denominator_data, contrasts.arg = fit$contrasts
      )
      vector <- as.numeric(numerator_matrix - denominator_matrix)
      names(vector) <- colnames(numerator_matrix)
      vector <- vector[names(beta)]
      estimate <- sum(vector * beta)
      standard_error <- sqrt(drop(t(vector) %*% covariance %*% vector))
      statistic <- estimate / standard_error
      if (inherits(fit, "lm") && !inherits(fit, "glm")) {
        critical <- stats::qt((1 + spec$confidence_level[[1]]) / 2, stats::df.residual(fit))
        p_value <- 2 * stats::pt(abs(statistic), stats::df.residual(fit), lower.tail = FALSE)
      } else {
        critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
        p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
      }
      conf_low <- estimate - critical * standard_error
      conf_high <- estimate + critical * standard_error
      contrast_exponentiate <- if (
        comparison$comparison_type[[1]] == "against_global_mean"
      ) isTRUE(comparison$comparison_object[[1]]$exponentiate) else
        isTRUE(spec$exponentiate[[1]])
      if (contrast_exponentiate) {
        estimate <- exp(estimate); conf_low <- exp(conf_low); conf_high <- exp(conf_high)
      }
      output_effect_measure <- if (!contrast_exponentiate &&
          isTRUE(spec$exponentiate[[1]])) {
        "linear_predictor_difference"
      } else spec$effect_measure[[1]]
      output_scale <- if (!contrast_exponentiate &&
          isTRUE(spec$exponentiate[[1]])) {
        spec$model_scale[[1]]
      } else spec$scale[[1]]
      tibble::tibble(
        analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
        predictor = spec$predictor[[1]], contrast_id = comparison$contrast_id[[1]],
        contrast = paste0(pairs$numerator[[j]], " - ", pairs$denominator[[j]]),
        numerator = pairs$numerator[[j]], denominator = pairs$denominator[[j]],
        modifier = NA_character_, modifier_level = NA_character_,
        estimate = estimate, std_error = standard_error,
        std_error_scale = spec$model_scale[[1]],
        conf_low = conf_low, conf_high = conf_high,
        p_value = p_value, p_adjusted = NA_real_,
        adjust_method = comparison$adjust_method[[1]],
        effect_measure = output_effect_measure, scale = output_scale
      )
    })
    result <- vctrs::vec_rbind(!!!rows)
    result$p_adjusted <- stats::p.adjust(
      result$p_value, method = comparison$adjust_method[[1]]
    )
    result
  })
  vctrs::vec_rbind(!!!outputs)
}

compute_conditional_contrasts <- function(fit, spec, data) {
  registry <- contrasts(data)
  if (nrow(registry) == 0L) return(contrasts_prototype())
  registered <- registry[
    registry$contrast_id %in% spec$contrast_ids[[1]] &
      registry$comparison_type == "within_levels", , drop = FALSE
  ]
  if (nrow(registered) == 0L) return(contrasts_prototype())
  model_frame <- stats::model.frame(fit)
  predictor <- model_frame[[spec$predictor[[1]]]]
  if (!is.factor(predictor)) {
    stop_comparison_output("`within_levels()` currently requires a categorical predictor.")
  }
  covariance <- covariance_for_contrasts(fit, spec)
  beta <- stats::coef(fit)
  outputs <- lapply(seq_len(nrow(registered)), function(i) {
    comparison <- registered[i, , drop = FALSE]
    modifier_name <- comparison$modifier[[1]]
    modifier <- model_frame[[modifier_name]]
    levels_modifier <- if (is.factor(modifier)) levels(modifier) else sort(unique(modifier))
    reference <- levels(predictor)[[1]]
    numerators <- levels(predictor)[-1L]
    rows <- list()
    for (modifier_level in levels_modifier) {
      for (numerator in numerators) {
        base <- model_frame[1, , drop = FALSE]
        for (name in names(base)) {
          column <- model_frame[[name]]
          if (is.factor(column)) {
            base[[name]] <- factor(levels(column)[[1]], levels = levels(column))
          } else if (is.numeric(column)) {
            base[[name]] <- mean(column, na.rm = TRUE)
          }
        }
        base[[modifier_name]] <- if (is.factor(modifier)) {
          factor(modifier_level, levels = levels(modifier))
        } else modifier_level
        numerator_data <- denominator_data <- base
        numerator_data[[spec$predictor[[1]]]] <- factor(numerator, levels = levels(predictor))
        denominator_data[[spec$predictor[[1]]]] <- factor(reference, levels = levels(predictor))
        terms_object <- stats::delete.response(stats::terms(fit))
        numerator_matrix <- stats::model.matrix(terms_object, numerator_data,
          contrasts.arg = fit$contrasts)
        denominator_matrix <- stats::model.matrix(terms_object, denominator_data,
          contrasts.arg = fit$contrasts)
        contrast_vector <- as.numeric(numerator_matrix - denominator_matrix)
        names(contrast_vector) <- colnames(numerator_matrix)
        contrast_vector <- contrast_vector[names(beta)]
        estimate <- sum(contrast_vector * beta)
        std_error <- sqrt(drop(t(contrast_vector) %*% covariance %*% contrast_vector))
        statistic <- estimate / std_error
        if (inherits(fit, "glm") || inherits(fit, "coxph")) {
          critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
          p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
        } else {
          critical <- stats::qt((1 + spec$confidence_level[[1]]) / 2,
            stats::df.residual(fit))
          p_value <- 2 * stats::pt(abs(statistic), stats::df.residual(fit),
            lower.tail = FALSE)
        }
        conf_low <- estimate - critical * std_error
        conf_high <- estimate + critical * std_error
        if (isTRUE(spec$exponentiate[[1]])) {
          estimate <- exp(estimate); conf_low <- exp(conf_low); conf_high <- exp(conf_high)
        }
        rows[[length(rows) + 1L]] <- tibble::tibble(
          analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
          predictor = spec$predictor[[1]], contrast_id = comparison$contrast_id[[1]],
          contrast = paste0(numerator, " - ", reference, " | ", modifier_name, "=", modifier_level),
          numerator = numerator, denominator = reference,
          modifier = modifier_name, modifier_level = as.character(modifier_level),
          estimate = estimate, std_error = std_error,
          std_error_scale = spec$model_scale[[1]],
          conf_low = conf_low, conf_high = conf_high,
          p_value = p_value, p_adjusted = NA_real_,
          adjust_method = comparison$adjust_method[[1]],
          effect_measure = spec$effect_measure[[1]], scale = spec$scale[[1]]
        )
      }
    }
    result <- vctrs::vec_rbind(!!!rows)
    result$p_adjusted <- stats::p.adjust(result$p_value,
      method = comparison$adjust_method[[1]])
    result
  })
  vctrs::vec_rbind(!!!outputs)
}

compute_contrasts_of_contrasts <- function(fit, spec, data) {
  registry <- contrasts(data)
  if (nrow(registry) == 0L) return(contrasts_prototype())
  registered <- registry[
    registry$contrast_id %in% spec$contrast_ids[[1]] &
      registry$comparison_type == "contrast_of_contrasts", , drop = FALSE
  ]
  if (nrow(registered) == 0L) return(contrasts_prototype())
  model_frame <- stats::model.frame(fit)
  predictor_name <- spec$predictor[[1]]
  predictor <- model_frame[[predictor_name]]
  if (!is.factor(predictor)) {
    stop_comparison_output(
      "`contrast_of_contrasts()` requires a categorical predictor."
    )
  }
  covariance <- covariance_for_contrasts(fit, spec)
  beta <- stats::coef(fit)
  terms_object <- stats::delete.response(stats::terms(fit))
  outputs <- lapply(seq_len(nrow(registered)), function(i) {
    comparison <- registered[i, , drop = FALSE]
    comparison_spec <- comparison$comparison_object[[1]]
    modifier_name <- comparison$modifier[[1]]
    modifier <- model_frame[[modifier_name]]
    if (!is.factor(modifier)) {
      stop_comparison_output(
        "`contrast_of_contrasts()` requires a categorical modifier."
      )
    }
    inner <- level_contrast_vectors(
      levels(predictor), comparison_spec$inner, predictor
    )
    outer <- level_contrast_vectors(
      levels(modifier), comparison_spec$outer, modifier
    )
    base <- model_frame[1, -1L, drop = FALSE]
    for (name in names(base)) {
      column <- model_frame[[name]]
      if (is.factor(column)) {
        base[[name]] <- factor(levels(column)[[1]], levels = levels(column))
      } else if (is.numeric(column)) {
        base[[name]] <- mean(column, na.rm = TRUE)
      }
    }
    cell_matrices <- lapply(levels(modifier), function(modifier_level) {
      lapply(levels(predictor), function(predictor_level) {
        cell <- base
        cell[[predictor_name]] <- factor(
          predictor_level, levels = levels(predictor)
        )
        cell[[modifier_name]] <- factor(
          modifier_level, levels = levels(modifier)
        )
        stats::model.matrix(
          terms_object, cell, contrasts.arg = fit$contrasts
        )
      })
    })
    rows <- list()
    for (inner_i in seq_len(nrow(inner))) {
      for (outer_i in seq_len(nrow(outer))) {
        vector <- rep(0, length(beta)); names(vector) <- names(beta)
        for (modifier_i in seq_along(levels(modifier))) {
          for (predictor_i in seq_along(levels(predictor))) {
            coefficient <- outer$weights[[outer_i]][[modifier_i]] *
              inner$weights[[inner_i]][[predictor_i]]
            matrix_row <- cell_matrices[[modifier_i]][[predictor_i]]
            vector <- vector + coefficient * as.numeric(matrix_row[, names(beta)])
          }
        }
        estimate <- sum(vector * beta)
        standard_error <- sqrt(drop(t(vector) %*% covariance %*% vector))
        statistic <- estimate / standard_error
        if (inherits(fit, "glm")) {
          critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
          p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
        } else {
          critical <- stats::qt(
            (1 + spec$confidence_level[[1]]) / 2, stats::df.residual(fit)
          )
          p_value <- 2 * stats::pt(
            abs(statistic), stats::df.residual(fit), lower.tail = FALSE
          )
        }
        conf_low <- estimate - critical * standard_error
        conf_high <- estimate + critical * standard_error
        exponentiate <- isTRUE(comparison_spec$exponentiate)
        effect_measure <- if (inherits(fit, "glm")) {
          if (exponentiate) "ratio_of_odds_ratios" else "log_ratio_of_odds_ratios"
        } else if (inherits(fit, "coxph")) {
          if (exponentiate) "ratio_of_hazard_ratios" else "log_ratio_of_hazard_ratios"
        } else "difference_of_differences"
        output_scale <- if (exponentiate) "ratio" else spec$model_scale[[1]]
        if (exponentiate) {
          estimate <- exp(estimate)
          conf_low <- exp(conf_low)
          conf_high <- exp(conf_high)
        }
        rows[[length(rows) + 1L]] <- tibble::tibble(
          analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
          predictor = predictor_name, contrast_id = comparison$contrast_id[[1]],
          contrast = paste0("(", inner$label[[inner_i]], ") | ",
            outer$label[[outer_i]]),
          numerator = inner$numerator[[inner_i]],
          denominator = inner$denominator[[inner_i]],
          modifier = modifier_name, modifier_level = NA_character_,
          inner_contrast = inner$label[[inner_i]],
          outer_contrast = outer$label[[outer_i]],
          estimand = "difference_of_differences", exponentiated = exponentiate,
          estimate = estimate, std_error = standard_error,
          std_error_scale = spec$model_scale[[1]],
          conf_low = conf_low, conf_high = conf_high,
          p_value = p_value, p_adjusted = NA_real_,
          adjust_method = comparison$adjust_method[[1]],
          effect_measure = effect_measure, scale = output_scale
        )
      }
    }
    result <- vctrs::vec_rbind(!!!rows)
    result$p_adjusted <- stats::p.adjust(
      result$p_value, method = comparison$adjust_method[[1]]
    )
    result
  })
  vctrs::vec_rbind(!!!outputs)
}

level_contrast_vectors <- function(levels, contrast_spec, observed) {
  pairs <- contrast_level_pairs(
    levels, contrast_spec$type, contrast_spec$reference
  )
  level_weights <- if (contrast_spec$type == "against_global_mean" &&
      identical(contrast_spec$weights, "observed")) {
    as.numeric(table(observed)[levels]) / length(observed)
  } else rep(1 / length(levels), length(levels))
  weights <- lapply(seq_len(nrow(pairs)), function(i) {
    vector <- stats::setNames(rep(0, length(levels)), levels)
    vector[[pairs$numerator[[i]]]] <- 1
    if (pairs$denominator[[i]] == ".global_mean") {
      vector <- vector - level_weights
    } else {
      vector[[pairs$denominator[[i]]]] <-
        vector[[pairs$denominator[[i]]]] - 1
    }
    unname(vector)
  })
  tibble::tibble(
    numerator = pairs$numerator, denominator = pairs$denominator,
    label = paste0(pairs$numerator, " - ", pairs$denominator), weights = weights
  )
}

covariance_for_contrasts <- function(fit, spec) {
  if (spec$variance[[1]] == "robust") return(sandwich::vcovHC(fit, type = "HC0"))
  if (spec$variance[[1]] == "cluster_robust") {
    model_rows <- as.integer(rownames(stats::model.frame(fit)))
    clusters <- attr(fit, "bq_clusters")
    return(sandwich::vcovCL(fit, cluster = clusters[model_rows],
      type = "HC1", cadjust = TRUE))
  }
  stats::vcov(fit)
}
