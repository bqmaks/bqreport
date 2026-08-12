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
                              function_hash = NA_character_, modifier = NULL) {
  structure(list(
    type = type, reference = reference, compute = compute,
    effect_measure = effect_measure, scale = scale, model_scale = model_scale,
    exponentiate = exponentiate, required_packages = required_packages,
    function_id = function_id, function_hash = function_hash, modifier = modifier
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
  modifier_id <- modifier_name <- NA_character_
  if (identical(comparisons$type, "within_levels")) {
    modifier_selected <- names(tidyselect::eval_select(comparisons$modifier_quo, .data))
    if (length(modifier_selected) != 1L) {
      stop_comparison("`within_levels()` requires exactly one modifier.",
        "bq_error_invalid_comparison")
    }
    modifier_name <- modifier_selected[[1]]
    modifier_id <- registry$var_id[match(modifier_name, registry$name)]
  }
  rows <- match(selected, registry$name)
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

compute_builtin_contrasts <- function(estimate_data, spec, data) {
  registry <- contrasts(data)
  if (nrow(registry) == 0L) return(contrasts_prototype())
  requested <- spec$contrast_ids[[1]]
  if (length(requested) == 0L) return(contrasts_prototype())
  registered <- registry[
    registry$contrast_id %in% requested & registry$comparison_type == "against_reference",
    , drop = FALSE
  ]
  if (nrow(registered) == 0L) return(contrasts_prototype())
  rows <- estimate_data$term == spec$predictor[[1]] & !is.na(estimate_data$level)
  if (!any(rows)) return(contrasts_prototype())
  outputs <- lapply(seq_len(nrow(registered)), function(i) {
    comparison <- registered[i, , drop = FALSE]
    reference <- as.character(comparison$reference[[1]])
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], contrast_id = comparison$contrast_id[[1]],
      contrast = paste0(estimate_data$level[rows], " - ", reference),
      numerator = estimate_data$level[rows], denominator = reference,
      modifier = NA_character_, modifier_level = NA_character_,
      estimate = estimate_data$estimate[rows], conf_low = estimate_data$conf_low[rows],
      conf_high = estimate_data$conf_high[rows], p_value = estimate_data$p_value[rows],
      p_adjusted = stats::p.adjust(estimate_data$p_value[rows], method = comparison$adjust_method[[1]]),
      adjust_method = comparison$adjust_method[[1]],
      effect_measure = estimate_data$effect_measure[rows], scale = estimate_data$scale[rows]
    )
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
        if (inherits(fit, "glm")) {
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
          estimate = estimate, conf_low = conf_low, conf_high = conf_high,
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
