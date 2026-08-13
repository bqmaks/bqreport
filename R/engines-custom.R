# Custom engines, method chains, and user-contract validation.

compute_custom_comparisons <- function(model, spec, frame, data) {
  registry <- contrasts(data)
  if (nrow(registry) == 0L) return(contrasts_prototype())
  rows <- registry$contrast_id %in% spec$contrast_ids[[1]] &
    registry$comparison_type == "custom_function"
  registered <- registry[rows, , drop = FALSE]
  if (nrow(registered) == 0L) return(contrasts_prototype())
  outputs <- lapply(seq_len(nrow(registered)), function(i) {
    comparison_row <- registered[i, , drop = FALSE]
    comparison <- comparison_row$comparison_object[[1]]
    missing_packages <- comparison$required_packages[
      !vapply(comparison$required_packages, requireNamespace, logical(1), quietly = TRUE)
    ]
    if (length(missing_packages)) {
      stop_comparison_output(paste0(
        "Missing packages for comparison `", comparison$function_id, "`: ",
        paste(missing_packages, collapse = ", "), "."
      ))
    }
    context <- build_analysis_context(spec, frame, data)
    context$comparison_spec <- comparison_row
    output <- comparison$compute(model, context)
    output <- validate_comparison_output(output, spec, comparison)
    output$contrast_id <- comparison_row$contrast_id[[1]]
    output$adjust_method <- comparison_row$adjust_method[[1]]
    output$p_adjusted <- stats::p.adjust(
      output$p_value, method = comparison_row$adjust_method[[1]]
    )
    if (isTRUE(comparison$exponentiate)) {
      output$estimate <- exp(output$estimate)
      output$conf_low <- exp(output$conf_low)
      output$conf_high <- exp(output$conf_high)
      output$scale <- comparison$scale
    }
    output
  })
  vctrs::vec_rbind(!!!outputs)
}

validate_comparison_output <- function(x, spec, comparison) {
  if (!inherits(x, "data.frame")) {
    stop_comparison_output("Custom comparison must return a data frame.")
  }
  if (!"modifier" %in% names(x)) x$modifier <- rep(NA_character_, nrow(x))
  if (!"modifier_level" %in% names(x)) x$modifier_level <- rep(NA_character_, nrow(x))
  if (!"std_error" %in% names(x)) x$std_error <- rep(NA_real_, nrow(x))
  if (!"std_error_scale" %in% names(x)) {
    x$std_error_scale <- rep(comparison$model_scale, nrow(x))
  }
  x <- add_optional_interaction_contrast_columns(x)
  prototype <- contrasts_prototype()
  missing <- setdiff(names(prototype), names(x))
  if (length(missing)) {
    stop_comparison_output(paste0(
      "Custom comparison output is missing columns: ",
      paste(missing, collapse = ", "), "."
    ))
  }
  x <- tibble::as_tibble(x)[names(prototype)]
  if (nrow(x) && any(x$analysis_id != spec$analysis_id[[1]])) {
    stop_comparison_output("Custom comparison returned an incorrect `analysis_id`.")
  }
  expected_scale <- if (isTRUE(comparison$exponentiate)) {
    comparison$model_scale
  } else {
    comparison$scale
  }
  if (nrow(x) && (
    any(x$effect_measure != comparison$effect_measure) ||
      any(x$scale != expected_scale)
  )) {
    stop_comparison_output("Custom comparison violates effect measure or scale.")
  }
  tryCatch(vctrs::vec_cast(x, prototype), error = function(error) {
    stop_comparison_output("Custom comparison output has incompatible column types.")
  })
}

stop_comparison_output <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_comparison_output", "error", "condition")
  ))
}

execute_method_chain <- function(spec, frame, data) {
  chain <- spec$method_object[[1]]
  rows <- list()
  non_advanceable <- c(
    "bq_error_invalid_engine_output",
    "bq_error_invalid_method_contract"
  )
  last_condition <- NULL
  for (i in seq_along(chain$methods)) {
    method_name <- names(chain$methods)[[i]]
    method <- chain$methods[[i]]
    member_spec <- spec
    member_spec$method[[1]] <- method$method
    member_spec$engine[[1]] <- method$engine
    member_spec$estimator[[1]] <- method$estimator
    member_spec$ci_method[[1]] <- method$ci_method
    member_spec$family[[1]] <- method$family
    member_spec$link[[1]] <- method$link
    member_spec$effect_measure[[1]] <- method$effect_measure
    member_spec$model_scale[[1]] <- method$model_scale
    member_spec$scale[[1]] <- method$scale
    member_spec$exponentiate[[1]] <- method$exponentiate
    member_spec$function_id[[1]] <- method$function_id
    member_spec$function_hash[[1]] <- method$function_hash
    member_spec$required_packages[[1]] <- method$required_packages
    member_spec$method_object[[1]] <- method
    output <- tryCatch(
      execute_custom_method(member_spec, frame, data),
      error = function(condition) condition
    )
    if (!inherits(output, "error")) {
      rows[[length(rows) + 1L]] <- attempt_row(
        spec$analysis_id[[1]], chain$method, i, method_name, method$method,
        "success", NA_character_, NA_character_
      )
      return(list(
        success = TRUE, output = output,
        attempts = bind_component(rows, attempts_prototype()),
        method = method$method, spec = member_spec
      ))
    }
    last_condition <- output
    condition_class <- class(output)[[1]]
    rows[[length(rows) + 1L]] <- attempt_row(
      spec$analysis_id[[1]], chain$method, i, method_name, method$method,
      "failed", condition_class, conditionMessage(output)
    )
    contract_failure <- any(inherits(output, non_advanceable))
    allowed <- any(vapply(chain$advance_on, function(x) inherits(output, x), logical(1)))
    if (contract_failure || !allowed || i == length(chain$methods)) break
  }
  list(
    success = FALSE, output = last_condition,
    attempts = bind_component(rows, attempts_prototype()),
    method = NA_character_, spec = spec
  )
}

attempt_row <- function(analysis_id, chain_id, attempt, member, method,
                        status, condition_class, message) {
  tibble::tibble(
    analysis_id = analysis_id, chain_id = chain_id,
    attempt = as.integer(attempt), member = member, method = method,
    status = status, condition_class = condition_class, message = message
  )
}

execute_custom_method <- function(spec, frame, data) {
  method <- spec$method_object[[1]]
  missing_packages <- method$required_packages[
    !vapply(method$required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    stop_method_output(paste0("Missing required packages: ", paste(missing_packages, collapse = ", "), "."))
  }
  context <- build_analysis_context(spec, frame, data)
  output <- method$run(context)
  if (!inherits(output, "analysis_output")) {
    stop_method_output("Custom method must return `analysis_output()`.")
  }
  output$estimates <- validate_custom_component(output$estimates, estimates_prototype(),
    "estimates", spec, method, required = TRUE)
  if (isTRUE(method$exponentiate) && nrow(output$estimates)) {
    output$estimates$estimate <- exp(output$estimates$estimate)
    output$estimates$conf_low <- exp(output$estimates$conf_low)
    output$estimates$conf_high <- exp(output$estimates$conf_high)
    output$estimates$scale <- method$scale
  }
  output$tests <- validate_custom_component(output$tests, tests_prototype(), "tests", spec, method)
  output$contrasts <- validate_custom_component(output$contrasts, contrasts_prototype(), "contrasts", spec, method)
  if (isTRUE(method$exponentiate) && nrow(output$contrasts)) {
    output$contrasts$estimate <- exp(output$contrasts$estimate)
    output$contrasts$conf_low <- exp(output$contrasts$conf_low)
    output$contrasts$conf_high <- exp(output$contrasts$conf_high)
    output$contrasts$scale <- method$scale
  }
  output$diagnostics <- validate_custom_component(output$diagnostics, diagnostics_prototype(), "diagnostics", spec, method)
  output$issues <- validate_custom_component(output$issues, issues_prototype(), "issues", spec, method)
  output
}

build_analysis_context <- function(spec, frame, data) {
  registry <- variables(data)
  registered_contrasts <- contrasts(data)
  selected_contrasts <- if (nrow(registered_contrasts) == 0L) {
    registered_contrasts
  } else {
    registered_contrasts[
      registered_contrasts$contrast_id %in% spec$contrast_ids[[1]], , drop = FALSE
    ]
  }
  structure(list(
    analysis_id = spec$analysis_id[[1]], formula = spec$formula[[1]],
    model_frame = frame, model_matrix = stats::model.matrix(spec$formula[[1]], frame),
    response = frame[[spec$outcome[[1]]]], weights = if ("..bq_weight" %in% names(frame)) frame$..bq_weight else NULL,
    offset = NULL,
    outcome_spec = registry[match(spec$outcome_id[[1]], registry$var_id), , drop = FALSE],
    predictor_spec = registry[match(spec$predictor_id[[1]], registry$var_id), , drop = FALSE],
    design_spec = list(cluster = spec$cluster[[1]], variance = spec$variance[[1]]),
    contrast_specs = selected_contrasts,
    missing_counts = tibble::tibble(
      n_total = spec$n_total[[1]], n_analyzed = spec$n_analyzed[[1]],
      n_missing_outcome = spec$n_missing_outcome[[1]],
      n_missing_predictor = spec$n_missing_predictor[[1]]
    ), confidence_level = spec$confidence_level[[1]]
  ), class = "analysis_context")
}

validate_custom_component <- function(x, prototype, component, spec, method, required = FALSE) {
  if (is.null(x)) {
    if (required) stop_method_output(paste0("Custom output must contain `", component, "`."))
    return(prototype)
  }
  if (!inherits(x, "data.frame")) stop_method_output(paste0("`", component, "` must be a data frame."))
  if ("stratum_label" %in% names(prototype) && !"stratum_label" %in% names(x)) {
    x$stratum_label <- rep(spec$stratum_label[[1]], nrow(x))
  }
  if (identical(component, "estimates")) {
    if (!"transformation_id" %in% names(x)) {
      x$transformation_id <- rep(NA_character_, nrow(x))
    }
    if (!"transformation_label" %in% names(x)) {
      x$transformation_label <- rep(NA_character_, nrow(x))
    }
  }
  if (identical(component, "contrasts")) {
    if (!"modifier" %in% names(x)) x$modifier <- rep(NA_character_, nrow(x))
    if (!"modifier_level" %in% names(x)) {
      x$modifier_level <- rep(NA_character_, nrow(x))
    }
    x <- add_optional_interaction_contrast_columns(x)
  }
  missing <- setdiff(names(prototype), names(x))
  if (length(missing)) stop_method_output(paste0("`", component, "` is missing columns: ", paste(missing, collapse = ", "), "."))
  x <- tibble::as_tibble(x)[names(prototype)]
  if (nrow(x) && any(x$analysis_id != spec$analysis_id[[1]])) stop_method_output("Custom output contains an incorrect `analysis_id`.")
  if (component %in% c("estimates", "contrasts") && nrow(x)) {
    expected_scale <- if (isTRUE(method$exponentiate)) method$model_scale else method$scale
    if (any(x$effect_measure != method$effect_measure) || any(x$scale != expected_scale)) {
      stop_method_output("Custom estimates violate the declared effect measure or scale.")
    }
  }
  tryCatch(vctrs::vec_cast(x, prototype), error = function(error) {
    stop_method_output(paste0("`", component, "` has incompatible column types."))
  })
}

add_optional_interaction_contrast_columns <- function(x) {
  if (!"inner_contrast" %in% names(x)) {
    x$inner_contrast <- rep(NA_character_, nrow(x))
  }
  if (!"outer_contrast" %in% names(x)) {
    x$outer_contrast <- rep(NA_character_, nrow(x))
  }
  if (!"estimand" %in% names(x)) x$estimand <- rep(NA_character_, nrow(x))
  if (!"exponentiated" %in% names(x)) {
    x$exponentiated <- rep(FALSE, nrow(x))
  }
  x
}

stop_method_output <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_engine_output", "error", "condition")))
}

