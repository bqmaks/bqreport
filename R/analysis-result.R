#' Run a validated analysis plan
#'
#' Only validated tasks with status `ready` are executed. Other tasks are
#' retained in the result plan and represented in the issues component. Engine
#' failures never trigger a fallback method.
#'
#' @param plan A validated `analysis_plan`.
#' @param data A `bq_data` object.
#' @param error Runtime engine error handling: collect, stop, or warn.
#'
#' @return An `analysis_result`.
#' @export
run_analysis <- function(plan, data, error = c("collect", "stop", "warn")) {
  error <- match.arg(error)
  if (!inherits(plan, "analysis_plan")) {
    stop_plan("`plan` must be an analysis_plan.", "bq_error_invalid_plan")
  }
  check_bq_data(data)
  if (any(!plan$validated)) {
    stop_plan(
      "All plan tasks must pass `validate_plan()` before execution.",
      "bq_error_unvalidated_plan"
    )
  }

  plan <- validate_plan(plan, data)
  model_list <- list()
  estimate_rows <- list()
  test_rows <- list()
  diagnostic_rows <- list()
  issue_rows <- list()
  provenance_rows <- list()
  contrast_rows <- list()
  descriptive_rows <- list()

  for (i in seq_len(nrow(plan))) {
    spec <- plan[i, , drop = FALSE]
    analysis_id <- spec$analysis_id[[1]]

    if (spec$status[[1]] != "ready") {
      severity <- if (spec$status[[1]] == "invalid") "error" else "info"
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id,
        "preflight",
        severity,
        paste0("bq_", spec$status[[1]], "_analysis"),
        if (is.na(spec$reason[[1]])) {
          paste0("Analysis status is `", spec$status[[1]], "`.")
        } else {
          spec$reason[[1]]
        }
      )
      next
    }

    if (identical(spec$analysis_type[[1]], "descriptive")) {
      computed <- tryCatch(
        compute_observed_descriptives(spec, data),
        error = function(condition) condition
      )
      if (inherits(computed, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "compute", "error", class(computed)[[1]],
          conditionMessage(computed)
        )
      } else {
        descriptive_rows[[length(descriptive_rows) + 1L]] <- computed
        comparison <- tryCatch(
          compute_descriptive_comparison(spec, data),
          error = function(condition) condition
        )
        if (inherits(comparison, "error")) {
          issue_rows[[length(issue_rows) + 1L]] <- issue_row(
            analysis_id, "comparison", "error", class(comparison)[[1]],
            conditionMessage(comparison)
          )
        } else {
          if (nrow(comparison$contrasts)) {
            contrast_rows[[length(contrast_rows) + 1L]] <- comparison$contrasts
          }
          if (nrow(comparison$tests)) {
            test_rows[[length(test_rows) + 1L]] <- comparison$tests
          }
        }
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    if (identical(spec$analysis_type[[1]], "survival_regression")) {
      output <- tryCatch(
        execute_cox_analysis(spec, data),
        error = function(condition) condition
      )
      if (inherits(output, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "fit", "error", class(output)[[1]],
          conditionMessage(output)
        )
      } else {
        model_list[[analysis_id]] <- output$model
        estimate_rows[[length(estimate_rows) + 1L]] <- output$estimates
        test_rows[[length(test_rows) + 1L]] <- output$tests
        diagnostic_rows[[length(diagnostic_rows) + 1L]] <- output$diagnostics
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    frame <- build_analysis_frame(spec, data)
    captured_warnings <- character()
    if (identical(spec$engine[[1]], "custom_function")) {
      custom_result <- tryCatch(
        execute_custom_method(spec, frame, data),
        error = function(condition) condition
      )
      if (inherits(custom_result, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "fit", "error", class(custom_result)[[1]],
          conditionMessage(custom_result)
        )
        next
      }
      if (!is.null(custom_result$model)) model_list[[analysis_id]] <- custom_result$model
      if (nrow(custom_result$estimates)) estimate_rows[[length(estimate_rows) + 1L]] <- custom_result$estimates
      if (nrow(custom_result$tests)) test_rows[[length(test_rows) + 1L]] <- custom_result$tests
      if (nrow(custom_result$contrasts)) contrast_rows[[length(contrast_rows) + 1L]] <- custom_result$contrasts
      if (nrow(custom_result$diagnostics)) diagnostic_rows[[length(diagnostic_rows) + 1L]] <- custom_result$diagnostics
      if (nrow(custom_result$issues)) issue_rows[[length(issue_rows) + 1L]] <- custom_result$issues
      if (!is.null(custom_result$model)) {
        additional_comparisons <- tryCatch(
          compute_custom_comparisons(custom_result$model, spec, frame, data),
          error = function(condition) condition
        )
        if (inherits(additional_comparisons, "error")) {
          issue_rows[[length(issue_rows) + 1L]] <- issue_row(
            analysis_id, "contrasts", "error", class(additional_comparisons)[[1]],
            conditionMessage(additional_comparisons)
          )
        } else if (nrow(additional_comparisons)) {
          contrast_rows[[length(contrast_rows) + 1L]] <- additional_comparisons
        }
      }
      provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      next
    }
    fit <- tryCatch(
      withCallingHandlers(
        fit_builtin_engine(spec, frame),
        warning = function(condition) {
          captured_warnings <<- c(captured_warnings, conditionMessage(condition))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(condition) condition
    )

    if (inherits(fit, "error")) {
      message <- paste0(
        "Engine `", spec$engine[[1]], "` failed for analysis `",
        analysis_id, "`: ", conditionMessage(fit)
      )
      if (error == "stop") {
        stop_engine(message, analysis_id)
      }
      if (error == "warn") {
        warning(engine_warning(message, analysis_id), call. = FALSE)
      }
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "fit", "error", class(fit)[[1]], message
      )
      next
    }

    if ("..bq_cluster" %in% names(frame)) {
      attr(fit, "bq_clusters") <- frame[["..bq_cluster"]]
    }

    model_list[[analysis_id]] <- fit
    post_fit <- withCallingHandlers(
      list(
        estimates = tidy_builtin_estimates(fit, spec),
        tests = tidy_builtin_test(fit, spec, frame),
        diagnostics = diagnose_builtin(fit, spec)
      ),
      warning = function(condition) {
        captured_warnings <<- c(captured_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    estimate_rows[[length(estimate_rows) + 1L]] <- post_fit$estimates
    computed_contrasts <- compute_builtin_contrasts(post_fit$estimates, spec, data)
    if (nrow(computed_contrasts) > 0L) {
      contrast_rows[[length(contrast_rows) + 1L]] <- computed_contrasts
    }
    conditional_contrasts <- tryCatch(
      compute_conditional_contrasts(fit, spec, data),
      error = function(condition) condition
    )
    if (inherits(conditional_contrasts, "error")) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "contrasts", "error", class(conditional_contrasts)[[1]],
        conditionMessage(conditional_contrasts)
      )
    } else if (nrow(conditional_contrasts)) {
      contrast_rows[[length(contrast_rows) + 1L]] <- conditional_contrasts
    }
    custom_comparisons <- tryCatch(
      compute_custom_comparisons(fit, spec, frame, data),
      error = function(condition) condition
    )
    if (inherits(custom_comparisons, "error")) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "contrasts", "error", class(custom_comparisons)[[1]],
        conditionMessage(custom_comparisons)
      )
    } else if (nrow(custom_comparisons)) {
      contrast_rows[[length(contrast_rows) + 1L]] <- custom_comparisons
    }
    test_rows[[length(test_rows) + 1L]] <- post_fit$tests
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <- post_fit$diagnostics
    provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)

    for (message in unique(captured_warnings)) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "fit", "warning", "bq_warning_engine", message
      )
    }
  }

  structure(
    list(
      plan = plan,
      models = model_list,
      estimates = bind_component(estimate_rows, estimates_prototype()),
      contrasts = bind_component(contrast_rows, contrasts_prototype()),
      tests = bind_component(test_rows, tests_prototype()),
      descriptives = bind_component(descriptive_rows, descriptives_prototype()),
      diagnostics = bind_component(diagnostic_rows, diagnostics_prototype()),
      issues = bind_component(issue_rows, issues_prototype()),
      provenance = bind_component(provenance_rows, provenance_prototype())
    ),
    class = "analysis_result"
  )
}

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

stop_method_output <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_engine_output", "error", "condition")))
}

build_analysis_frame <- function(spec, data) {
  data <- data[stratum_mask(data, spec$strata[[1]], spec$stratum_values[[1]]), , drop = FALSE]
  outcome_name <- spec$outcome[[1]]
  predictor_name <- spec$predictor[[1]]
  registry <- variables(data)
  outcome_spec <- registry[match(spec$outcome_id[[1]], registry$var_id), , drop = FALSE]
  predictor_spec <- registry[match(spec$predictor_id[[1]], registry$var_id), , drop = FALSE]
  outcome_original <- data[[outcome_name]]
  predictor_original <- data[[predictor_name]]
  outcome <- analysis_vector(outcome_original)
  predictor <- analysis_vector(predictor_original)
  outcome[special_missing_mask(outcome_original)] <- NA
  predictor[special_missing_mask(predictor_original)] <- NA
  predictor_transformation <- spec$transformation_specs[[1]][[spec$predictor_id[[1]]]]
  predictor <- apply_transformation_spec(
    predictor, predictor_transformation, predictor_name, spec$analysis_id[[1]]
  )

  if (outcome_spec$type[[1]] == "binary") {
    event <- outcome_spec$event_value[[1]]
    outcome <- ifelse(is.na(outcome), NA_integer_, as.integer(outcome == event))
  }
  if (predictor_spec$type[[1]] %in% c("binary", "nominal")) {
    predictor <- factor(predictor)
    predictor <- stats::relevel(
      predictor,
      ref = as.character(predictor_spec$reference[[1]])
    )
  }

  frame <- tibble::tibble(outcome, predictor)
  names(frame) <- c(outcome_name, predictor_name)
  for (name in spec$covariates[[1]]) {
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    covariate_id <- variables(data)$var_id[match(name, variables(data)$name)]
    value <- apply_transformation_spec(
      value, spec$transformation_specs[[1]][[covariate_id]],
      name, spec$analysis_id[[1]]
    )
    frame[[name]] <- value
  }
  for (name in spec$effect_modifiers[[1]]) {
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    modifier_spec <- variables(data)[match(name, variables(data)$name), , drop = FALSE]
    if (modifier_spec$type[[1]] %in% c("binary", "nominal")) {
      value <- factor(value)
      reference <- modifier_spec$reference[[1]]
      if (!is.null(reference)) value <- stats::relevel(value, ref = as.character(reference))
    }
    frame[[name]] <- value
  }
  if (!is.na(spec$weight[[1]])) frame[["..bq_weight"]] <- data[[spec$weight[[1]]]]
  if (!is.na(spec$cluster[[1]])) frame[["..bq_cluster"]] <- data[[spec$cluster[[1]]]]
  frame
}

analysis_vector <- function(x) {
  if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else x
}

fit_builtin_engine <- function(spec, frame) {
  fit_formula <- spec$formula[[1]]
  environment(fit_formula) <- environment()
  fit_weights <- if ("..bq_weight" %in% names(frame)) frame[["..bq_weight"]] else NULL
  if (spec$engine[[1]] == "lm") {
    if (!is.null(fit_weights)) {
      return(stats::lm(fit_formula, data = frame, weights = fit_weights, na.action = stats::na.omit))
    }
    return(stats::lm(fit_formula, data = frame, na.action = stats::na.omit))
  }
  if (spec$engine[[1]] == "glm") {
    if (!is.null(fit_weights)) {
      return(stats::glm(
        fit_formula, data = frame, family = stats::binomial("logit"),
        weights = fit_weights, na.action = stats::na.omit
      ))
    }
    return(stats::glm(
      fit_formula,
      data = frame,
      family = stats::binomial("logit"),
      na.action = stats::na.omit
    ))
  }
  stop(paste0("Unknown built-in engine `", spec$engine[[1]], "`."))
}

tidy_builtin_estimates <- function(fit, spec) {
  coefficients <- summary(fit)$coefficients
  coefficient_metadata <- normalize_coefficient_metadata(fit, spec)
  beta <- unname(coefficients[, "Estimate"])
  standard_error <- unname(coefficients[, "Std. Error"])
  if (spec$variance[[1]] == "robust") {
    robust_covariance <- sandwich::vcovHC(fit, type = "HC0")
    standard_error <- unname(sqrt(diag(robust_covariance)))
  } else if (spec$variance[[1]] == "cluster_robust") {
    model_rows <- as.integer(rownames(stats::model.frame(fit)))
    frame_clusters <- attr(fit, "bq_clusters")
    robust_covariance <- sandwich::vcovCL(
      fit, cluster = frame_clusters[model_rows], type = "HC1", cadjust = TRUE
    )
    standard_error <- unname(sqrt(diag(robust_covariance)))
  }
  is_glm <- inherits(fit, "glm")
  statistic <- beta / standard_error
  if (!is_glm) {
    df <- rep.int(stats::df.residual(fit), length(beta))
    critical <- stats::qt((1 + spec$confidence_level[[1]]) / 2, df = df)
    p_value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
    estimate <- beta
    conf_low <- beta - critical * standard_error
    conf_high <- beta + critical * standard_error
    scale <- "identity"
    std_error_scale <- "identity"
    n_events <- NA_integer_
  } else {
    df <- rep(NA_real_, length(beta))
    critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
    p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    estimate <- beta
    conf_low <- beta - critical * standard_error
    conf_high <- beta + critical * standard_error
    scale <- "link"
    std_error_scale <- "log_odds"
    n_events <- as.integer(sum(stats::model.response(stats::model.frame(fit)) == 1))
  }
  if (isTRUE(spec$exponentiate[[1]])) {
    estimate <- exp(estimate)
    conf_low <- exp(conf_low)
    conf_high <- exp(conf_high)
    scale <- "ratio"
  }
  transformation_metadata <- coefficient_transformation_metadata(
    coefficient_metadata$term, spec
  )

  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]],
    stratum_label = spec$stratum_label[[1]],
    transformation_id = transformation_metadata$id,
    transformation_label = transformation_metadata$label,
    term = coefficient_metadata$term,
    level = coefficient_metadata$level,
    estimate = estimate,
    std_error = standard_error,
    std_error_scale = std_error_scale,
    conf_low = conf_low,
    conf_high = conf_high,
    statistic = statistic,
    df = as.numeric(df),
    p_value = p_value,
    effect_measure = spec$effect_measure[[1]],
    scale = scale,
    n = as.integer(stats::nobs(fit)),
    n_events = n_events,
    method = spec$method[[1]]
    , variance = spec$variance[[1]]
  )
}

coefficient_transformation_metadata <- function(terms, spec) {
  ids <- rep(NA_character_, length(terms))
  labels <- rep(NA_character_, length(terms))
  variable_ids <- c(spec$predictor_id[[1]], spec$covariate_ids[[1]])
  variable_names <- c(spec$predictor[[1]], spec$covariates[[1]])
  for (i in seq_along(variable_ids)) {
    transformation <- spec$transformation_specs[[1]][[variable_ids[[i]]]]
    rows <- terms == variable_names[[i]]
    if (!is.null(transformation)) {
      ids[rows] <- transformation$id
      labels[rows] <- transformation$label
    }
  }
  list(id = ids, label = labels)
}

normalize_coefficient_metadata <- function(fit, spec) {
  coefficient_names <- rownames(summary(fit)$coefficients)
  terms <- coefficient_names
  levels <- rep(NA_character_, length(coefficient_names))
  intercept <- coefficient_names == "(Intercept)"
  terms[intercept] <- "(Intercept)"

  model_frame <- stats::model.frame(fit)
  predictor_name <- spec$predictor[[1]]
  predictor <- model_frame[[predictor_name]]
  if (is.factor(predictor)) {
    design <- stats::model.matrix(fit)
    assignment <- attr(design, "assign")
    design_columns <- colnames(design)
    predictor_columns <- design_columns[assignment == 1L]
    coefficient_rows <- match(predictor_columns, coefficient_names)
    contrast_matrix <- stats::contrasts(predictor)
    contrast_columns <- colnames(contrast_matrix)
    represented_levels <- vapply(contrast_columns, function(column) {
      nonzero <- which(contrast_matrix[, column] != 0)
      if (length(nonzero) == 1L && contrast_matrix[nonzero, column] == 1) {
        rownames(contrast_matrix)[[nonzero]]
      } else {
        NA_character_
      }
    }, character(1))

    valid <- !is.na(coefficient_rows)
    terms[coefficient_rows[valid]] <- predictor_name
    levels[coefficient_rows[valid]] <- represented_levels[valid]
  } else {
    predictor_row <- match(predictor_name, coefficient_names)
    if (!is.na(predictor_row)) {
      terms[[predictor_row]] <- predictor_name
    }
  }

  tibble::tibble(term = unname(terms), level = unname(levels))
}

tidy_builtin_test <- function(fit, spec, frame) {
  null_formula <- rlang::new_formula(
    lhs = rlang::sym(spec$outcome[[1]]),
    rhs = 1,
    env = baseenv()
  )
  if (!inherits(fit, "glm")) {
    null_fit <- stats::lm(null_formula, data = frame, na.action = stats::na.omit)
    comparison <- stats::anova(null_fit, fit)
    statistic <- comparison$F[[2]]
    df <- comparison$Df[[2]]
    p_value <- comparison$`Pr(>F)`[[2]]
    test <- "partial_f"
  } else {
    null_fit <- stats::glm(
      null_formula,
      data = frame,
      family = stats::binomial("logit"),
      na.action = stats::na.omit
    )
    comparison <- stats::anova(null_fit, fit, test = "Chisq")
    statistic <- comparison$Deviance[[2]]
    df <- comparison$Df[[2]]
    p_value <- comparison$`Pr(>Chi)`[[2]]
    test <- "likelihood_ratio"
  }
  overall <- tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]],
    test = test,
    statistic = unname(statistic),
    df = unname(as.numeric(df)),
    p_value = unname(p_value),
    method = spec$method[[1]]
  )
  if (length(spec$effect_modifiers[[1]]) == 0L) return(overall)
  interaction_tests <- lapply(spec$effect_modifiers[[1]], function(modifier) {
    reduced_formula <- new_analysis_formula(
      spec$outcome[[1]], spec$predictor[[1]],
      c(spec$covariates[[1]], modifier), character()
    )
    if (!inherits(fit, "glm")) {
      reduced <- stats::lm(reduced_formula, data = frame, na.action = stats::na.omit)
      comparison <- stats::anova(reduced, fit)
      statistic <- comparison$F[[2]]
      p_value <- comparison$`Pr(>F)`[[2]]
      df <- comparison$Df[[2]]
    } else {
      reduced <- stats::glm(reduced_formula, data = frame,
        family = stats::binomial("logit"), na.action = stats::na.omit)
      comparison <- stats::anova(reduced, fit, test = "Chisq")
      statistic <- comparison$Deviance[[2]]
      p_value <- comparison$`Pr(>Chi)`[[2]]
      df <- comparison$Df[[2]]
    }
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], test = "interaction",
      statistic = unname(statistic), df = as.numeric(df),
      p_value = unname(p_value), method = spec$method[[1]]
    )
  })
  vctrs::vec_rbind(overall, !!!interaction_tests)
}

diagnose_builtin <- function(fit, spec) {
  if (!inherits(fit, "glm")) {
    fit_summary <- summary(fit)
    metrics <- c(
      sigma = fit_summary$sigma,
      r_squared = fit_summary$r.squared,
      adjusted_r_squared = fit_summary$adj.r.squared
    )
  } else {
    metrics <- c(
      converged = as.numeric(isTRUE(fit$converged)),
      deviance = fit$deviance,
      null_deviance = fit$null.deviance
    )
  }
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    metric = names(metrics),
    value = unname(as.numeric(metrics)),
    status = "observed",
    message = NA_character_
  )
}

provenance_row <- function(spec) {
  transformations <- spec$transformation_specs[[1]]
  transformations <- transformations[!vapply(transformations, is.null, logical(1))]
  descriptive_functions <- if (
    "descriptive_functions" %in% names(spec)
  ) spec$descriptive_functions[[1]] else list()
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    method = spec$method[[1]],
    engine = spec$engine[[1]],
    selector_id = spec$selector_id[[1]],
    selector_hash = spec$selector_hash[[1]],
    candidate_methods = spec$candidate_methods,
    selection_reason = spec$selection_reason[[1]],
    selection_diagnostics = spec$selection_diagnostics,
    function_id = spec$function_id[[1]],
    function_hash = spec$function_hash[[1]],
    r_version = as.character(getRversion()),
    required_packages = spec$required_packages,
    package_versions = list(c(stats = as.character(utils::packageVersion("stats")))),
    transformation_ids = list(vapply(transformations, `[[`, character(1), "id")),
    transformation_hashes = list(vapply(transformations, `[[`, character(1), "function_hash")),
    transformation_parameters = list(lapply(transformations, `[[`, "parameters")),
    descriptive_function_ids = list(vapply(
      descriptive_functions, `[[`, character(1), "id"
    )),
    descriptive_function_hashes = list(vapply(
      descriptive_functions, `[[`, character(1), "function_hash"
    )),
    comparison_method = if (
      "comparison_method" %in% names(spec)
    ) spec$comparison_method[[1]] else NA_character_,
    comparison_estimand = if (
      "comparison_estimand" %in% names(spec)
    ) spec$comparison_estimand[[1]] else NA_character_,
    comparison_scale = if (
      "comparison_scale" %in% names(spec)
    ) spec$comparison_scale[[1]] else NA_character_,
    comparison_ci_method = if (
      "comparison_ci_method" %in% names(spec)
    ) spec$comparison_ci_method[[1]] else NA_character_,
    comparison_function_hash = if (
      "comparison_function_hash" %in% names(spec)
    ) spec$comparison_function_hash[[1]] else NA_character_
  )
}

issue_row <- function(analysis_id, stage, severity, condition_class, message) {
  tibble::tibble(
    analysis_id = analysis_id,
    stage = stage,
    severity = severity,
    condition_class = condition_class,
    message = message
  )
}

bind_component <- function(rows, prototype) {
  if (length(rows) == 0L) return(prototype)
  vctrs::vec_rbind(!!!rows)
}

estimates_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
    stratum_label = character(),
    transformation_id = character(), transformation_label = character(),
    term = character(), level = character(), estimate = double(),
    std_error = double(), std_error_scale = character(), conf_low = double(),
    conf_high = double(), statistic = double(), df = double(), p_value = double(),
    effect_measure = character(), scale = character(), n = integer(),
    n_events = integer(), method = character(), variance = character()
  )
}

contrasts_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
      contrast_id = character(), contrast = character(), numerator = character(), denominator = character(),
    modifier = character(), modifier_level = character(),
    estimate = double(), conf_low = double(), conf_high = double(),
    p_value = double(), p_adjusted = double(), adjust_method = character(),
    effect_measure = character(), scale = character()
  )
}

tests_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
    test = character(), statistic = double(), df = double(), p_value = double(),
    method = character()
  )
}

diagnostics_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), metric = character(), value = double(),
    status = character(), message = character()
  )
}

issues_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), stage = character(), severity = character(),
    condition_class = character(), message = character()
  )
}

provenance_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), method = character(), engine = character(),
    selector_id = character(), selector_hash = character(),
    candidate_methods = list(), selection_reason = character(),
    selection_diagnostics = list(),
    function_id = character(), function_hash = character(), r_version = character(),
    required_packages = list(), package_versions = list()
    , transformation_ids = list(), transformation_hashes = list(),
    transformation_parameters = list(), descriptive_function_ids = list(),
    descriptive_function_hashes = list(), comparison_method = character(),
    comparison_estimand = character(), comparison_scale = character(),
    comparison_ci_method = character(), comparison_function_hash = character()
  )
}

stop_engine <- function(message, analysis_id) {
  condition <- structure(
    list(message = message, call = sys.call(-1L), analysis_id = analysis_id),
    class = c("bq_error_engine", "error", "condition")
  )
  stop(condition)
}

engine_warning <- function(message, analysis_id) {
  structure(
    list(message = message, call = NULL, analysis_id = analysis_id),
    class = c("bq_warning_engine", "warning", "condition")
  )
}

check_analysis_result <- function(x) {
  if (!inherits(x, "analysis_result")) {
    stop_plan("`x` must be an analysis_result.", "bq_error_invalid_result")
  }
  invisible(x)
}

#' Access analysis result components
#'
#' @param x An `analysis_result`.
#'
#' @return The requested tidy result component, or a named list for `models()`.
#' @export
estimates <- function(x) {
  check_analysis_result(x)
  x$estimates
}

#' @rdname contrasts
#' @export
contrasts.analysis_result <- function(x) {
  check_analysis_result(x)
  x$contrasts
}

#' @rdname estimates
#' @export
tests <- function(x) {
  check_analysis_result(x)
  x$tests
}

#' @rdname estimates
#' @export
diagnostics <- function(x) {
  check_analysis_result(x)
  x$diagnostics
}

#' @rdname estimates
#' @export
issues <- function(x) {
  check_analysis_result(x)
  x$issues
}

#' @rdname estimates
#' @export
models <- function(x) {
  check_analysis_result(x)
  x$models
}

#' @rdname estimates
#' @export
descriptives <- function(x) {
  check_analysis_result(x)
  x$descriptives
}
