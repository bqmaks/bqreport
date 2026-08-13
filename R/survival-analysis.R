#' Construct a Cox proportional hazards method
#'
#' @param ties Ties method passed to `survival::coxph()`.
#' @return A concrete method specification.
#' @export
cox_model <- function(ties = c("efron", "breslow", "exact")) {
  ties <- match.arg(ties)
  method <- new_method_spec(
    "cox_proportional_hazards", "coxph", "partial_likelihood", "wald",
    NA_character_, NA_character_, "hazard_ratio",
    scale = "ratio", model_scale = "log_hazard",
    required_packages = "survival", exponentiate = TRUE
  )
  method$ties <- ties
  method
}

#' Compile a Cox survival analysis plan
#'
#' @param .data A `bq_data` object.
#' @param outcomes Composite survival outcomes selected with tidyselect syntax.
#' @param predictors Predictor columns selected with tidyselect syntax.
#' @param covariates Optional adjustment covariates selected with tidyselect.
#' @param confidence_level Confidence level.
#' @param method A concrete Cox method specification.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_survival <- function(
  .data,
  outcomes = tidyselect::everything(),
  predictors = all_predictors(),
  covariates = tidyselect::any_of(character()),
  confidence_level = 0.95,
  method = cox_model()
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (!inherits(method, "method_spec") || method$engine != "coxph") {
    stop_plan("`method` must be a Cox method specification.",
      "bq_error_invalid_method_contract")
  }
  outcome_registry <- resolve_outcomes(.data)
  survival_registry <- outcome_registry[outcome_registry$type == "survival", , drop = FALSE]
  selection_data <- stats::setNames(
    as.data.frame(rep(list(logical()), nrow(survival_registry))),
    survival_registry$name
  )
  outcome_selection <- tidyselect::eval_select(
    rlang::enquo(outcomes), selection_data
  )
  predictor_selection <- tidyselect::eval_select(
    rlang::enquo(predictors), .data
  )
  covariate_selection <- tidyselect::eval_select(
    rlang::enquo(covariates), .data
  )
  if (length(outcome_selection) == 0L || length(predictor_selection) == 0L) {
    return(empty_analysis_plan())
  }
  variable_registry <- variables(.data)
  pairs <- expand.grid(
    outcome = names(outcome_selection), predictor = names(predictor_selection),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    outcome_spec <- survival_registry[
      match(pairs$outcome[[i]], survival_registry$name), , drop = FALSE
    ]
    predictor_spec <- variable_registry[
      match(pairs$predictor[[i]], variable_registry$name), , drop = FALSE
    ]
    pseudo_outcome <- tibble::tibble(
      var_id = outcome_spec$outcome_id, name = outcome_spec$name
    )
    row <- analysis_plan_row(
      pseudo_outcome, predictor_spec, method,
      status = if (outcome_spec$status[[1]] == "valid" &&
        predictor_spec$status[[1]] == "valid") "ready" else "review",
      reason = if (outcome_spec$status[[1]] == "valid" &&
        predictor_spec$status[[1]] == "valid") NA_character_ else
        "Outcome or predictor metadata require review.",
      confidence_level = confidence_level,
      contrast_ids = contrast_ids_for(.data, predictor_spec$var_id[[1]]),
      covariate_specs = variable_registry[
        match(names(covariate_selection), variable_registry$name), , drop = FALSE
      ]
    )
    row$analysis_type <- "survival_regression"
    row$survival_outcome_id <- outcome_spec$outcome_id
    row$time_id <- outcome_spec$time_var_id
    row$event_id <- outcome_spec$event_var_id
    row$event_value <- outcome_spec$event_value
    row$time_unit <- outcome_spec$time_unit
    row$ties <- method$ties
    row$formula <- list(NULL)
    row
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_survival_plan_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  issues <- character()
  outcome_registry <- outcomes(data)
  outcome_row <- match(
    plan$survival_outcome_id[[i]], outcome_registry$outcome_id
  )
  predictor_row <- match(plan$predictor_id[[i]], registry$var_id)
  covariate_rows <- match(plan$covariate_ids[[i]], registry$var_id)
  if (is.na(outcome_row) || outcome_registry$status[[outcome_row]] != "valid") {
    issues <- c(issues, "The survival outcome or one of its components is absent.")
  }
  if (is.na(predictor_row)) {
    issues <- c(issues, "The predictor referenced by stable id is absent.")
  }
  if (anyNA(covariate_rows)) {
    issues <- c(issues, "A covariate referenced by stable id is absent.")
  }
  if (!is.na(outcome_row) && !is.na(predictor_row)) {
    outcome <- outcome_registry[outcome_row, , drop = FALSE]
    plan$outcome[[i]] <- outcome$name[[1]]
    plan$time_id[[i]] <- outcome$time_var_id[[1]]
    plan$event_id[[i]] <- outcome$event_var_id[[1]]
    plan$event_value[i] <- outcome$event_value
    plan$time_unit[[i]] <- outcome$time_unit[[1]]
    plan$predictor[[i]] <- registry$name[[predictor_row]]
    covariate_names <- registry$name[covariate_rows[!is.na(covariate_rows)]]
    plan$covariates[[i]] <- covariate_names
    time <- data[[outcome$time[[1]]]]
    event <- data[[outcome$event[[1]]]]
    predictor <- data[[registry$name[[predictor_row]]]]
    complete <- !special_missing_mask(time) & !special_missing_mask(event) &
      !special_missing_mask(predictor)
    for (name in covariate_names) {
      complete <- complete & !special_missing_mask(data[[name]])
    }
    time_values <- analysis_vector(time)[complete]
    event_values <- analysis_vector(event)[complete]
    predictor_values <- analysis_vector(predictor)[complete]
    plan$n_total[[i]] <- nrow(data)
    plan$n_eligible[[i]] <- nrow(data)
    plan$n_analyzed[[i]] <- sum(complete)
    plan$n_missing_outcome[[i]] <- sum(
      special_missing_mask(time) | special_missing_mask(event)
    )
    plan$n_missing_predictor[[i]] <- sum(special_missing_mask(predictor))
    if (!is.numeric(time_values)) {
      issues <- c(issues, "Survival time must be numeric.")
    } else if (any(time_values <= 0)) {
      issues <- c(issues, "Survival time must contain only positive values.")
    }
    event_count <- sum(event_values == outcome$event_value[[1]])
    if (event_count == 0L) issues <- c(issues, "No events are available for Cox analysis.")
    if (n_distinct_values(predictor_values) < 2L) {
      issues <- c(issues, "Predictor has no variation in the analyzed data.")
    }
    if (registry$type[[predictor_row]] %in% c("binary", "nominal", "ordinal")) {
      reference <- registry$reference[[predictor_row]]
      if (is.null(reference) || !reference %in% predictor_values) {
        issues <- c(issues, "Categorical predictor has no observed configured reference.")
      }
    }
    current_model_terms <- plan$model_term_specs[[i]]
    if (!is.null(current_model_terms[[plan$predictor_id[[i]]]])) {
      issues <- c(
        issues,
        "Nonlinear model terms are currently supported only for adjustment covariates, not the primary predictor."
      )
    }
    for (j in seq_along(covariate_names)) {
      covariate_id <- plan$covariate_ids[[i]][[j]]
      value <- analysis_vector(data[[covariate_names[[j]]]])
      value[special_missing_mask(data[[covariate_names[[j]]]])] <- NA
      transformed <- tryCatch(
        apply_transformation_spec(
          value, plan$transformation_specs[[i]][[covariate_id]],
          covariate_names[[j]], plan$analysis_id[[i]]
        ),
        error = function(condition) condition
      )
      if (inherits(transformed, "error")) {
        issues <- c(issues, conditionMessage(transformed))
        next
      }
      term <- current_model_terms[[covariate_id]]
      if (!is.null(term)) {
        resolved <- tryCatch(
          resolve_model_term_spec(
            term, transformed[complete], covariate_names[[j]],
            plan$analysis_id[[i]]
          ),
          error = function(condition) condition
        )
        if (inherits(resolved, "error")) {
          issues <- c(issues, conditionMessage(resolved))
        } else {
          current_model_terms[[covariate_id]] <- resolved
        }
      }
    }
    plan$model_term_specs[[i]] <- current_model_terms
    formula_covariates <- model_formula_covariates(
      plan$covariate_ids[[i]], covariate_names, current_model_terms
    )
    plan$formula[[i]] <- stats::reformulate(
      c(plan$predictor[[i]], formula_covariates),
      response = "survival::Surv(..bq_time, ..bq_event)"
    )
  }
  missing_packages <- plan$required_packages[[i]][
    !vapply(plan$required_packages[[i]], requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    issues <- c(issues, paste0(
      "Missing required packages: ", paste(missing_packages, collapse = ", "), "."
    ))
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

execute_cox_analysis <- function(spec, data) {
  registry <- variables(data)
  outcome <- outcomes(data)
  outcome <- outcome[outcome$outcome_id == spec$survival_outcome_id[[1]], , drop = FALSE]
  predictor_name <- registry$name[match(spec$predictor_id[[1]], registry$var_id)]
  time <- analysis_vector(data[[outcome$time[[1]]]])
  event_original <- data[[outcome$event[[1]]]]
  event <- analysis_vector(event_original)
  predictor_original <- data[[predictor_name]]
  predictor <- analysis_vector(predictor_original)
  time[special_missing_mask(data[[outcome$time[[1]]]])] <- NA
  event[special_missing_mask(event_original)] <- NA
  predictor[special_missing_mask(predictor_original)] <- NA
  predictor <- apply_transformation_spec(
    predictor, spec$transformation_specs[[1]][[spec$predictor_id[[1]]]],
    predictor_name, spec$analysis_id[[1]]
  )
  predictor_spec <- registry[match(spec$predictor_id[[1]], registry$var_id), , drop = FALSE]
  if (predictor_spec$type[[1]] %in% c("binary", "nominal", "ordinal")) {
    predictor <- stats::relevel(
      factor(predictor), ref = as.character(predictor_spec$reference[[1]])
    )
  }
  frame <- tibble::tibble(
    ..bq_time = time,
    ..bq_event = event == outcome$event_value[[1]],
    predictor = predictor
  )
  names(frame)[[3]] <- predictor_name
  for (i in seq_along(spec$covariates[[1]])) {
    name <- spec$covariates[[1]][[i]]
    covariate_id <- spec$covariate_ids[[1]][[i]]
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    value <- apply_transformation_spec(
      value, spec$transformation_specs[[1]][[covariate_id]],
      name, spec$analysis_id[[1]]
    )
    term <- spec$model_term_specs[[1]][[covariate_id]]
    if (is.null(term)) {
      frame[[name]] <- value
    } else {
      basis <- apply_model_term_spec(value, term)
      for (column in seq_along(term$output_names)) {
        frame[[term$output_names[[column]]]] <- basis[, column]
      }
    }
  }
  formula <- spec$formula[[1]]
  fit <- survival::coxph(
    formula, data = frame, ties = spec$ties[[1]], na.action = stats::na.omit,
    x = TRUE, model = TRUE
  )
  summary_fit <- summary(fit)
  beta <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
  estimates <- tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]], stratum_label = NA_character_,
    transformation_id = NA_character_, transformation_label = NA_character_,
    term = names(beta), level = NA_character_, estimate = exp(unname(beta)),
    std_error = unname(se), std_error_scale = "log_hazard",
    conf_low = exp(unname(beta) - critical * unname(se)),
    conf_high = exp(unname(beta) + critical * unname(se)),
    statistic = unname(beta / se), df = NA_real_,
    p_value = unname(summary_fit$coefficients[, "Pr(>|z|)"]),
    effect_measure = "hazard_ratio", scale = "ratio",
    n = as.integer(stats::nobs(fit)), n_events = as.integer(fit$nevent),
    method = "cox_proportional_hazards", variance = "model_based"
  )
  tests <- tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]], test = "likelihood_ratio",
    statistic = unname(summary_fit$logtest[[1]]),
    df = unname(summary_fit$logtest[[2]]),
    p_value = unname(summary_fit$logtest[[3]]),
    method = "cox_proportional_hazards"
  )
  formula_covariates <- model_formula_covariates(
    spec$covariate_ids[[1]], spec$covariates[[1]], spec$model_term_specs[[1]]
  )
  if (is.factor(frame[[predictor_name]])) {
    reduced_formula <- if (length(formula_covariates)) {
      stats::reformulate(
        formula_covariates,
        response = "survival::Surv(..bq_time, ..bq_event)"
      )
    } else {
      stats::as.formula("survival::Surv(..bq_time, ..bq_event) ~ 1")
    }
    reduced <- survival::coxph(
      reduced_formula, data = frame, ties = spec$ties[[1]],
      na.action = stats::na.omit
    )
    comparison <- stats::anova(reduced, fit, test = "Chisq")
    tests <- vctrs::vec_rbind(tests, tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], test = "predictor_omnibus",
      statistic = unname(comparison$Chisq[[2]]),
      df = unname(as.numeric(comparison$Df[[2]])),
      p_value = unname(comparison$`Pr(>|Chi|)`[[2]]),
      method = "cox_proportional_hazards"
    ))
  }
  term_tests <- lapply(seq_along(spec$covariates[[1]]), function(i) {
    covariate_id <- spec$covariate_ids[[1]][[i]]
    term <- spec$model_term_specs[[1]][[covariate_id]]
    if (is.null(term)) return(NULL)
    reduced_formula <- stats::reformulate(
      c(predictor_name, setdiff(formula_covariates, term$output_names)),
      response = "survival::Surv(..bq_time, ..bq_event)"
    )
    reduced <- survival::coxph(
      reduced_formula, data = frame, ties = spec$ties[[1]],
      na.action = stats::na.omit
    )
    comparison <- stats::anova(reduced, fit, test = "Chisq")
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$covariates[[1]][[i]], test = "model_term_omnibus",
      statistic = unname(comparison$Chisq[[2]]),
      df = unname(as.numeric(comparison$Df[[2]])),
      p_value = unname(comparison$`Pr(>|Chi|)`[[2]]),
      method = "cox_proportional_hazards"
    )
  })
  tests <- vctrs::vec_rbind(tests, !!!term_tests)
  ph <- survival::cox.zph(fit)
  ph_table <- as.data.frame(ph$table)
  diagnostics <- tibble::tibble(
    analysis_id = spec$analysis_id[[1]], metric = rownames(ph_table),
    value = as.numeric(ph_table[, "p"]), status = "observed",
    message = NA_character_
  )
  model_contrasts <- compute_builtin_contrasts(fit, spec, data)
  list(
    model = fit, estimates = estimates, tests = tests,
    contrasts = model_contrasts, diagnostics = diagnostics
  )
}
