#' Construct Fine--Gray competing-risks regression
#'
#' @param cause Non-censoring event value defining the cumulative-incidence
#'   estimand.
#' @param exponentiate Return subdistribution hazard ratios instead of log
#'   subdistribution hazards.
#'
#' @return A concrete Fine--Gray method specification.
#' @export
fine_gray_model <- function(cause, exponentiate = TRUE) {
  if (length(cause) != 1L || is.na(cause)) {
    stop_method_contract("`cause` must be one non-missing event value.")
  }
  check_exponentiate(exponentiate)
  method <- new_method_spec(
    "fine_gray_regression", "crr", "weighted_estimating_equation", "wald",
    NA_character_, NA_character_,
    if (exponentiate) "subdistribution_hazard_ratio" else
      "log_subdistribution_hazard",
    scale = if (exponentiate) "ratio" else "link", model_scale = "link",
    required_packages = "cmprsk", exponentiate = exponentiate
  )
  method$cause <- cause
  method
}

#' Compile a Fine--Gray regression plan
#'
#' @param .data A `bq_data` object.
#' @param outcomes Composite competing-risk outcomes selected with tidyselect.
#' @param predictors Predictor columns selected with tidyselect.
#' @param covariates Optional adjustment covariates selected with tidyselect.
#' @param confidence_level Confidence level.
#' @param method A [fine_gray_model()] specification containing the target
#'   event cause.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_fine_gray <- function(
  .data, outcomes = tidyselect::everything(), predictors = all_predictors(),
  covariates = tidyselect::any_of(character()), confidence_level = 0.95,
  method
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (missing(method) || !inherits(method, "method_spec") || method$engine != "crr") {
    stop_method_contract("`method` must be a fine_gray_model specification.")
  }
  outcome_registry <- resolve_outcomes(.data)
  competing <- outcome_registry[outcome_registry$type == "competing_risk", , drop = FALSE]
  selection_data <- stats::setNames(
    as.data.frame(rep(list(logical()), nrow(competing))), competing$name
  )
  selected_outcomes <- tidyselect::eval_select(rlang::enquo(outcomes), selection_data)
  selected_predictors <- tidyselect::eval_select(rlang::enquo(predictors), .data)
  selected_covariates <- tidyselect::eval_select(rlang::enquo(covariates), .data)
  if (!length(selected_outcomes) || !length(selected_predictors)) {
    return(empty_analysis_plan())
  }
  registry <- variables(.data)
  pairs <- expand.grid(
    outcome = names(selected_outcomes), predictor = names(selected_predictors),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    outcome <- competing[match(pairs$outcome[[i]], competing$name), , drop = FALSE]
    predictor <- registry[match(pairs$predictor[[i]], registry$name), , drop = FALSE]
    row <- analysis_plan_row(
      tibble::tibble(var_id = outcome$outcome_id, name = outcome$name),
      predictor, method,
      status = if (outcome$status[[1]] == "valid" && predictor$status[[1]] == "valid")
        "ready" else "review",
      reason = NA_character_, confidence_level = confidence_level,
      covariate_specs = registry[
        match(names(selected_covariates), registry$name), , drop = FALSE
      ]
    )
    row$analysis_type <- "fine_gray_regression"
    row$survival_outcome_id <- outcome$outcome_id
    row$time_id <- outcome$time_var_id
    row$event_id <- outcome$event_var_id
    row$censor_value <- outcome$censor_value
    row$event_cause <- list(method$cause)
    row$formula <- list(NULL)
    refine_analysis_id(row, outcome$outcome_id, method$cause)
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_fine_gray_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  outcomes_registry <- outcomes(data)
  outcome_row <- match(plan$survival_outcome_id[[i]], outcomes_registry$outcome_id)
  predictor_row <- match(plan$predictor_id[[i]], registry$var_id)
  covariate_rows <- match(plan$covariate_ids[[i]], registry$var_id)
  issues <- character()
  if (is.na(outcome_row) || outcomes_registry$status[[outcome_row]] != "valid") {
    issues <- c(issues, "The competing-risk outcome is absent or invalid.")
  }
  if (is.na(predictor_row) || anyNA(covariate_rows)) {
    issues <- c(issues, "A predictor or covariate referenced by stable id is absent.")
  }
  if (!is.na(outcome_row) && !is.na(predictor_row) && !anyNA(covariate_rows)) {
    outcome <- outcomes_registry[outcome_row, , drop = FALSE]
    time_name <- registry$name[match(outcome$time_var_id[[1]], registry$var_id)]
    event_name <- registry$name[match(outcome$event_var_id[[1]], registry$var_id)]
    predictor_name <- registry$name[[predictor_row]]
    covariate_names <- registry$name[covariate_rows]
    plan$outcome[[i]] <- outcome$name[[1]]
    plan$predictor[[i]] <- predictor_name
    plan$covariates[[i]] <- covariate_names
    complete <- !special_missing_mask(data[[time_name]]) &
      !special_missing_mask(data[[event_name]]) &
      !special_missing_mask(data[[predictor_name]])
    for (name in covariate_names) complete <- complete & !special_missing_mask(data[[name]])
    event <- analysis_vector(data[[event_name]])[complete]
    if (!any(event == plan$event_cause[[i]])) {
      issues <- c(issues, "The requested Fine-Gray event cause is not observed.")
    }
    if (n_distinct_values(analysis_vector(data[[predictor_name]])[complete]) < 2L) {
      issues <- c(issues, "Predictor has no variation in analyzed data.")
    }
    plan$n_total[[i]] <- nrow(data)
    plan$n_eligible[[i]] <- nrow(data)
    plan$n_analyzed[[i]] <- sum(complete)
    plan$n_missing_outcome[[i]] <- sum(
      special_missing_mask(data[[time_name]]) | special_missing_mask(data[[event_name]])
    )
    plan$n_missing_predictor[[i]] <- sum(special_missing_mask(data[[predictor_name]]))
  }
  missing_packages <- plan$required_packages[[i]][
    !vapply(plan$required_packages[[i]], requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    issues <- c(issues, paste0("Missing required packages: ",
      paste(missing_packages, collapse = ", "), "."))
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

execute_fine_gray <- function(spec, data) {
  registry <- variables(data)
  outcome <- outcomes(data)
  outcome <- outcome[outcome$outcome_id == spec$survival_outcome_id[[1]], , drop = FALSE]
  time_name <- registry$name[match(outcome$time_var_id[[1]], registry$var_id)]
  event_name <- registry$name[match(outcome$event_var_id[[1]], registry$var_id)]
  variable_names <- c(spec$predictor[[1]], spec$covariates[[1]])
  frame <- tibble::as_tibble(data)[c(time_name, event_name, variable_names)]
  complete <- stats::complete.cases(frame)
  frame <- frame[complete, , drop = FALSE]
  formula <- stats::reformulate(variable_names)
  covariates <- stats::model.matrix(formula, frame)[, -1L, drop = FALSE]
  fit <- cmprsk::crr(
    ftime = analysis_vector(frame[[time_name]]),
    fstatus = analysis_vector(frame[[event_name]]), cov1 = covariates,
    failcode = spec$event_cause[[1]], cencode = spec$censor_value[[1]]
  )
  beta <- unname(fit$coef)
  se <- sqrt(diag(fit$var))
  statistic <- beta / se
  critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
  estimate <- beta; low <- beta - critical * se; high <- beta + critical * se
  scale <- "link"
  if (isTRUE(spec$exponentiate[[1]])) {
    estimate <- exp(estimate); low <- exp(low); high <- exp(high); scale <- "ratio"
  }
  list(
    model = fit,
    estimates = tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], stratum_label = spec$stratum_label[[1]],
      transformation_id = NA_character_, transformation_label = NA_character_,
      term = names(fit$coef), level = NA_character_, estimate = estimate,
      std_error = unname(se), std_error_scale = "log_subdistribution_hazard",
      conf_low = low, conf_high = high, statistic = statistic, df = NA_real_,
      p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      effect_measure = spec$effect_measure[[1]], scale = scale,
      n = as.integer(nrow(frame)),
      n_events = as.integer(sum(frame[[event_name]] == spec$event_cause[[1]])),
      method = spec$method[[1]], variance = "model_based"
    ),
    diagnostics = tibble::tibble(
      analysis_id = spec$analysis_id[[1]], metric = "converged",
      value = as.numeric(isTRUE(fit$converged)),
      status = if (isTRUE(fit$converged)) "ok" else "warning",
      message = if (isTRUE(fit$converged)) NA_character_ else "Fine-Gray model did not converge."
    )
  )
}
