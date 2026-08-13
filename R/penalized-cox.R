#' Construct cross-validated penalized Cox regression
#'
#' @param alpha Elastic-net mixing parameter between zero and one.
#' @param lambda Selected cross-validation solution: `lambda.min` or
#'   `lambda.1se`.
#' @param nfolds Number of cross-validation folds.
#' @param seed Explicit reproducibility seed.
#'
#' @return A concrete penalized Cox method specification.
#' @export
penalized_cox_model <- function(
  alpha = 1, lambda = c("lambda.min", "lambda.1se"), nfolds = 10L, seed
) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha < 0 || alpha > 1) {
    stop_method_contract("`alpha` must be one number between zero and one.")
  }
  lambda <- match.arg(lambda)
  if (!is.numeric(nfolds) || length(nfolds) != 1L || is.na(nfolds) ||
      nfolds < 3 || nfolds != as.integer(nfolds)) {
    stop_method_contract("`nfolds` must be one integer of at least three.")
  }
  if (missing(seed) || !is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      seed != as.integer(seed)) {
    stop_method_contract("`seed` must be an explicitly supplied integer.")
  }
  method <- new_method_spec(
    "penalized_cox", "glmnet_cox", "penalized_partial_likelihood", "none",
    NA_character_, NA_character_, "penalized_hazard_ratio",
    scale = "ratio", model_scale = "log_hazard", required_packages = c("glmnet", "survival"),
    exponentiate = TRUE
  )
  method$alpha <- alpha; method$lambda <- lambda
  method$nfolds <- as.integer(nfolds); method$seed <- as.integer(seed)
  method
}

#' Compile penalized Cox regression tasks
#'
#' @inheritParams plan_survival
#' @param method A [penalized_cox_model()] specification.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_penalized_cox <- function(
  .data, outcomes = tidyselect::everything(), predictors = all_predictors(),
  covariates = tidyselect::any_of(character()), confidence_level = 0.95,
  method
) {
  if (missing(method) || !inherits(method, "method_spec") ||
      method$engine != "glmnet_cox") {
    stop_method_contract("`method` must be a penalized_cox_model specification.")
  }
  outcomes_quo <- rlang::enquo(outcomes)
  predictors_quo <- rlang::enquo(predictors)
  covariates_quo <- rlang::enquo(covariates)
  plan <- rlang::inject(plan_survival(
    .data, outcomes = !!outcomes_quo, predictors = !!predictors_quo,
    covariates = !!covariates_quo, confidence_level = confidence_level,
    method = cox_model()
  ))
  if (!nrow(plan)) return(plan)
  plan$analysis_type <- "penalized_survival_regression"
  plan$method <- method$method; plan$engine <- method$engine
  plan$estimator <- method$estimator; plan$ci_method <- method$ci_method
  plan$effect_measure <- method$effect_measure; plan$model_scale <- method$model_scale
  plan$scale <- method$scale; plan$exponentiate <- method$exponentiate
  plan$required_packages <- rep(list(method$required_packages), nrow(plan))
  plan$method_object <- rep(list(method), nrow(plan))
  plan$function_id <- NA_character_; plan$function_hash <- NA_character_
  plan$analysis_id <- vapply(seq_len(nrow(plan)), function(i) bq_id(
    "analysis", plan$analysis_id[[i]], method$method, method$alpha,
    method$lambda, method$nfolds, method$seed
  ), character(1))
  new_analysis_plan(plan)
}

execute_penalized_cox <- function(spec, data) {
  registry <- variables(data)
  outcome <- outcomes(data)
  outcome <- outcome[outcome$outcome_id == spec$survival_outcome_id[[1]], , drop = FALSE]
  time_name <- registry$name[match(outcome$time_var_id[[1]], registry$var_id)]
  event_name <- registry$name[match(outcome$event_var_id[[1]], registry$var_id)]
  variable_names <- c(spec$predictor[[1]], spec$covariates[[1]])
  frame <- tibble::as_tibble(data)[c(time_name, event_name, variable_names)]
  for (name in variable_names) {
    row <- match(name, registry$name)
    if (registry$type[[row]] %in% c("binary", "nominal", "ordinal")) {
      frame[[name]] <- stats::relevel(
        factor(frame[[name]]), ref = as.character(registry$reference[[row]])
      )
    }
  }
  frame <- frame[stats::complete.cases(frame), , drop = FALSE]
  matrix <- stats::model.matrix(stats::reformulate(variable_names), frame)[, -1L, drop = FALSE]
  response <- survival::Surv(
    analysis_vector(frame[[time_name]]),
    analysis_vector(frame[[event_name]]) == outcome$event_value[[1]]
  )
  method <- spec$method_object[[1]]
  fit <- with_local_seed(method$seed, glmnet::cv.glmnet(
    matrix, response, family = "cox", alpha = method$alpha,
    nfolds = method$nfolds, cox.ties = "breslow"
  ))
  beta <- as.numeric(stats::coef(fit, s = method$lambda))
  names(beta) <- rownames(stats::coef(fit, s = method$lambda))
  estimate <- if (isTRUE(spec$exponentiate[[1]])) exp(beta) else beta
  selected_lambda <- fit[[method$lambda]]
  list(
    model = fit,
    estimates = tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], stratum_label = spec$stratum_label[[1]],
      transformation_id = NA_character_, transformation_label = NA_character_,
      term = names(beta), level = NA_character_, estimate = unname(estimate),
      std_error = NA_real_, std_error_scale = "log_hazard",
      conf_low = NA_real_, conf_high = NA_real_, statistic = NA_real_, df = NA_real_,
      p_value = NA_real_, effect_measure = spec$effect_measure[[1]],
      scale = spec$scale[[1]], n = as.integer(nrow(frame)),
      n_events = as.integer(sum(response[, "status"])), method = spec$method[[1]],
      variance = "penalized"
    ),
    diagnostics = tibble::tibble(
      analysis_id = spec$analysis_id[[1]],
      metric = c("selected_lambda", "nonzero_coefficients"),
      value = c(selected_lambda, sum(beta != 0)), status = "observed",
      message = NA_character_
    )
  )
}
