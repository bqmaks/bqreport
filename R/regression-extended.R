#' Construct robust linear regression
#'
#' Fits an M-estimation linear model with [MASS::rlm()]. Inference uses the
#' coefficient standard errors reported by the backend and a normal
#' approximation.
#'
#' @param psi Robust score function: `huber`, `hampel`, or `bisquare`.
#' @param maxit Maximum number of fitting iterations.
#'
#' @return A concrete `method_spec`.
#' @export
robust_linear_model <- function(
  psi = c("huber", "hampel", "bisquare"), maxit = 20L
) {
  psi <- match.arg(psi)
  if (!is.numeric(maxit) || length(maxit) != 1L || is.na(maxit) ||
      maxit < 1 || maxit != as.integer(maxit)) {
    stop_method_contract("`maxit` must be one positive integer.")
  }
  method <- analysis_function(
    id = "robust_linear_model", effect_measure = "robust_mean_difference",
    scale = "identity", required_packages = "MASS",
    run = function(context) run_robust_linear(context, psi, as.integer(maxit))
  )
  method$estimator <- "m_estimation"
  method$ci_method <- "normal_approximation"
  method$psi <- psi
  method$maxit <- as.integer(maxit)
  method
}

#' Construct conditional quantile regression
#'
#' @param tau One quantile strictly between zero and one.
#' @param se Standard-error method passed to [quantreg::summary.rq()].
#'
#' @return A concrete `method_spec`.
#' @export
quantile_model <- function(tau = 0.5, se = c("nid", "ker", "boot")) {
  if (!is.numeric(tau) || length(tau) != 1L || is.na(tau) ||
      !is.finite(tau) || tau <= 0 || tau >= 1) {
    stop_method_contract("`tau` must be one finite number strictly between zero and one.")
  }
  se <- match.arg(se)
  method <- analysis_function(
    id = "quantile_regression",
    effect_measure = "conditional_quantile_difference", scale = "identity",
    required_packages = "quantreg",
    run = function(context) run_quantile_regression(context, tau, se)
  )
  method$estimator <- "quantile_regression"
  method$ci_method <- paste0("quantreg_", se)
  method$tau <- tau
  method$se <- se
  method
}

#' Construct beta regression
#'
#' The outcome must be continuous and strictly inside `(0, 1)` after missing
#' values are removed.
#'
#' @param link Mean-submodel link accepted by [betareg::betareg()].
#'
#' @return A concrete `method_spec`.
#' @export
beta_model <- function(link = c("logit", "probit", "cloglog", "cauchit", "log", "loglog")) {
  link <- match.arg(link)
  method <- analysis_function(
    id = "beta_regression", effect_measure = "beta_mean_link_coefficient",
    scale = "link", required_packages = "betareg",
    run = function(context) run_beta_regression(context, link)
  )
  method$estimator <- "maximum_likelihood"
  method$ci_method <- "wald"
  method$family <- "beta"
  method$link <- link
  method$link_function <- link
  method
}

#' Construct zero-inflated and hurdle count models
#'
#' Both constructors fit a count component using the planned regression
#' formula and an intercept-only zero component. Component names are retained
#' in tidy coefficient terms.
#'
#' @param distribution Count distribution: `poisson`, `negbin`, or `geometric`.
#'
#' @return A concrete `method_spec`.
#' @export
zero_inflated_model <- function(
  distribution = c("poisson", "negbin", "geometric")
) {
  distribution <- match.arg(distribution)
  count_component_model("zero_inflated", distribution)
}

#' @rdname zero_inflated_model
#' @export
hurdle_model <- function(
  distribution = c("poisson", "negbin", "geometric")
) {
  distribution <- match.arg(distribution)
  count_component_model("hurdle", distribution)
}

count_component_model <- function(type, distribution) {
  id <- if (type == "zero_inflated") "zero_inflated_count" else "hurdle_count"
  method <- analysis_function(
    id = id, effect_measure = "multiplicative_component_coefficient",
    scale = "ratio", model_scale = "link", exponentiate = TRUE,
    required_packages = "pscl",
    run = function(context) run_count_component_model(context, type, distribution)
  )
  method$estimator <- "maximum_likelihood"
  method$ci_method <- "wald"
  method$family <- distribution
  method$link <- "log"
  method$distribution <- distribution
  method$component_type <- type
  method
}

run_robust_linear <- function(context, psi, maxit) {
  psi_function <- switch(
    psi, huber = MASS::psi.huber, hampel = MASS::psi.hampel,
    bisquare = MASS::psi.bisquare
  )
  args <- list(
    formula = context$formula, data = context$model_frame,
    psi = psi_function, maxit = maxit, na.action = stats::na.omit
  )
  if (!is.null(context$weights)) args$weights <- context$weights
  fit <- do.call(MASS::rlm, args)
  coefficients <- summary(fit)$coefficients
  extended_regression_output(
    context, fit, coefficients[, "Value"], coefficients[, "Std. Error"],
    coefficients[, "t value"],
    2 * stats::pnorm(abs(coefficients[, "t value"]), lower.tail = FALSE),
    "robust_mean_difference", "identity", "identity",
    "robust_linear_model", converged = isTRUE(fit$converged)
  )
}

run_quantile_regression <- function(context, tau, se) {
  args <- list(
    formula = context$formula, data = context$model_frame, tau = tau,
    na.action = stats::na.omit
  )
  if (!is.null(context$weights)) args$weights <- context$weights
  fit <- do.call(quantreg::rq, args)
  coefficients <- summary(fit, se = se)$coefficients
  extended_regression_output(
    context, fit, coefficients[, 1L], coefficients[, 2L],
    coefficients[, 3L], coefficients[, 4L],
    "conditional_quantile_difference", "identity", "identity",
    "quantile_regression"
  )
}

run_beta_regression <- function(context, link) {
  args <- list(
    formula = context$formula, data = context$model_frame, link = link,
    na.action = stats::na.omit
  )
  if (!is.null(context$weights)) args$weights <- context$weights
  fit <- do.call(betareg::betareg, args)
  coefficients <- summary(fit)$coefficients$mean
  extended_regression_output(
    context, fit, coefficients[, "Estimate"], coefficients[, "Std. Error"],
    coefficients[, "z value"], coefficients[, "Pr(>|z|)"],
    "beta_mean_link_coefficient", "link", "link", "beta_regression",
    converged = isTRUE(fit$converged)
  )
}

run_count_component_model <- function(context, type, distribution) {
  formula <- stats::as.formula(
    paste(paste(deparse(context$formula), collapse = " "), "| 1"),
    env = environment(context$formula)
  )
  backend <- if (type == "zero_inflated") pscl::zeroinfl else pscl::hurdle
  args <- list(
    formula = formula, data = context$model_frame, dist = distribution,
    na.action = stats::na.omit
  )
  if (!is.null(context$weights)) args$weights <- context$weights
  fit <- do.call(backend, args)
  summary_fit <- summary(fit)
  count <- summary_fit$coefficients$count
  zero <- summary_fit$coefficients$zero
  coefficients <- rbind(count, zero)
  component <- c(rep("count", nrow(count)), rep("zero", nrow(zero)))
  rownames(coefficients) <- paste0(component, ":", rownames(coefficients))
  extended_regression_output(
    context, fit, coefficients[, "Estimate"], coefficients[, "Std. Error"],
    coefficients[, 3L], coefficients[, 4L],
    "multiplicative_component_coefficient", "link", "link",
    if (type == "zero_inflated") "zero_inflated_count" else "hurdle_count",
    converged = isTRUE(fit$optim$convergence == 0L)
  )
}

extended_regression_output <- function(
  context, fit, estimate, std_error, statistic, p_value,
  effect_measure, scale, std_error_scale, method, converged = NA
) {
  critical <- stats::qnorm((1 + context$confidence_level) / 2)
  estimates <- tibble::tibble(
    analysis_id = context$analysis_id,
    outcome = context$outcome_spec$name[[1]],
    predictor = context$predictor_spec$name[[1]],
    term = names(estimate), level = NA_character_,
    estimate = unname(estimate), std_error = unname(std_error),
    std_error_scale = std_error_scale,
    conf_low = unname(estimate - critical * std_error),
    conf_high = unname(estimate + critical * std_error),
    statistic = unname(statistic), df = NA_real_, p_value = unname(p_value),
    effect_measure = effect_measure, scale = scale,
    n = as.integer(tryCatch(
      stats::nobs(fit), error = function(e) nrow(stats::na.omit(context$model_frame))
    )), n_events = NA_integer_, method = method,
    variance = "model_based"
  )
  diagnostics <- if (is.na(converged)) diagnostics_prototype() else {
    tibble::tibble(
      analysis_id = context$analysis_id, metric = "converged",
      value = as.numeric(converged),
      status = if (converged) "ok" else "warning",
      message = if (converged) NA_character_ else "Model did not converge."
    )
  }
  analysis_output(model = fit, estimates = estimates, diagnostics = diagnostics)
}
