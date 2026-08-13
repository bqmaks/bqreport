#' Construct built-in method specifications
#' @param ci_method Confidence interval method.
#' @param exponentiate Whether estimates and compatible contrasts are returned
#'   on the exponentiated ratio scale.
#' @return A concrete `method_spec`.
#' @export
linear_model <- function(ci_method = "t", exponentiate = FALSE) {
  check_exponentiate(exponentiate)
  new_method_spec("linear_model", "lm", "ordinary_least_squares", ci_method,
    "gaussian", "identity",
    if (exponentiate) "exponentiated_coefficient" else "mean_difference",
    exponentiate = exponentiate, model_scale = "identity",
    scale = if (exponentiate) "ratio" else "identity")
}

#' @rdname linear_model
#' @export
logistic_model <- function(ci_method = "wald", exponentiate = TRUE) {
  check_exponentiate(exponentiate)
  new_method_spec("logistic_model", "glm", "maximum_likelihood", ci_method,
    "binomial", "logit", if (exponentiate) "odds_ratio" else "log_odds",
    exponentiate = exponentiate, model_scale = "link",
    scale = if (exponentiate) "ratio" else "link")
}

#' Construct a Poisson count-regression specification
#'
#' @inheritParams linear_model
#' @return A concrete `method_spec` returning rate ratios by default.
#' @export
poisson_model <- function(ci_method = "wald", exponentiate = TRUE) {
  check_exponentiate(exponentiate)
  new_method_spec(
    "poisson_model", "glm", "maximum_likelihood", ci_method,
    "poisson", "log", if (exponentiate) "rate_ratio" else "log_rate",
    exponentiate = exponentiate, model_scale = "link",
    scale = if (exponentiate) "ratio" else "link"
  )
}

#' Construct a negative-binomial count-regression specification
#'
#' @inheritParams linear_model
#' @return A concrete custom `method_spec` using the optional `MASS` backend.
#' @export
negative_binomial_model <- function(ci_method = "wald", exponentiate = TRUE) {
  check_exponentiate(exponentiate)
  analysis_function(
    id = "negative_binomial_model",
    effect_measure = if (exponentiate) "rate_ratio" else "log_rate",
    scale = if (exponentiate) "ratio" else "link", model_scale = "link",
    exponentiate = exponentiate, required_packages = "MASS",
    run = function(context) run_negative_binomial(context)
  )
}

run_negative_binomial <- function(context) {
  fit_args <- list(
    formula = context$formula, data = context$model_frame,
    na.action = stats::na.omit
  )
  if (!is.null(context$weights)) fit_args$weights <- context$weights
  fit <- do.call(MASS::glm.nb, fit_args)
  coefficients <- summary(fit)$coefficients
  beta <- unname(coefficients[, "Estimate"])
  standard_error <- unname(coefficients[, "Std. Error"])
  critical <- stats::qnorm((1 + context$confidence_level) / 2)
  null_args <- list(
    formula = stats::reformulate(
      character(), response = context$outcome_spec$name[[1]]
    ),
    data = context$model_frame, na.action = stats::na.omit
  )
  if (!is.null(context$weights)) null_args$weights <- context$weights
  null_fit <- do.call(MASS::glm.nb, null_args)
  statistic <- 2 * (stats::logLik(fit) - stats::logLik(null_fit))
  degrees <- attr(stats::logLik(fit), "df") - attr(stats::logLik(null_fit), "df")
  analysis_output(
    model = fit,
    estimates = tibble::tibble(
      analysis_id = context$analysis_id, outcome = context$outcome_spec$name[[1]],
      predictor = context$predictor_spec$name[[1]], term = rownames(coefficients),
      level = NA_character_, estimate = beta, std_error = standard_error,
      std_error_scale = "log_rate", conf_low = beta - critical * standard_error,
      conf_high = beta + critical * standard_error,
      statistic = beta / standard_error, df = NA_real_,
      p_value = 2 * stats::pnorm(abs(beta / standard_error), lower.tail = FALSE),
      effect_measure = "rate_ratio", scale = "link",
      n = as.integer(stats::nobs(fit)), n_events = NA_integer_,
      method = "negative_binomial_model", variance = "model_based"
    ),
    tests = tibble::tibble(
      analysis_id = context$analysis_id, outcome = context$outcome_spec$name[[1]],
      predictor = context$predictor_spec$name[[1]],
      contrast = NA_character_, numerator = NA_character_, denominator = NA_character_,
      test = "likelihood_ratio",
      statistic = as.numeric(statistic), df = as.numeric(degrees),
      p_value = stats::pchisq(statistic, degrees, lower.tail = FALSE),
      p_adjusted = NA_real_, adjust_method = "none",
      method = "negative_binomial_model"
    ),
    diagnostics = tibble::tibble(
      analysis_id = context$analysis_id,
      metric = c("theta", "converged"), value = c(fit$theta, as.numeric(fit$converged)),
      status = "observed", message = NA_character_
    )
  )
}

#' Construct Firth penalized logistic regression
#'
#' @param exponentiate Whether coefficients and confidence limits are returned
#'   as odds ratios rather than log odds.
#' @return A concrete custom `method_spec` using the optional `logistf` backend.
#' @export
firth_logistic <- function(exponentiate = TRUE) {
  check_exponentiate(exponentiate)
  analysis_function(
    id = "firth_logistic",
    effect_measure = if (exponentiate) "odds_ratio" else "log_odds",
    scale = if (exponentiate) "ratio" else "link",
    model_scale = "link",
    exponentiate = exponentiate,
    required_packages = "logistf",
    run = function(context) run_firth_logistic(context, exponentiate)
  )
}

#' Select ordinary or Firth logistic regression before fitting
#'
#' The selector uses `detectseparation` during preflight. Finite maximum
#' likelihood estimates select `logistic_model()`; complete or quasi-complete
#' separation selects `firth_logistic()`. The selected method is fixed in the
#' validated plan.
#'
#' @param exponentiate Whether either candidate returns odds ratios.
#' @param id Stable selector identifier.
#' @return A `method_selector` with `glm` and `firth` candidates.
#' @export
separation_logistic <- function(exponentiate = TRUE,
                                id = "separation_glm_or_firth") {
  check_exponentiate(exponentiate)
  method_selector(
    id = id,
    candidates = list(
      glm = logistic_model(exponentiate = exponentiate),
      firth = firth_logistic(exponentiate = exponentiate)
    ),
    required_packages = "detectseparation",
    select = function(context) {
      args <- list(
        formula = context$formula,
        data = context$model_frame,
        family = stats::binomial("logit"),
        method = detectseparation::detect_separation
      )
      if (!is.null(context$weights)) args$weights <- context$weights
      detection <- do.call(stats::glm, args)
      separated <- any(is.infinite(stats::coef(detection)))
      method_choice(
        method = if (separated) "firth" else "glm",
        reason = if (separated) {
          "Detected complete or quasi-complete separation."
        } else {
          "Maximum-likelihood estimates are finite."
        },
        diagnostics = tibble::tibble(separation = separated)
      )
    }
  )
}

run_firth_logistic <- function(context, exponentiate) {
  args <- list(
    formula = context$formula,
    data = context$model_frame,
    alpha = 1 - context$confidence_level,
    na.action = stats::na.omit
  )
  if (!is.null(context$weights)) args$weights <- context$weights
  fit <- do.call(logistf::logistf, args)
  coefficient <- unname(fit$coefficients)
  standard_error <- sqrt(diag(fit$var))
  terms <- names(fit$coefficients)
  output_measure <- if (exponentiate) {
    "odds_ratio"
  } else {
    "log_odds"
  }
  # Output stays on the link scale; the common normalization layer performs
  # optional exponentiation according to the selected method specification.
  analysis_output(
    model = fit,
    estimates = tibble::tibble(
      analysis_id = context$analysis_id,
      outcome = context$outcome_spec$name[[1]],
      predictor = context$predictor_spec$name[[1]],
      term = terms,
      level = NA_character_,
      estimate = coefficient,
      std_error = unname(standard_error),
      std_error_scale = "link",
      conf_low = unname(fit$ci.lower),
      conf_high = unname(fit$ci.upper),
      statistic = coefficient / unname(standard_error),
      df = NA_real_,
      p_value = unname(fit$prob),
      effect_measure = output_measure,
      scale = "link",
      n = as.integer(fit$n),
      n_events = as.integer(sum(context$response == 1, na.rm = TRUE)),
      method = "firth_logistic",
      variance = "model_based"
    )
  )
}

new_method_spec <- function(id, engine, estimator, ci_method, family, link,
                            effect_measure, run = NULL, scale = NULL,
                            required_packages = "stats", exponentiate = FALSE,
                            model_scale = scale) {
  structure(list(
    method = id, engine = engine, estimator = estimator, ci_method = ci_method,
    family = family, link = link, effect_measure = effect_measure,
    selection_reason = paste0("Selected concrete method `", id, "`."),
    run = run, scale = scale, model_scale = model_scale,
    exponentiate = exponentiate, required_packages = required_packages,
    function_id = if (is.null(run)) NA_character_ else id,
    function_hash = if (is.null(run)) NA_character_ else digest::digest(run)
  ), class = "method_spec")
}

#' Construct an atomic custom analysis function
#' @param id Stable method identifier.
#' @param run Function accepting a read-only `analysis_context` and returning
#'   an [analysis_output()].
#' @param effect_measure Declared effect measure.
#' @param scale Declared estimate scale.
#' @param required_packages Packages required before execution.
#' @param exponentiate Whether the normalization layer exponentiates estimates,
#'   confidence limits, and contrasts returned on `model_scale`.
#' @param model_scale Scale returned by the custom callbacks before optional
#'   exponentiation.
#' @return A concrete custom `method_spec`.
#' @export
analysis_function <- function(id, run, effect_measure, scale,
                              required_packages = character(),
                              exponentiate = FALSE, model_scale = scale) {
  check_contract_id(id)
  if (!is.function(run)) stop_method_contract("`run` must be a function.")
  check_contract_id(effect_measure, "effect_measure")
  check_contract_id(scale, "scale")
  check_exponentiate(exponentiate)
  new_method_spec(id, "custom_function", "custom", "custom", NA_character_,
    NA_character_, effect_measure, run, scale, required_packages,
    exponentiate, model_scale)
}

#' Construct a structured custom analysis method
#' @param id Stable method identifier.
#' @param fit Fit callback.
#' @param tidy_estimates Estimate callback.
#' @param tidy_tests Test callback.
#' @param compute_contrasts Contrast callback.
#' @param diagnose Diagnostic callback.
#' @param effect_measure Declared effect measure.
#' @param scale Declared estimate scale.
#' @param required_packages Required packages.
#' @param exponentiate Whether to exponentiate normalized outputs.
#' @param model_scale Scale returned by callbacks before transformation.
#' @return A custom `method_spec`.
#' @export
analysis_method <- function(id, fit, tidy_estimates, tidy_tests = NULL,
                            compute_contrasts = NULL, diagnose = NULL,
                            effect_measure, scale,
                            required_packages = character(),
                            exponentiate = FALSE, model_scale = scale) {
  callbacks <- list(fit = fit, tidy_estimates = tidy_estimates,
    tidy_tests = tidy_tests, compute_contrasts = compute_contrasts,
    diagnose = diagnose)
  if (!is.function(fit) || !is.function(tidy_estimates) ||
      any(vapply(callbacks[!vapply(callbacks, is.null, logical(1))],
        function(x) !is.function(x), logical(1)))) {
    stop_method_contract("Method callbacks must be functions or NULL.")
  }
  runner <- function(context) {
    model <- fit(context)
    analysis_output(
      model = model,
      estimates = tidy_estimates(model, context),
      tests = if (is.null(tidy_tests)) NULL else tidy_tests(model, context),
      contrasts = if (is.null(compute_contrasts)) NULL else compute_contrasts(model, context),
      diagnostics = if (is.null(diagnose)) NULL else diagnose(model, context)
    )
  }
  analysis_function(id, runner, effect_measure, scale, required_packages,
    exponentiate, model_scale)
}

check_exponentiate <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop_method_contract("`exponentiate` must be TRUE or FALSE.")
  }
  invisible(x)
}

#' Construct normalized output from a custom engine
#' @param model Optional fitted model.
#' @param estimates,tests,contrasts,diagnostics,issues Optional tidy components.
#' @return An `analysis_output` object.
#' @export
analysis_output <- function(model = NULL, estimates = NULL, tests = NULL,
                            contrasts = NULL, diagnostics = NULL, issues = NULL) {
  structure(list(model = model, estimates = estimates, tests = tests,
    contrasts = contrasts, diagnostics = diagnostics, issues = issues),
    class = "analysis_output")
}

#' Construct a data-dependent method selector
#'
#' A selector is evaluated once during [validate_plan()] and must choose one of
#' its explicitly named candidate methods. It is never used as a runtime
#' fallback.
#'
#' @param id Stable selector identifier.
#' @param candidates Named list of concrete `method_spec` objects.
#' @param select Function accepting an `analysis_context` and returning
#'   `method_choice()`.
#' @param required_packages Packages needed to evaluate the selector.
#' @return A `method_selector` specification.
#' @export
method_selector <- function(id, candidates, select,
                            required_packages = character()) {
  check_contract_id(id)
  if (!is.list(candidates) || length(candidates) == 0L ||
      is.null(names(candidates)) || any(!nzchar(names(candidates))) ||
      anyDuplicated(names(candidates))) {
    stop_method_contract("`candidates` must be a non-empty, uniquely named list.")
  }
  if (!all(vapply(candidates, inherits, logical(1), "method_spec"))) {
    stop_method_contract("Every candidate must be a concrete method_spec.")
  }
  if (!is.function(select)) stop_method_contract("`select` must be a function.")
  if (!is.character(required_packages) || anyNA(required_packages)) {
    stop_method_contract("`required_packages` must be a character vector.")
  }
  structure(list(
    id = id, candidates = candidates, select = select,
    required_packages = unique(required_packages),
    function_hash = digest::digest(select)
  ), class = "method_selector")
}

#' Record a method selector decision
#'
#' @param method Name of one candidate declared in `method_selector()`.
#' @param reason Human-readable basis for the decision.
#' @param diagnostics A tidy data frame of unrounded pre-fit diagnostics.
#' @return A `method_choice` object.
#' @export
method_choice <- function(method, reason, diagnostics = tibble::tibble()) {
  if (!is.character(method) || length(method) != 1L || is.na(method) || !nzchar(method)) {
    stop_method_choice("`method` must be one non-empty candidate name.")
  }
  if (!is.character(reason) || length(reason) != 1L || is.na(reason) || !nzchar(reason)) {
    stop_method_choice("`reason` must be one non-empty string.")
  }
  if (!inherits(diagnostics, "data.frame")) {
    stop_method_choice("`diagnostics` must be a data frame.")
  }
  structure(list(
    method = method, reason = reason,
    diagnostics = tibble::as_tibble(diagnostics)
  ), class = "method_choice")
}

#' Define analysis method rules
#' @param ... Two-sided formulas of selector to concrete method specification.
#' @return An `analysis_rules` object.
#' @export
analysis_rules <- function(...) {
  formulas <- list(...)
  valid <- vapply(formulas, inherits, logical(1), "formula")
  if (!all(valid)) stop_method_contract("Every rule must be a two-sided formula.")
  rules <- lapply(seq_along(formulas), function(i) {
    formula <- formulas[[i]]
    method <- eval(formula[[3]], environment(formula))
    if (!inherits(method, c("method_spec", "method_selector"))) {
      stop_method_contract("Rule RHS must return a method_spec or method_selector.")
    }
    list(id = paste0("rule_", i), selector = formula[[2]],
      environment = environment(formula), method = method)
  })
  structure(rules, class = "analysis_rules")
}

resolve_method_rule <- function(rules, data, outcome_name) {
  if (is.null(rules)) return(NULL)
  matches <- vapply(rules, function(rule) {
    selected <- tidyselect::eval_select(
      rlang::new_quosure(rule$selector, rule$environment), data
    )
    outcome_name %in% names(selected)
  }, logical(1))
  if (sum(matches) > 1L) {
    stop_plan(paste0("Multiple rules match outcome `", outcome_name, "`."),
      "bq_error_ambiguous_rule")
  }
  if (!any(matches)) return(NULL)
  rules[[which(matches)]]
}

check_contract_id <- function(x, argument = "id") {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop_method_contract(paste0("`", argument, "` must be one non-empty string."))
  }
}

stop_method_contract <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_method_contract", "error", "condition")))
}

stop_method_choice <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_method_choice", "error", "condition")))
}
