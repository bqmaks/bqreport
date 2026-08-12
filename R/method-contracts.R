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
    if (!inherits(method, "method_spec")) stop_method_contract("Rule RHS must return a method_spec.")
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
