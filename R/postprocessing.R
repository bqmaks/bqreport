#' Construct explicit model post-processing specifications
#'
#' Post-processing is represented as an ordered, inspectable pipeline. Each
#' step declares its statistical method and all choices which affect the
#' numerical result.
#'
#' @param ... Objects inheriting from `model_postprocessing_step`.
#' @param x A `model_postprocessing_spec`.
#' @param step A `model_postprocessing_step`.
#'
#' @return A `model_postprocessing_spec`.
#' @export
model_postprocessing <- function(...) {
  steps <- list(...)
  validate_postprocessing_steps(steps)
  structure(list(steps = steps), class = "model_postprocessing_spec")
}

#' @rdname model_postprocessing
#' @export
add_postprocessing <- function(x, step) {
  if (!inherits(x, "model_postprocessing_spec")) {
    stop_postprocessing("`x` must be a model_postprocessing_spec.")
  }
  validate_postprocessing_steps(list(step))
  x$steps <- c(x$steps, list(step))
  x
}

#' Attach post-processing to an analysis method
#'
#' This operation does not mutate the original method. The complete pipeline
#' becomes part of the method specification and therefore of its reproducible
#' identity in a compiled plan.
#'
#' @param method A built-in or custom analysis method specification.
#' @param postprocessing A `model_postprocessing_spec`.
#' @return The method with an attached post-processing pipeline.
#' @export
with_postprocessing <- function(method, postprocessing) {
  if (!inherits(method, c("method_spec", "group_comparison_spec"))) {
    stop_postprocessing("`method` must be an analysis method specification.")
  }
  if (!inherits(postprocessing, "model_postprocessing_spec")) {
    stop_postprocessing("`postprocessing` must be a model_postprocessing_spec.")
  }
  method$postprocessing <- postprocessing
  packages <- unlist(lapply(postprocessing$steps, function(step) {
    step$required_packages %||% character()
  }), use.names = FALSE)
  method$required_packages <- unique(c(method$required_packages, packages))
  method$postprocessing_hash <- digest::digest(postprocessing)
  method
}

#' Construct a custom model post-processing step
#'
#' @param id Stable function identifier.
#' @param run Function accepting a read-only post-processing context.
#' @param output Result component populated by the function.
#' @param required_packages Packages required by `run`.
#' @return A `model_postprocessing_step`.
#' @export
postprocessing_function <- function(id, run, output, required_packages = character()) {
  if (missing(id) || !is.character(id) || length(id) != 1L || is.na(id) ||
      !nzchar(id)) {
    stop_postprocessing("`id` must be one non-empty string.")
  }
  if (!is.function(run)) stop_postprocessing("`run` must be a function.")
  if (missing(output) || !is.character(output) || length(output) != 1L ||
      !output %in% c("estimates", "contrasts", "tests", "diagnostics")) {
    stop_postprocessing("`output` must explicitly name a supported result component.")
  }
  if (!is.character(required_packages) || anyNA(required_packages)) {
    stop_postprocessing("`required_packages` must be a character vector.")
  }
  structure(list(
    type = "custom_function", id = id, run = run, output = output,
    required_packages = unique(required_packages),
    function_hash = digest::digest(run)
  ), class = "model_postprocessing_step")
}

#' Construct a pairwise post-hoc step
#'
#' @param method A post-hoc method created by [tukey_test()],
#'   [games_howell_test()], or [dunn_test()].
#' @param comparisons An explicit `contrast_spec` identifying target pairs.
#' @param multiplicity A multiplicity specification created by
#'   [p_adjustment()] or [built_in_multiplicity()].
#'
#' @return A `post_hoc_spec` and `model_postprocessing_step`.
#' @export
pairwise_post_hoc <- function(method, comparisons, multiplicity) {
  if (missing(method) || !inherits(method, "post_hoc_method_spec")) {
    stop_postprocessing("`method` must be an explicit post_hoc_method_spec.")
  }
  if (missing(comparisons) || !inherits(comparisons, "contrast_spec")) {
    stop_postprocessing("`comparisons` must be an explicit contrast_spec.")
  }
  if (!comparisons$type %in% c(
      "against_reference", "all_pairwise", "consecutive"
    )) {
    stop_postprocessing("This target comparison is not supported for pairwise post-hoc tests.")
  }
  if (missing(multiplicity) || !inherits(multiplicity, "multiplicity_spec")) {
    stop_postprocessing("`multiplicity` must be an explicit multiplicity_spec.")
  }
  structure(list(
    type = "pairwise", method = method, comparisons = comparisons,
    multiplicity = multiplicity
  ), class = c("post_hoc_spec", "model_postprocessing_step"))
}

#' Declare that no post-hoc comparisons are requested
#' @return A `post_hoc_spec`.
#' @export
no_post_hoc <- function() {
  structure(list(type = "none"),
    class = c("post_hoc_spec", "model_postprocessing_step"))
}

#' Declare that no omnibus test is requested
#'
#' This is distinct from an omnibus test that was attempted but could not be
#' computed. It lets pairwise comparisons remain an independent analysis step.
#'
#' @return An `omnibus_spec`.
#' @export
no_omnibus <- function() {
  structure(list(type = "none"),
    class = c("omnibus_spec", "model_postprocessing_step"))
}

#' Construct built-in post-hoc method specifications
#' @return A `post_hoc_method_spec`.
#' @export
tukey_test <- function() new_post_hoc_method("tukey", character())

#' @rdname tukey_test
#' @export
games_howell_test <- function() new_post_hoc_method("games_howell", "PMCMRplus")

#' @rdname tukey_test
#' @export
dunn_test <- function() new_post_hoc_method("dunn", "PMCMRplus")

new_post_hoc_method <- function(id, required_packages) {
  structure(list(id = id, required_packages = required_packages),
    class = "post_hoc_method_spec")
}

#' Construct an explicit multiplicity specification
#'
#' @param method A method accepted by [stats::p.adjust()].
#' @return A `multiplicity_spec`.
#' @export
p_adjustment <- function(method) {
  if (missing(method) || !is.character(method) || length(method) != 1L ||
      is.na(method) || !method %in% stats::p.adjust.methods) {
    stop_postprocessing("`method` must be an explicit method supported by stats::p.adjust().")
  }
  structure(list(type = "p_adjust", method = method),
    class = "multiplicity_spec")
}

#' @rdname p_adjustment
#' @export
built_in_multiplicity <- function() {
  structure(list(type = "built_in", method = "built_in"),
    class = "multiplicity_spec")
}

#' @rdname p_adjustment
#' @export
no_adjustment <- function() p_adjustment("none")

#' Construct a confidence-interval post-processing step
#'
#' @param level Confidence level.
#' @param method Explicit interval method identifier.
#' @return A `model_postprocessing_step`.
#' @export
confidence_intervals <- function(level, method) {
  check_confidence_level(level)
  if (missing(method) || !is.character(method) || length(method) != 1L ||
      is.na(method) || !nzchar(method)) {
    stop_postprocessing("`method` must be an explicit non-empty identifier.")
  }
  structure(list(type = "confidence_intervals", level = level, method = method),
    class = "model_postprocessing_step")
}

#' Construct explicit hypothesis specifications
#'
#' @param null Null value on the estimand scale.
#' @param direction Direction in which larger or smaller values support the
#'   alternative.
#' @param alpha One-sided type-I error rate.
#' @return A `hypothesis_spec`.
#' @export
superiority <- function(null, direction, alpha) {
  new_one_sided_hypothesis("superiority", null = null, direction = direction,
    alpha = alpha)
}

#' @rdname superiority
#' @export
two_sided <- function(null, alpha) {
  check_finite_scalar(null, "null")
  check_alpha(alpha)
  structure(list(type = "two_sided", null = null, alpha = alpha),
    class = "hypothesis_spec")
}

#' @rdname superiority
#' @param margin Non-inferiority margin on the declared estimand scale.
#' @export
non_inferiority <- function(margin, direction, alpha) {
  new_one_sided_hypothesis("non_inferiority", margin = margin,
    direction = direction, alpha = alpha)
}

#' @rdname superiority
#' @param lower,upper Lower and upper equivalence margins.
#' @export
equivalence <- function(lower, upper, alpha) {
  check_finite_scalar(lower, "lower")
  check_finite_scalar(upper, "upper")
  check_alpha(alpha)
  if (lower >= upper) stop_postprocessing("`lower` must be less than `upper`.")
  structure(list(type = "equivalence", bounds = c(lower, upper), alpha = alpha),
    class = "hypothesis_spec")
}

new_one_sided_hypothesis <- function(type, null = NULL, margin = NULL,
                                     direction, alpha) {
  value <- if (is.null(null)) margin else null
  check_finite_scalar(value, if (is.null(null)) "margin" else "null")
  if (missing(direction) || !is.character(direction) || length(direction) != 1L ||
      !direction %in% c("greater", "less")) {
    stop_postprocessing("`direction` must be explicitly `greater` or `less`.")
  }
  check_alpha(alpha)
  fields <- list(type = type, direction = direction, alpha = alpha)
  fields[[if (is.null(null)) "margin" else "null"]] <- value
  structure(fields, class = "hypothesis_spec")
}

check_finite_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop_postprocessing(paste0("`", name, "` must be one finite number."))
  }
}

check_alpha <- function(alpha) {
  check_finite_scalar(alpha, "alpha")
  if (alpha <= 0 || alpha >= 0.5) {
    stop_postprocessing("`alpha` must be greater than 0 and less than 0.5.")
  }
}

validate_postprocessing_steps <- function(steps) {
  if (any(!vapply(steps, inherits, logical(1), "model_postprocessing_step"))) {
    stop_postprocessing("Every post-processing item must be a model_postprocessing_step.")
  }
  invisible(steps)
}

stop_postprocessing <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_postprocessing", "error", "condition")))
}
