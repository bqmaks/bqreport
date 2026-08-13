# Tidy accessors for analysis_result components.

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
attempts <- function(x) {
  check_analysis_result(x)
  x$attempts
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

#' @rdname estimates
#' @export
survival_estimates <- function(x) {
  check_analysis_result(x)
  x$survival_estimates
}

#' @rdname estimates
#' @export
omnibus_effects <- function(x) {
  check_analysis_result(x)
  x$omnibus_effects
}

#' @rdname estimates
#' @export
correlations <- function(x) {
  check_analysis_result(x)
  x$correlations
}
