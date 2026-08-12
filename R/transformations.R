#' Scalar transformation specifications
#' @param increment Positive increment represented by one coefficient unit.
#' @return A `transformation_spec`.
#' @export
per <- function(increment) {
  if (!is.numeric(increment) || length(increment) != 1L || is.na(increment) ||
      !is.finite(increment) || increment <= 0) {
    stop_transformation("`increment` must be one finite positive number.")
  }
  new_transformation_spec(
    paste0("per_", format(increment, scientific = FALSE, trim = TRUE)),
    paste0("per ", format(increment, scientific = FALSE, trim = TRUE), " units"),
    function(x, context) x / increment,
    parameters = list(increment = increment), effect_increment = increment
  )
}

#' @rdname per
#' @export
log2_transform <- function() {
  new_transformation_spec("log2", "per doubling", function(x, context) log2(x),
    parameters = list(base = 2), domain = "positive")
}

#' @rdname per
#' @export
log10_transform <- function() {
  new_transformation_spec("log10", "per ten-fold increase", function(x, context) log10(x),
    parameters = list(base = 10), domain = "positive")
}

#' Construct a custom scalar transformation
#' @param id Stable identifier.
#' @param transform Function accepting `(x, context)` and returning one numeric
#'   vector of the same length.
#' @param label Human-readable effect interpretation.
#' @param parameters Serializable significant settings.
#' @param required_packages Required packages.
#' @return A `transformation_spec`.
#' @export
transformation_function <- function(id, transform, label,
                                    parameters = list(),
                                    required_packages = character()) {
  check_contract_id(id)
  check_contract_id(label, "label")
  if (!is.function(transform)) stop_transformation("`transform` must be a function.")
  new_transformation_spec(id, label, transform, parameters,
    required_packages = required_packages)
}

new_transformation_spec <- function(id, label, transform, parameters = list(),
                                    effect_increment = NA_real_, domain = "any",
                                    required_packages = character()) {
  structure(list(
    id = id, label = label, transform = transform, parameters = parameters,
    effect_increment = effect_increment, domain = domain,
    required_packages = required_packages,
    function_hash = digest::digest(list(transform, parameters))
  ), class = "transformation_spec")
}

#' Assign scalar transformations
#' @param .data A `bq_data` object.
#' @param .cols Numeric predictors or covariates selected with tidyselect.
#' @param transformation A `transformation_spec`.
#' @return Updated `bq_data`.
#' @export
set_transformation <- function(.data, .cols, transformation) {
  check_bq_data(.data)
  if (!inherits(transformation, "transformation_spec")) {
    stop_transformation("`transformation` must be a transformation_spec.")
  }
  selected <- names(tidyselect::eval_select(rlang::enquo(.cols), .data))
  if (any(!vapply(.data[selected], is.numeric, logical(1)))) {
    stop_transformation("Scalar transformations require numeric variables.")
  }
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected, registry$name)
  for (row in rows) registry$transformation[[row]] <- transformation
  attr(.data, "variable_registry") <- registry
  .data
}

apply_transformation_spec <- function(x, spec, variable, analysis_id) {
  if (is.null(spec)) return(x)
  missing_packages <- spec$required_packages[
    !vapply(spec$required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    stop_transformation(paste0("Missing required packages: ", paste(missing_packages, collapse = ", "), "."))
  }
  if (identical(spec$domain, "positive") && any(x <= 0, na.rm = TRUE)) {
    stop_transformation(paste0("Variable `", variable, "` must be strictly positive."))
  }
  context <- list(analysis_id = analysis_id, variable = variable,
    parameters = spec$parameters)
  out <- spec$transform(x, context)
  if (!is.numeric(out) || length(out) != length(x)) {
    stop_transformation("A scalar transformation must return a numeric vector of the same length.")
  }
  if (any(!is.finite(out[!is.na(out)]))) {
    stop_transformation("A transformation returned non-finite values.")
  }
  out
}

stop_transformation <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_transformation", "error", "condition")))
}
