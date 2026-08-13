#' Configure a nonlinear model term
#'
#' Model terms expand one covariate into a fixed basis during preflight. They
#' are distinct from scalar transformations and are currently supported only
#' for adjustment covariates.
#'
#' @param .data A `bq_data` object.
#' @param .cols Numeric columns selected with tidyselect.
#' @param term A `model_term_spec`.
#'
#' @return Updated `bq_data`.
#' @export
set_model_term <- function(.data, .cols, term) {
  check_bq_data(.data)
  if (!inherits(term, "model_term_spec")) {
    stop_invalid_model_term("`term` must be a model_term_spec.")
  }
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(names(selected), registry$name)
  eligible <- registry$type[rows] %in% c("continuous", "count") |
    registry$storage_type[rows] %in% c("integer", "double")
  if (any(!eligible)) {
    stop_invalid_model_term("Model terms require continuous or count variables.")
  }
  for (row in rows) registry$model_term[row] <- list(term)
  attr(.data, "variable_registry") <- registry
  .data
}

#' Construct a polynomial covariate term
#'
#' @param degree Positive polynomial degree.
#' @param raw Whether to use raw rather than orthogonal polynomials.
#'
#' @return A `model_term_spec`.
#' @export
polynomial_term <- function(degree, raw = FALSE) {
  if (!is.numeric(degree) || length(degree) != 1L || is.na(degree) ||
      degree < 1 || degree != floor(degree)) {
    stop_invalid_model_term("`degree` must be one positive whole number.")
  }
  if (!is.logical(raw) || length(raw) != 1L || is.na(raw)) {
    stop_invalid_model_term("`raw` must be TRUE or FALSE.")
  }
  new_model_term_spec(
    "polynomial", list(degree = as.integer(degree), raw = raw)
  )
}

#' Construct a natural spline covariate term
#'
#' @param df Optional degrees of freedom, at least two.
#' @param knots Optional strictly increasing interior knots.
#' @param boundary_knots Optional two strictly increasing boundary knots.
#'
#' @return A `model_term_spec`.
#' @export
natural_spline <- function(df = NULL, knots = NULL, boundary_knots = NULL) {
  if (!is.null(df)) {
    valid_df <- is.numeric(df) && length(df) == 1L && !is.na(df) &&
      df >= 2 && df == floor(df)
    if (!valid_df) stop_invalid_model_term("`df` must be a whole number of at least two.")
    df <- as.integer(df)
  }
  knots <- validate_knot_vector(knots, "knots", minimum_length = 1L)
  boundary_knots <- validate_knot_vector(
    boundary_knots, "boundary_knots", minimum_length = 2L, exact_length = 2L
  )
  if (is.null(df) && is.null(knots)) {
    stop_invalid_model_term("Supply either `df` or explicit `knots`.")
  }
  new_model_term_spec(
    "natural_spline",
    list(df = df, knots = knots, boundary_knots = boundary_knots),
    required_packages = "splines"
  )
}

new_model_term_spec <- function(id, parameters, required_packages = character()) {
  structure(list(
    id = id, parameters = parameters, resolved_parameters = NULL,
    output_names = character(), required_packages = required_packages,
    function_hash = NA_character_
  ), class = "model_term_spec")
}

validate_knot_vector <- function(x, argument, minimum_length, exact_length = NULL) {
  if (is.null(x)) return(NULL)
  valid <- is.numeric(x) && !anyNA(x) && all(is.finite(x)) &&
    length(x) >= minimum_length && !is.unsorted(x, strictly = TRUE)
  if (!is.null(exact_length)) valid <- valid && length(x) == exact_length
  if (!valid) {
    stop_invalid_model_term(paste0(
      "`", argument, "` must contain strictly increasing finite values."
    ))
  }
  as.numeric(x)
}

stop_invalid_model_term <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_model_term", "error", "condition")
  ))
}

resolve_model_term_spec <- function(term, values, variable_name, analysis_id) {
  if (is.null(term)) return(NULL)
  observed <- values[!is.na(values)]
  if (!is.numeric(observed) || length(observed) == 0L) {
    stop_invalid_model_term(paste0(
      "Model term for `", variable_name, "` has no numeric observations."
    ))
  }
  resolved <- term
  if (term$id == "polynomial") {
    degree <- term$parameters$degree
    if (length(unique(observed)) <= degree) {
      stop_invalid_model_term(paste0(
        "Polynomial degree for `", variable_name,
        "` must be smaller than the number of distinct values."
      ))
    }
    basis <- stats::poly(observed, degree = degree, raw = term$parameters$raw)
    resolved$resolved_parameters <- list(
      degree = degree, raw = term$parameters$raw,
      coefs = if (term$parameters$raw) NULL else attr(basis, "coefs")
    )
    width <- degree
  } else if (term$id == "natural_spline") {
    arguments <- list(
      x = observed, df = term$parameters$df, knots = term$parameters$knots,
      Boundary.knots = term$parameters$boundary_knots, intercept = FALSE
    )
    arguments <- arguments[!vapply(arguments, is.null, logical(1))]
    basis <- do.call(splines::ns, arguments)
    resolved$resolved_parameters <- list(
      knots = unname(attr(basis, "knots")),
      boundary_knots = unname(attr(basis, "Boundary.knots")),
      intercept = FALSE
    )
    width <- ncol(basis)
  } else {
    stop_invalid_model_term(paste0("Unknown model term `", term$id, "`."))
  }
  safe_name <- gsub("[^A-Za-z0-9_]", "_", variable_name)
  resolved$output_names <- paste0("..bq_", safe_name, "_basis_", seq_len(width))
  resolved$analysis_id <- analysis_id
  resolved
}

apply_model_term_spec <- function(values, term) {
  if (term$id == "polynomial") {
    parameters <- term$resolved_parameters
    return(stats::poly(
      values, degree = parameters$degree, raw = parameters$raw,
      coefs = parameters$coefs, simple = TRUE
    ))
  }
  if (term$id == "natural_spline") {
    parameters <- term$resolved_parameters
    return(splines::ns(
      values, knots = parameters$knots,
      Boundary.knots = parameters$boundary_knots,
      intercept = parameters$intercept
    ))
  }
  stop_invalid_model_term(paste0("Unknown model term `", term$id, "`."))
}

model_formula_covariates <- function(covariate_ids, covariate_names, specs) {
  unlist(Map(function(id, name) {
    term <- specs[[id]]
    if (is.null(term)) name else term$output_names
  }, covariate_ids, covariate_names), use.names = FALSE)
}
