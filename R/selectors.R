registry_select <- function(predicate, fn) {
  data <- tidyselect::peek_data(fn = fn)
  if (!inherits(data, "bq_data")) {
    stop_variable_setting(
      paste0("`", fn, "()` can only select columns from a bq_data object."),
      "bq_error_invalid_selector_context"
    )
  }
  registry <- attr(data, "variable_registry", exact = TRUE)
  registry$name[predicate(registry)]
}

#' Select variables by analytical role
#'
#' These helpers select variables solely from registry metadata. They do not
#' filter variables by validation status and can be composed with ordinary
#' tidyselect expressions.
#'
#' @param role A supported analytical role.
#'
#' @return A tidyselect selection.
#' @export
where_role <- function(role) {
  role <- check_role(role)
  registry_select(
    function(registry) vapply(registry$role, function(x) role %in% x, logical(1)),
    "where_role"
  )
}

#' @rdname where_role
#' @export
all_outcomes <- function() where_role("outcome")

#' @rdname where_role
#' @export
all_predictors <- function() where_role("predictor")

#' @rdname where_role
#' @export
all_groups <- function() where_role("group")

#' Select variables by analytical type
#'
#' @param type One or more supported analytical variable types.
#'
#' @return A tidyselect selection.
#' @export
where_type <- function(type) {
  type <- check_selector_values(type, valid_variable_types, "type")
  registry_select(function(registry) registry$type %in% type, "where_type")
}

#' @rdname where_type
#' @export
where_binary <- function() where_type("binary")

#' @rdname where_type
#' @export
where_continuous <- function() where_type("continuous")

#' @rdname where_type
#' @export
where_ordinal <- function() where_type("ordinal")

#' @rdname where_type
#' @export
where_nominal <- function() where_type("nominal")

#' @rdname where_type
#' @export
where_count <- function() where_type("count")

#' Select variables by registry status
#'
#' @param status One or more registry status values.
#'
#' @return A tidyselect selection.
#' @export
where_status <- function(status) {
  status <- check_selector_values(
    status,
    c("review", "valid", "invalid"),
    "status"
  )
  registry_select(function(registry) registry$status %in% status, "where_status")
}

#' Select variables whose type was inferred
#'
#' @return A tidyselect selection.
#' @export
where_inferred <- function() {
  registry_select(function(registry) registry$source == "inferred", "where_inferred")
}

#' Select variables by distribution profile
#'
#' @param distribution One or more distribution profile identifiers.
#'
#' @return A tidyselect selection.
#' @export
where_distribution <- function(distribution) {
  distribution <- check_selector_values(
    distribution,
    c("gaussian", "skewed"),
    "distribution"
  )
  registry_select(
    function(registry) !is.na(registry$distribution) &
      registry$distribution %in% distribution,
    "where_distribution"
  )
}

#' @rdname where_distribution
#' @export
where_gaussian <- function() where_distribution("gaussian")

#' @rdname where_distribution
#' @export
where_skewed <- function() where_distribution("skewed")

check_selector_values <- function(x, choices, argument) {
  valid <- is.character(x) && length(x) > 0L && !anyNA(x) &&
    all(x %in% choices)
  if (!valid) {
    error_class <- if (identical(argument, "type")) {
      "bq_error_invalid_variable_type"
    } else {
      "bq_error_invalid_selector"
    }
    stop_variable_setting(
      paste0(
        "`", argument, "` must contain only: ",
        paste(choices, collapse = ", "),
        "."
      ),
      error_class
    )
  }
  unique(x)
}
