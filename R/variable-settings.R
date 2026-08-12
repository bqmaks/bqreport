valid_variable_types <- c(
  "continuous", "binary", "ordinal", "nominal", "count", "date",
  "datetime", "identifier", "unknown"
)

#' Define outcome variables
#'
#' @param .data A `bq_data` object.
#' @param .cols Columns selected using tidyselect syntax.
#' @param type An optional analytical variable type. If omitted, the current
#'   inferred or configured type is retained.
#' @param event An optional event value for binary outcomes.
#'
#' @return `.data` with updated variable metadata.
#' @export
set_outcome <- function(.data, .cols, type = NULL, event = NULL) {
  check_bq_data(.data)
  type <- check_optional_variable_type(type)
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  selected_names <- names(selected)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)

  resolved_types <- if (is.null(type)) registry$type[rows] else rep(type, length(rows))
  if (!is.null(event) && any(resolved_types != "binary")) {
    stop_variable_setting(
      "`event` can only be set for binary outcomes.",
      "bq_error_invalid_outcome"
    )
  }
  if (!is.null(event)) {
    check_scalar_setting(event, "event", "bq_error_invalid_outcome")
  }

  .data <- add_role_by_name(.data, selected_names, "outcome")
  .data <- set_explicit_types(.data, selected_names, type)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)
  if (!is.null(event)) {
    for (row in rows) registry$event_value[[row]] <- event
  }
  attr(.data, "variable_registry") <- registry
  .data
}

#' Define predictor variables
#'
#' @param .data A `bq_data` object.
#' @param .cols Columns selected using tidyselect syntax.
#' @param type An optional analytical variable type. If omitted, the current
#'   inferred or configured type is retained.
#' @param reference An optional reference value for binary, ordinal, or nominal
#'   predictors.
#'
#' @return `.data` with updated variable metadata.
#' @export
set_predictor <- function(.data, .cols, type = NULL, reference = NULL) {
  check_bq_data(.data)
  type <- check_optional_variable_type(type)
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  selected_names <- names(selected)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)

  resolved_types <- if (is.null(type)) registry$type[rows] else rep(type, length(rows))
  categorical_types <- c("binary", "ordinal", "nominal")
  if (!is.null(reference) && any(!resolved_types %in% categorical_types)) {
    stop_variable_setting(
      "`reference` can only be set for binary, ordinal, or nominal predictors.",
      "bq_error_invalid_predictor"
    )
  }
  if (!is.null(reference)) {
    check_scalar_setting(reference, "reference", "bq_error_invalid_predictor")
  }

  .data <- add_role_by_name(.data, selected_names, "predictor")
  .data <- set_explicit_types(.data, selected_names, type)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)
  if (!is.null(reference)) {
    for (row in rows) registry$reference[[row]] <- reference
  }
  attr(.data, "variable_registry") <- registry
  .data
}

add_role_by_name <- function(.data, selected_names, role) {
  update_roles(.data, selected_names, function(roles) {
    unique(c(setdiff(roles, "auxiliary"), role))
  })
}

set_explicit_types <- function(.data, selected_names, type) {
  if (is.null(type)) {
    return(.data)
  }
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)
  registry$type[rows] <- type
  registry$source[rows] <- "explicit"
  registry$status[rows] <- "valid"
  registry$locked[rows] <- TRUE
  attr(.data, "variable_registry") <- registry
  .data
}

check_optional_variable_type <- function(type) {
  if (is.null(type)) {
    return(NULL)
  }
  valid <- is.character(type) && length(type) == 1L && !is.na(type) &&
    type %in% valid_variable_types && type != "unknown"
  if (!valid) {
    stop_variable_setting(
      paste0(
        "`type` must be one of: ",
        paste(setdiff(valid_variable_types, "unknown"), collapse = ", "),
        "."
      ),
      "bq_error_invalid_variable_type"
    )
  }
  type
}

check_scalar_setting <- function(x, argument, class) {
  if (length(x) != 1L || is.na(x)) {
    stop_variable_setting(
      paste0("`", argument, "` must be a single non-missing value."),
      class
    )
  }
  invisible(x)
}

stop_variable_setting <- function(message, class) {
  condition <- structure(
    list(message = message, call = sys.call(-1L)),
    class = c(class, "error", "condition")
  )
  stop(condition)
}
