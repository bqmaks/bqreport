#' Configure a regression weight
#' @param .data A `bq_data` object.
#' @param .cols Exactly one weight column selected with tidyselect.
#' @param type Weight semantics: frequency, precision, or IPW.
#' @return Updated `bq_data`.
#' @export
set_weight <- function(.data, .cols, type = c("ipw", "frequency", "precision")) {
  check_bq_data(.data)
  type <- match.arg(type)
  selected <- names(tidyselect::eval_select(rlang::enquo(.cols), .data))
  if (length(selected) != 1L) {
    stop_variable_setting("`set_weight()` requires exactly one column.", "bq_error_invalid_weight")
  }
  if (!is.numeric(.data[[selected]])) {
    stop_variable_setting("A weight column must be numeric.", "bq_error_invalid_weight")
  }
  .data <- add_role_by_name(.data, selected, "weight")
  registry <- attr(.data, "variable_registry", exact = TRUE)
  row <- match(selected, registry$name)
  registry$weight_type[[row]] <- type
  attr(.data, "variable_registry") <- registry
  .data
}
