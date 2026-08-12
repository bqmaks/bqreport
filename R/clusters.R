#' Configure an observation cluster
#' @param .data A `bq_data` object.
#' @param .cols Exactly one cluster identifier selected with tidyselect.
#' @param type Cluster semantics. Currently matched sets are supported.
#' @return Updated `bq_data`.
#' @export
set_cluster <- function(.data, .cols, type = "matched_set") {
  check_bq_data(.data)
  if (!identical(type, "matched_set")) {
    stop_variable_setting("Only `matched_set` clusters are supported.", "bq_error_invalid_cluster")
  }
  selected <- names(tidyselect::eval_select(rlang::enquo(.cols), .data))
  if (length(selected) != 1L) {
    stop_variable_setting("`set_cluster()` requires exactly one column.", "bq_error_invalid_cluster")
  }
  .data <- add_role_by_name(.data, selected, "cluster")
  registry <- attr(.data, "variable_registry", exact = TRUE)
  row <- match(selected, registry$name)
  registry$cluster_type[[row]] <- type
  attr(.data, "variable_registry") <- registry
  .data
}
