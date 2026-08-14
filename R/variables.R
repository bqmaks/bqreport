#' Read the variable registry
#'
#' Returns the analytic properties recorded for each column: one row per
#' column, in column order. The result is a plain tibble, so it can be
#' explored with the usual dplyr verbs.
#'
#' This is the supported way to read the registry; the attribute it is stored
#' in is an implementation detail.
#'
#' @param data A `bq_data` object.
#'
#' @return A tibble with columns `var_id`, `name`, `label`, `role` and `type`.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55), sex = c("f", "m")))
#' variables(data)
variables <- function(data) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  attr(data, "variables")
}
