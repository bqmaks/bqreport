#' Inspect ordered variable levels
#'
#' Returns the flat registry that stores declared category order. Each row is
#' one level of one variable; `var_id` links it to [variables()], and `position`
#' records the order independently of the current row arrangement.
#'
#' @param data A `bq_data` object.
#'
#' @return A plain tibble with columns `var_id`, `value` and `position`.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(
#'   severity = c("low", "high")
#' ))
#' data <- set_type(data, severity, type_ordinal(c("low", "medium", "high")))
#' variable_levels(data)
variable_levels <- function(data) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  attr(data, "levels")
}
