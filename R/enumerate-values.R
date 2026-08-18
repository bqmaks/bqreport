#' Enumerate values in small summary cells
#'
#' Declares a presentation rule that shows the observed values when a variable
#' has at most `max_n` non-missing values in a summary cell. Statistics may be
#' hidden or retained alongside the values, but the rule never changes which
#' statistics are computed.
#'
#' @param max_n Maximum number of non-missing values to enumerate. Must be one
#'   positive whole number no greater than `.Machine$integer.max`.
#' @param display_statistics Whether descriptive statistics should be displayed
#'   alongside enumerated values when the rule applies.
#'
#' @return A `bq_enumerate_values` display-rule specification.
#' @export
#' @examples
#' enumerate_values()
#' enumerate_values(max_n = 3, display_statistics = TRUE)
enumerate_values <- function(max_n = 2L, display_statistics = FALSE) {
  if (
    !is.numeric(max_n) || length(max_n) != 1L || is.na(max_n) ||
      !is.finite(max_n) || max_n <= 0 || max_n != floor(max_n) ||
      max_n > .Machine$integer.max
  ) {
    bq_abort(
      "bq_error_invalid_display_rule",
      paste0(
        "`max_n` must be one positive whole number no greater than ",
        ".Machine$integer.max."
      )
    )
  }

  if (
    !is.logical(display_statistics) || length(display_statistics) != 1L ||
      is.na(display_statistics)
  ) {
    bq_abort(
      "bq_error_invalid_display_rule",
      "`display_statistics` must be either TRUE or FALSE."
    )
  }

  structure(
    list(
      kind = "enumerate_values",
      max_n = as.integer(max_n),
      display_statistics = display_statistics
    ),
    class = c("bq_enumerate_values", "bq_display_rule")
  )
}
