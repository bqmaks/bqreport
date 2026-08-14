#' Signal a typed bqreport error
#'
#' Every error raised by the package carries a specific class (`bq_error_*`)
#' together with the shared `bq_error` class, so a caller can catch either one
#' failure mode or all failures coming from bqreport.
#'
#' @param class Character scalar: the specific condition class, for example
#'   `"bq_error_invalid_data"`.
#' @param message Character scalar shown to the user.
#' @param ... Extra named fields stored on the condition object, for later
#'   programmatic inspection.
#'
#' @return Never returns; always raises an error.
#' @noRd
bq_abort <- function(class, message, ...) {
  condition <- structure(
    list(message = message, call = sys.call(-1L), ...),
    class = c(class, "bq_error", "error", "condition")
  )
  stop(condition)
}
