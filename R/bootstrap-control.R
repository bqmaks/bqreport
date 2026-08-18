#' Configure bootstrap confidence intervals
#'
#' Declares a reproducible nonparametric bootstrap configuration. The
#' configuration is computationally inert: bootstrap resampling is performed
#' only when an analytic function consumes it.
#'
#' Ordinary bootstrap uses [boot::boot()]. Fractional weighted bootstrap uses
#' `fwb::fwb()` and requires an analytic function that explicitly supports
#' weighted estimation. The selected engine is recorded in the specification.
#'
#' @param method Bootstrap method: `"ordinary"` for resampling observations or
#'   `"fractional"` for fractional exponential weights.
#' @param iterations Number of bootstrap resamples. Must be one positive whole
#'   number no greater than `.Machine$integer.max`.
#' @param conf_type Confidence-interval method. One of `"bca"`, `"percentile"`
#'   and `"basic"`.
#' @param seed Integer seed used to reproduce the bootstrap resampling, or
#'   `NULL` to use the current random-number stream. Must be a non-negative
#'   whole number no greater than `.Machine$integer.max` when supplied.
#' @param weight_type Fractional-weight distribution. Must be `NULL` for an
#'   ordinary bootstrap. For a fractional bootstrap, `NULL` selects
#'   `"exponential"`, which is currently the only supported distribution.
#'
#' @return A `bq_bootstrap_control` specification.
#' @export
#' @examples
#' bootstrap_control()
#' bootstrap_control(method = "fractional", iterations = 5000, seed = 2026)
bootstrap_control <- function(
  method = "ordinary",
  iterations = 2000L,
  conf_type = "bca",
  seed = NULL,
  weight_type = NULL
) {
  if (
    !is.character(method) || length(method) != 1L || is.na(method) ||
      !method %in% c("ordinary", "fractional")
  ) {
    bq_abort(
      "bq_error_invalid_bootstrap_control",
      "`method` must be either \"ordinary\" or \"fractional\"."
    )
  }

  if (
    !is.numeric(iterations) || length(iterations) != 1L ||
      is.na(iterations) || !is.finite(iterations) || iterations <= 0 ||
      iterations != floor(iterations) || iterations > .Machine$integer.max
  ) {
    bq_abort(
      "bq_error_invalid_bootstrap_control",
      paste0(
        "`iterations` must be one positive whole number no greater than ",
        ".Machine$integer.max."
      )
    )
  }

  allowed_conf_types <- c("bca", "percentile", "basic")
  if (
    !is.character(conf_type) || length(conf_type) != 1L ||
      is.na(conf_type) || !conf_type %in% allowed_conf_types
  ) {
    bq_abort(
      "bq_error_invalid_bootstrap_control",
      paste0(
        "`conf_type` must be one of \"bca\", \"percentile\" and \"basic\"."
      )
    )
  }

  if (
    !is.null(seed) && (
      !is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
        !is.finite(seed) || seed < 0 || seed != floor(seed) ||
        seed > .Machine$integer.max
    )
  ) {
    bq_abort(
      "bq_error_invalid_bootstrap_control",
      paste0(
        "`seed` must be NULL or one non-negative whole number no greater ",
        "than .Machine$integer.max."
      )
    )
  }

  if (method == "ordinary" && !is.null(weight_type)) {
    bq_abort(
      "bq_error_invalid_bootstrap_control",
      "`weight_type` must be NULL for an ordinary bootstrap."
    )
  }
  if (method == "fractional") {
    if (is.null(weight_type)) {
      weight_type <- "exponential"
    } else if (
      !is.character(weight_type) || length(weight_type) != 1L ||
        is.na(weight_type) || !identical(weight_type, "exponential")
    ) {
      bq_abort(
        "bq_error_invalid_bootstrap_control",
        paste0(
          "`weight_type` must be NULL or \"exponential\" for a fractional ",
          "bootstrap."
        )
      )
    }
  }

  structure(
    list(
      method = method,
      engine = if (method == "ordinary") "boot" else "fwb",
      iterations = as.integer(iterations),
      conf_type = conf_type,
      seed = if (is.null(seed)) NULL else as.integer(seed),
      weight_type = weight_type
    ),
    class = "bq_bootstrap_control"
  )
}
