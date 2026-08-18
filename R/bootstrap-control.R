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
  class <- "bq_error_invalid_bootstrap_control"
  check_choice(method, "method", c("ordinary", "fractional"), class = class)
  iterations <- check_whole(iterations, "iterations", lower = 1, class = class)
  check_choice(
    conf_type, "conf_type", c("bca", "percentile", "basic"), class = class
  )
  if (!is.null(seed)) {
    seed <- check_whole(seed, "seed", lower = 0, class = class)
  }

  if (method == "ordinary" && !is.null(weight_type)) {
    bq_abort(class, "`weight_type` must be NULL for an ordinary bootstrap.")
  }
  if (method == "fractional") {
    if (is.null(weight_type)) {
      weight_type <- "exponential"
    } else if (!identical(weight_type, "exponential")) {
      bq_abort(
        class,
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
      iterations = iterations,
      conf_type = conf_type,
      seed = seed,
      weight_type = weight_type
    ),
    class = "bq_bootstrap_control"
  )
}

#' Require a valid bootstrap specification and its engine
#'
#' Analytic constructors accept `bootstrap = NULL` or a specification built by
#' [bootstrap_control()]. The structure is re-checked here rather than trusted
#' from the class alone, because a specification is plain data that a user can
#' edit after construction. The engine package is required at declaration
#' time: a missing engine is a configuration error, not a runtime surprise.
#'
#' @param bootstrap `NULL` or a `bq_bootstrap_control` object.
#'
#' @return `bootstrap`, invisibly.
#' @noRd
check_bootstrap_control <- function(bootstrap) {
  if (is.null(bootstrap)) {
    return(invisible(NULL))
  }
  valid <- identical(class(bootstrap), "bq_bootstrap_control") &&
    identical(
      names(bootstrap),
      c("method", "engine", "iterations", "conf_type", "seed", "weight_type")
    ) &&
    is.character(bootstrap$method) && length(bootstrap$method) == 1L &&
    !is.na(bootstrap$method) &&
    bootstrap$method %in% c("ordinary", "fractional") &&
    identical(
      bootstrap$engine,
      if (identical(bootstrap$method, "ordinary")) "boot" else "fwb"
    ) &&
    is.integer(bootstrap$iterations) && length(bootstrap$iterations) == 1L &&
    !is.na(bootstrap$iterations) && bootstrap$iterations > 0L &&
    is.character(bootstrap$conf_type) && length(bootstrap$conf_type) == 1L &&
    !is.na(bootstrap$conf_type) &&
    bootstrap$conf_type %in% c("bca", "percentile", "basic") &&
    (is.null(bootstrap$seed) || (
      is.integer(bootstrap$seed) && length(bootstrap$seed) == 1L &&
        !is.na(bootstrap$seed) && bootstrap$seed >= 0L
    )) &&
    if (identical(bootstrap$method, "ordinary")) {
      is.null(bootstrap$weight_type)
    } else {
      identical(bootstrap$weight_type, "exponential")
    }
  if (!valid) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`bootstrap` must be NULL or a valid `bootstrap_control()` specification."
    )
  }
  if (!requireNamespace(bootstrap$engine, quietly = TRUE)) {
    bq_abort(
      "bq_error_missing_dependency",
      sprintf(
        "%s bootstrap requires the suggested package `%s`.",
        if (bootstrap$method == "ordinary") {
          "Ordinary"
        } else {
          "Fractional weighted"
        },
        bootstrap$engine
      )
    )
  }
  invisible(bootstrap)
}
