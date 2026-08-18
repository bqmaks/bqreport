#' Configure random permutation inference
#'
#' Declares a reproducible random-permutation configuration. The configuration
#' is computationally inert: permutations are performed only when an analytic
#' function consumes it.
#'
#' Random permutation p-values use the plus-one correction
#' `(b + 1) / (R + 1)`. Exact enumeration is not selected automatically and is
#' not part of this specification.
#'
#' @param iterations Number of random permutations. Must be one positive whole
#'   number no greater than `.Machine$integer.max`.
#' @param p_method Method used to calculate a random-permutation p-value.
#'   Currently only `"plusone"` is supported.
#' @param seed Integer seed used to reproduce the permutations, or `NULL` to
#'   use the current random-number stream. Must be a non-negative whole number
#'   no greater than `.Machine$integer.max` when supplied.
#'
#' @return A `bq_permutation_control` specification.
#' @export
#' @examples
#' permutation_control()
#' permutation_control(iterations = 20000, seed = 2026)
permutation_control <- function(
  iterations = 10000L,
  p_method = "plusone",
  seed = NULL
) {
  class <- "bq_error_invalid_permutation_control"
  iterations <- check_whole(iterations, "iterations", lower = 1, class = class)
  check_choice(p_method, "p_method", "plusone", class = class)
  if (!is.null(seed)) {
    seed <- check_whole(seed, "seed", lower = 0, class = class)
  }

  structure(
    list(
      sampling = "random",
      iterations = iterations,
      p_method = p_method,
      seed = seed
    ),
    class = "bq_permutation_control"
  )
}

#' Require a permutation specification consistent with `inference`
#'
#' The specification is re-checked structurally for the same reason as in
#' `check_bootstrap_control()`: it is plain data. Presence is tied to
#' `inference` so that a declared permutation test cannot silently run without
#' its configuration and an unused configuration cannot be carried along.
#'
#' @param permutation `NULL` or a `bq_permutation_control` object.
#' @param inference `"analytical"` or `"permutation"`.
#'
#' @return `permutation`, invisibly.
#' @noRd
check_permutation_control <- function(permutation, inference) {
  valid <- is.null(permutation) || (
    identical(class(permutation), "bq_permutation_control") &&
      identical(
        names(permutation), c("sampling", "iterations", "p_method", "seed")
      ) &&
      identical(permutation$sampling, "random") &&
      is.integer(permutation$iterations) &&
      length(permutation$iterations) == 1L &&
      !is.na(permutation$iterations) && permutation$iterations > 0L &&
      identical(permutation$p_method, "plusone") &&
      (is.null(permutation$seed) || (
        is.integer(permutation$seed) && length(permutation$seed) == 1L &&
          !is.na(permutation$seed) && permutation$seed >= 0L
      ))
  )
  if (!valid) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`permutation` must be NULL or a valid `permutation_control()` ",
        "specification."
      )
    )
  }
  if (inference == "permutation" && is.null(permutation)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`permutation` is required when `inference = \"permutation\"`."
    )
  }
  if (inference != "permutation" && !is.null(permutation)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`permutation` must be NULL unless `inference = \"permutation\"`."
    )
  }
  invisible(permutation)
}
