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
  if (
    !is.numeric(iterations) || length(iterations) != 1L ||
      is.na(iterations) || !is.finite(iterations) || iterations <= 0 ||
      iterations != floor(iterations) || iterations > .Machine$integer.max
  ) {
    bq_abort(
      "bq_error_invalid_permutation_control",
      paste0(
        "`iterations` must be one positive whole number no greater than ",
        ".Machine$integer.max."
      )
    )
  }

  if (
    !is.character(p_method) || length(p_method) != 1L ||
      is.na(p_method) || !identical(p_method, "plusone")
  ) {
    bq_abort(
      "bq_error_invalid_permutation_control",
      "`p_method` must be \"plusone\" for random permutations."
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
      "bq_error_invalid_permutation_control",
      paste0(
        "`seed` must be NULL or one non-negative whole number no greater ",
        "than .Machine$integer.max."
      )
    )
  }

  structure(
    list(
      sampling = "random",
      iterations = as.integer(iterations),
      p_method = p_method,
      seed = if (is.null(seed)) NULL else as.integer(seed)
    ),
    class = "bq_permutation_control"
  )
}
