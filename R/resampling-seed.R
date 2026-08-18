#' Evaluate one resampling stage with its declared seed
#'
#' A fixed seed makes one stage reproducible without changing the caller's
#' random-number stream. `NULL` deliberately uses and advances that stream.
#' The stage is isolated in this helper so restoration happens before a later
#' permutation or bootstrap stage starts, including when evaluation fails.
#'
#' @param seed A validated integer seed, or `NULL`.
#' @param code Code to evaluate lazily after applying `seed`.
#'
#' @return The value of `code`.
#' @noRd
with_resampling_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }

  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit({
    if (seed_exists) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })

  set.seed(seed)
  force(code)
}
