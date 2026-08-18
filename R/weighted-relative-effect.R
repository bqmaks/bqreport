#' Compute a weighted relative effect
#'
#' Computes `P(X > Y) + 0.5 * P(X = Y)` from two empirical distributions with
#' non-negative observation weights. Weights are normalized separately within
#' each distribution, so their absolute scales do not affect the estimand.
#'
#' @param x,y Non-empty finite numeric vectors.
#' @param x_weights,y_weights Non-negative finite numeric weights corresponding
#'   positionally to `x` and `y`. Each vector must have a positive sum.
#'
#' @return One numeric relative effect between zero and one.
#' @noRd
weighted_relative_effect <- function(x, y, x_weights, y_weights) {
  valid_values <- function(values) {
    is.numeric(values) && !is.object(values) && is.null(dim(values)) &&
      length(values) > 0L && !anyNA(values) && all(is.finite(values))
  }
  valid_weights <- function(weights, values) {
    is.numeric(weights) && !is.object(weights) && is.null(dim(weights)) &&
      length(weights) == length(values) && !anyNA(weights) &&
      all(is.finite(weights)) && all(weights >= 0) && sum(weights) > 0
  }

  if (!valid_values(x) || !valid_values(y)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "Relative-effect samples must be non-empty finite numeric vectors."
    )
  }
  if (!valid_weights(x_weights, x) || !valid_weights(y_weights, y)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      paste0(
        "Relative-effect weights must be finite, non-negative, positionally ",
        "matched to their samples and have positive sums."
      )
    )
  }

  x_weights <- as.double(x_weights) / sum(x_weights)
  y_weights <- as.double(y_weights) / sum(y_weights)
  conditional_probabilities <- vapply(x, function(x_value) {
    sum(y_weights[y < x_value]) + 0.5 * sum(y_weights[y == x_value])
  }, double(1))

  unname(sum(x_weights * conditional_probabilities))
}
