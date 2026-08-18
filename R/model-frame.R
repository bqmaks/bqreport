#' Coerce a continuous variable for analytic computation
#'
#' Character and factor storage is accepted only when every observed value has
#' an unambiguous finite numeric representation. Returning `NULL` lets
#' preflight report the problem before an engine or custom statistic runs.
#'
#' @param x A source column declared as continuous.
#'
#' @return A plain double vector, or `NULL` when lossless coercion is not
#'   possible.
#' @noRd
as_continuous_model_vector <- function(x) {
  if (!is.atomic(x) || !is.null(dim(x)) || is.complex(x)) {
    return(NULL)
  }

  converted <- if (is.factor(x) || is.character(x)) {
    suppressWarnings(as.double(as.character(x)))
  } else {
    suppressWarnings(as.double(x))
  }

  introduced_missing <- is.na(converted) & !is.na(x)
  observed <- converted[!is.na(converted)]
  if (any(introduced_missing) || any(!is.finite(observed))) {
    return(NULL)
  }

  converted
}
