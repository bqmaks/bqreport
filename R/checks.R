#' Validate scalar arguments
#'
#' Constructors fix many scalar decisions and reject anything that is not one
#' well-formed value. These helpers keep that check and its message in one
#' place so every constructor rejects the same shapes for the same reason.
#'
#' Each helper returns its input invisibly, coerced to the storage the
#' specification records (`check_number()` returns a double, `check_whole()`
#' an integer), so a call can double as the assignment.
#'
#' @param x The argument value.
#' @param arg Argument name used in the message.
#' @param class Condition class signalled on failure.
#'
#' @return `x`, invisibly, possibly coerced.
#' @name checks
#' @noRd
NULL

#' @rdname checks
#' @noRd
check_flag <- function(x, arg, class = "bq_error_invalid_analysis_function") {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    bq_abort(class, sprintf("`%s` must be either TRUE or FALSE.", arg))
  }
  invisible(x)
}

#' @rdname checks
#' @param choices Allowed values.
#' @noRd
check_choice <- function(
  x,
  arg,
  choices,
  class = "bq_error_invalid_analysis_function"
) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    quoted <- encodeString(choices, quote = "\"")
    listed <- if (length(choices) == 1L) {
      quoted
    } else if (length(choices) == 2L) {
      paste0("either ", quoted[1L], " or ", quoted[2L])
    } else {
      paste0(
        "one of ",
        paste(quoted[-length(quoted)], collapse = ", "),
        " and ", quoted[length(quoted)]
      )
    }
    bq_abort(class, sprintf("`%s` must be %s.", arg, listed))
  }
  invisible(x)
}

#' @rdname checks
#' @noRd
check_string <- function(x, arg, class = "bq_error_invalid_analysis_function") {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    bq_abort(class, sprintf("`%s` must be one non-empty string.", arg))
  }
  invisible(x)
}

#' @rdname checks
#' @param lower,upper Bounds of the allowed range.
#' @param inclusive Whether the bounds themselves are allowed.
#' @noRd
check_number <- function(
  x,
  arg,
  lower = -Inf,
  upper = Inf,
  inclusive = TRUE,
  class = "bq_error_invalid_analysis_function"
) {
  inside <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    if (inclusive) x >= lower && x <= upper else x > lower && x < upper
  if (!inside) {
    bq_abort(
      class,
      sprintf(
        "`%s` must be one finite number %s.",
        arg, describe_range(lower, upper, inclusive)
      )
    )
  }
  invisible(as.double(x))
}

#' @rdname checks
#' @noRd
check_whole <- function(
  x,
  arg,
  lower = 0,
  class = "bq_error_invalid_analysis_function"
) {
  # .Machine$integer.max bounds the value so as.integer() cannot overflow.
  if (
    !is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < lower || x > .Machine$integer.max
  ) {
    bq_abort(
      class,
      sprintf(
        "`%s` must be one whole number %sno greater than .Machine$integer.max.",
        arg,
        if (is.finite(lower)) {
          paste0("no less than ", format(lower), " and ")
        } else {
          ""
        }
      )
    )
  }
  invisible(as.integer(x))
}

#' Describe a numeric range in words
#'
#' @return A character scalar such as `"strictly between 0 and 1"`.
#' @noRd
describe_range <- function(lower, upper, inclusive) {
  if (is.finite(lower) && is.finite(upper)) {
    return(sprintf(
      "%sbetween %s and %s",
      if (inclusive) "" else "strictly ", format(lower), format(upper)
    ))
  }
  if (is.finite(lower)) {
    return(sprintf(
      "%s %s", if (inclusive) "no less than" else "greater than", format(lower)
    ))
  }
  if (is.finite(upper)) {
    return(sprintf(
      "%s %s", if (inclusive) "no greater than" else "less than", format(upper)
    ))
  }
  ""
}

#' Require a suggested package at declaration time
#'
#' A missing engine is a configuration error, so it is reported when the
#' analysis is declared rather than when it runs. There is no fallback.
#'
#' @param package Package name.
#' @param purpose Text naming what needs the package, used in the message.
#' @param version Minimum version, or `NULL`.
#'
#' @return `TRUE`, invisibly.
#' @noRd
check_dependency <- function(package, purpose, version = NULL) {
  available <- requireNamespace(package, quietly = TRUE) &&
    (is.null(version) || utils::packageVersion(package) >= version)
  if (!available) {
    bq_abort(
      "bq_error_missing_dependency",
      sprintf(
        "%s requires the suggested package `%s`%s; install it with %s.",
        purpose, package,
        if (is.null(version)) "" else paste0(" version ", version, " or later"),
        sprintf("`install.packages(\"%s\")`", package)
      )
    )
  }
  invisible(TRUE)
}
