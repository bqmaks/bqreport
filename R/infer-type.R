#' Infer the analytic type of a column
#'
#' Classifies a vector into the measurement scale the analysis needs, which is
#' deliberately not the same thing as its storage in R: a count, a binary flag
#' and a continuous measurement are all numeric.
#'
#' The rules are conservative. `count` is never inferred, because a count and a
#' continuous measurement rounded to whole numbers are indistinguishable from
#' the values alone and the consequences of guessing wrong reach the model
#' family. A numeric column is only read as binary when every observed value is
#' 0 or 1.
#'
#' How a binary column is stored — `0`/`1`, `TRUE`/`FALSE` or two labels — is
#' not part of its type. It decides which level counts as the event, recorded
#' separately, and how the column is laid out in tables, which belongs to the
#' presentation layer.
#'
#' Factors are classified from their declared `levels()`, not from the values
#' that happen to occur, since the declaration states how many categories the
#' variable has. Call [droplevels()] first if unused levels are stale.
#'
#' @param x A vector.
#' @param max_levels Largest number of categories still read as `"nominal"`.
#'   Above it the column is left `"unknown"`, which keeps free text and
#'   identifiers out of the categorical analyses.
#'
#' @return One of `"continuous"`, `"binary"`, `"ordinal"`, `"nominal"`,
#'   `"date"`, `"datetime"` or `"unknown"`. `"count"` is only ever set
#'   explicitly.
#' @export
#' @examples
#' infer_type(c(40, 55, 61))
#' infer_type(c(0, 1, 1, 0))
#' infer_type(factor(c("f", "m")))
#' infer_type(factor(c("low", "mid", "high"), ordered = TRUE))
infer_type <- function(x, max_levels = 20L) {
  # A column without a single observation carries no evidence at all. This
  # also catches the empty columns that data exports tend to type as logical.
  if (all(is.na(x))) {
    return("unknown")
  }

  # Dates store a number underneath, so they have to be recognised before any
  # numeric rule can claim them.
  if (inherits(x, "Date")) {
    return("date")
  }

  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return("datetime")
  }

  if (is.logical(x)) {
    return("binary")
  }

  if (is.factor(x) || is.character(x)) {
    return(infer_categorical_type(x, max_levels))
  }

  if (is.numeric(x)) {
    observed <- unique(x[!is.na(x)])

    if (all(observed %in% c(0, 1))) {
      return("binary")
    }

    return("continuous")
  }

  "unknown"
}

#' Classify a factor or character column by its categories
#'
#' @param x A factor or character vector.
#' @param max_levels Largest number of categories still read as `"nominal"`.
#'
#' @return A type name.
#' @noRd
infer_categorical_type <- function(x, max_levels) {
  categories <- if (is.factor(x)) levels(x) else unique(x[!is.na(x)])

  # A 0/1 or TRUE/FALSE coding, usually logical data that lost its type on
  # import through a CSV. Named here so that it stays binary even when only
  # one of the two values occurs, which counting categories would miss.
  if (all(categories %in% c("0", "1")) || all(categories %in% c("FALSE", "TRUE"))) {
    return("binary")
  }

  # An order over two categories states nothing the two categories do not
  # already state, so it does not make the column ordinal.
  if (is.ordered(x) && length(categories) >= 3L) {
    return("ordinal")
  }

  if (length(categories) == 2L) {
    return("binary")
  }

  if (length(categories) <= max_levels) {
    return("nominal")
  }

  "unknown"
}
