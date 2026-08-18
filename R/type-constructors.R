#' Declare a continuous analytic type
#'
#' Creates a type specification for a continuous variable. The specification
#' records intent only; it does not convert or otherwise modify data. It is
#' designed to be applied to a `bq_data` column by a type setter.
#'
#' @return A `bq_type` specification.
#' @export
#' @examples
#' type_continuous()
type_continuous <- function() {
  structure(
    list(
      type = "continuous",
      event = NA_character_,
      reference = NA_character_,
      levels = character()
    ),
    class = "bq_type"
  )
}

#' Declare a count analytic type
#'
#' Creates a type specification for a count variable. The specification
#' records intent only; validation of the values belongs to preflight, when a
#' specification is associated with a concrete column.
#'
#' @return A `bq_type` specification.
#' @export
#' @examples
#' type_count()
type_count <- function() {
  structure(
    list(
      type = "count",
      event = NA_character_,
      reference = NA_character_,
      levels = character()
    ),
    class = "bq_type"
  )
}

#' Declare a binary analytic type
#'
#' Creates a type specification for a binary variable and records which value
#' is the event. The event is stored as text so specifications for columns with
#' different R storage types can expand into one flat registry column.
#'
#' This function validates the specification, not the data. Whether `event`
#' occurs in a concrete column is checked when the specification is applied.
#'
#' @param event A single, non-missing atomic value to treat as the event.
#'
#' @return A `bq_type` specification.
#' @export
#' @examples
#' type_binary(1)
#' type_binary(TRUE)
#' type_binary("case")
type_binary <- function(event) {
  if (missing(event)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`event` is required for `type_binary()`; choose the value that represents the event."
    )
  }

  if (!is.atomic(event) || length(event) != 1L || is.na(event)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`event` must be one non-missing atomic value."
    )
  }

  structure(
    list(
      type = "binary",
      event = as.character(event),
      reference = NA_character_,
      levels = character()
    ),
    class = "bq_type"
  )
}

#' Declare an ordinal analytic type
#'
#' Creates a type specification for an ordinal variable. `levels` states the
#' category order from lowest to highest. Values are stored as text so the
#' specification can later expand into one flat level registry.
#'
#' This function validates the specification, not the data. Whether the
#' declared levels match a concrete column is checked when the specification
#' is applied.
#'
#' @param levels An atomic vector of at least three distinct, non-missing
#'   values, ordered from lowest to highest.
#'
#' @return A `bq_type` specification.
#' @export
#' @examples
#' type_ordinal(c("low", "medium", "high"))
#' type_ordinal(1:5)
type_ordinal <- function(levels) {
  if (missing(levels)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      paste0(
        "`levels` is required for `type_ordinal()`; supply the categories from ",
        "lowest to highest."
      )
    )
  }

  if (!is.atomic(levels) || !is.null(dim(levels))) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`levels` must be an atomic vector."
    )
  }

  if (length(levels) < 3L || anyNA(levels)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`levels` must contain at least three non-missing values."
    )
  }

  levels <- as.character(levels)

  if (anyDuplicated(levels)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`levels` must not contain duplicates."
    )
  }

  structure(
    list(
      type = "ordinal",
      event = NA_character_,
      reference = NA_character_,
      levels = levels
    ),
    class = "bq_type"
  )
}

#' Declare a nominal analytic type
#'
#' Creates a type specification for a nominal variable and records its
#' reference category. The reference is stored as text so specifications for
#' columns with different R storage types can expand into one flat registry
#' column.
#'
#' This function validates the specification, not the data. Whether
#' `reference` occurs in a concrete column is checked when the specification is
#' applied.
#'
#' @param reference A single, non-missing atomic value to use as the reference
#'   category.
#'
#' @return A `bq_type` specification.
#' @export
#' @examples
#' type_nominal("control")
#' type_nominal(0)
type_nominal <- function(reference) {
  if (missing(reference)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      paste0(
        "`reference` is required for `type_nominal()`; choose the category to use ",
        "as the reference."
      )
    )
  }

  if (!is.atomic(reference) || length(reference) != 1L || is.na(reference)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`reference` must be one non-missing atomic value."
    )
  }

  structure(
    list(
      type = "nominal",
      event = NA_character_,
      reference = as.character(reference),
      levels = character()
    ),
    class = "bq_type"
  )
}
