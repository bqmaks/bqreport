#' Declare a custom continuous statistic
#'
#' Creates a specification for a user-defined raw statistic. During analysis,
#' `fun` receives the original continuous vector inside one cell, including
#' missing values and in its original row order. The function owns its missing
#' value policy completely.
#'
#' The constructor calls `fun(numeric(0))` once to fix the result schema before
#' analysis. The function must return a one-row data frame with one or more
#' uniquely named numeric columns. Prototype values are discarded; only column
#' names and storage types are kept. User functions should therefore be pure
#' and must handle an empty vector.
#'
#' @param name A single, non-missing, non-empty identifier for the statistic.
#' @param fun A function of one vector that returns a one-row data frame with
#'   numeric or integer columns.
#'
#' @return A `bq_continuous_statistic` specification.
#' @export
#' @examples
#' robust <- continuous_statistic(
#'   "robust",
#'   function(x) {
#'     data.frame(
#'       median = if (length(x) == 0L) NA_real_ else stats::median(x, na.rm = TRUE),
#'       mad = if (length(x) == 0L) NA_real_ else stats::mad(x, na.rm = TRUE)
#'     )
#'   }
#' )
#' robust
continuous_statistic <- function(name, fun) {
  if (
    !is.character(name) || length(name) != 1L || is.na(name) ||
      !nzchar(name)
  ) {
    bq_abort(
      "bq_error_invalid_statistic",
      "`name` must be one non-missing, non-empty character value."
    )
  }

  if (!is.function(fun)) {
    bq_abort(
      "bq_error_invalid_statistic",
      "`fun` must be a function accepting one continuous vector."
    )
  }

  prototype <- tryCatch(
    fun(numeric()),
    error = function(error) {
      bq_abort(
        "bq_error_invalid_statistic",
        paste0(
          "Statistic `", name, "` fails for an empty numeric vector: ",
          conditionMessage(error)
        )
      )
    }
  )

  if (!is.data.frame(prototype)) {
    bq_abort(
      "bq_error_invalid_statistic",
      sprintf("Statistic `%s` must return a data frame.", name)
    )
  }

  if (nrow(prototype) != 1L) {
    bq_abort(
      "bq_error_invalid_statistic",
      sprintf("Statistic `%s` must return exactly one row.", name)
    )
  }

  if (ncol(prototype) == 0L) {
    bq_abort(
      "bq_error_invalid_statistic",
      sprintf("Statistic `%s` must return at least one column.", name)
    )
  }

  if (any(names(prototype) == "") || anyDuplicated(names(prototype))) {
    bq_abort(
      "bq_error_invalid_statistic",
      sprintf("Statistic `%s` must return uniquely named, non-empty columns.", name)
    )
  }

  valid_columns <- vapply(
    prototype,
    function(column) {
      is.numeric(column) && !is.object(column) && is.null(dim(column))
    },
    logical(1)
  )

  if (!all(valid_columns)) {
    bq_abort(
      "bq_error_invalid_statistic",
      sprintf("Statistic `%s` must return only plain numeric or integer columns.", name)
    )
  }

  structure(
    list(
      kind = "continuous_statistic",
      name = name,
      components = names(prototype),
      component_types = vapply(prototype, typeof, character(1)),
      source = "custom_raw",
      missing = "user",
      fun = fun
    ),
    class = c("bq_continuous_statistic", "bq_statistic")
  )
}
