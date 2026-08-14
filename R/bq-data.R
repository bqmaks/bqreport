#' Analysis data with a variable registry
#'
#' `as_bq_data()` turns a data frame into a `bq_data` object. The data itself
#' stays an ordinary tibble; a registry of per-column analytic properties is
#' attached next to it and carried along through the analysis.
#'
#' The registry starts out empty: every column gets a stable `var_id` and its
#' current `name`, while `label`, `role` and `type` are `NA` until they are
#' set explicitly or inferred.
#'
#' @param data A data frame or tibble.
#'
#' @return A `bq_data` object: a tibble with a `variables` attribute.
#' @export
#' @examples
#' as_bq_data(data.frame(age = c(40, 55), sex = c("f", "m")))
as_bq_data <- function(data) {
  if (inherits(data, "bq_data")) {
    return(data)
  }

  if (!is.data.frame(data)) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a data frame, not %s.", class(data)[1L])
    )
  }

  # as_tibble() rejects duplicate and empty column names, which the registry
  # relies on to address columns by name.
  data <- tibble::as_tibble(data)

  new_bq_data(data, new_variable_registry(names(data)))
}

#' Build a bq_data object from its parts
#'
#' Low-level constructor: it performs no validation and expects the column
#' names of `data` to already match `variables$name`.
#'
#' The class vector is stated in full rather than derived from `data`, because
#' dplyr hands back bare data frames: deriving it would quietly produce a
#' `bq_data` that is no longer a tibble.
#'
#' @param data A data frame.
#' @param variables A variable registry tibble.
#'
#' @return A `bq_data` object.
#' @noRd
new_bq_data <- function(data, variables) {
  tibble::new_tibble(
    data,
    variables = variables,
    nrow = nrow(data),
    class = "bq_data"
  )
}

#' Build an empty variable registry
#'
#' Identifiers are sequential (`v001`, `v002`, ...) rather than hashes of the
#' column contents: the same input always produces the same identifiers, and
#' an id stays with its column when the column is renamed.
#'
#' @param names Character vector of column names, in column order.
#'
#' @return A tibble with one row per column.
#' @noRd
new_variable_registry <- function(names) {
  tibble::tibble(
    var_id = sprintf("v%03d", seq_along(names)),
    name = names,
    label = NA_character_,
    role = NA_character_,
    type = NA_character_
  )
}
