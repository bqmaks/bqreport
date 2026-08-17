#' Restore a bq_data object after a dplyr verb
#'
#' dplyr rebuilds a plain data frame and then asks the original object to
#' restore whatever it carries. This is the entry point used by `mutate()`,
#' `filter()`, the joins and the `bind_*()` functions.
#'
#' @param data The rebuilt plain data frame.
#' @param template The original `bq_data` object.
#'
#' @return A `bq_data` object.
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.bq_data <- function(data, template) {
  variables <- reconcile_variables(attr(template, "variables"), names(data))

  new_bq_data(
    data,
    variables,
    reconcile_levels(attr(template, "levels"), variables$var_id)
  )
}

#' Invalidate value-dependent metadata after mutate()
#'
#' `type`, its source, event, event source, reference and declared levels
#' describe the values of a column, so overwriting those values makes them
#' stale. `label` and `role` state the analyst's intent, are independent of the
#' values, and are kept.
#'
#' @param data The `bq_data` object being modified.
#' @param cols Named list of new column values, `NULL` meaning removal.
#'
#' @return A `bq_data` object.
#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.bq_data <- function(data, cols) {
  # Determined before the change, while the registry still describes the old
  # columns: names not listed here belong to columns that did not exist yet.
  rewritten <- intersect(names(cols), attr(data, "variables")$name)
  rewritten_ids <- attr(data, "variables")$var_id[
    attr(data, "variables")$name %in% rewritten
  ]

  # NextMethod() rebuilds the columns and reconstructs the registry from the
  # template; the invalidation below is applied to that result.
  out <- NextMethod()

  variables <- attr(out, "variables")
  variables$type[variables$name %in% rewritten] <- NA_character_
  variables$event[variables$name %in% rewritten] <- NA_character_
  variables$event_source[variables$name %in% rewritten] <- NA_character_
  variables$reference[variables$name %in% rewritten] <- NA_character_
  variables$type_source[variables$name %in% rewritten] <- NA_character_
  attr(out, "variables") <- variables
  levels <- attr(out, "levels")
  attr(out, "levels") <- levels[!levels$var_id %in% rewritten_ids, ]

  out
}

#' Subset a bq_data object
#'
#' `select()` and `relocate()` reach the object through `[`, not through
#' `dplyr_reconstruct()`, so the registry has to follow the columns here.
#'
#' @param x A `bq_data` object.
#' @param ... Row and column indices, passed on unchanged.
#'
#' @return A `bq_data` object, or a bare vector when `drop = TRUE` selects a
#'   single column.
#' @export
`[.bq_data` <- function(x, ...) {
  out <- NextMethod()

  # drop = TRUE yields a plain vector, which has no columns to describe.
  if (!is.data.frame(out)) {
    return(out)
  }

  variables <- reconcile_variables(attr(x, "variables"), names(out))

  new_bq_data(
    out,
    variables,
    reconcile_levels(attr(x, "levels"), variables$var_id)
  )
}

#' Rename the columns of a bq_data object
#'
#' `rename()` works by assigning a full name vector, leaving every column in
#' place. Names are therefore copied into the registry by position: matching
#' them by value instead would read a rename as one column dropped and another
#' one added, and would throw the metadata away.
#'
#' @param x A `bq_data` object.
#' @param value Character vector of new names, one per column.
#'
#' @return A `bq_data` object.
#' @export
`names<-.bq_data` <- function(x, value) {
  out <- NextMethod()

  variables <- attr(out, "variables")
  variables$name <- names(out)
  attr(out, "variables") <- variables

  out
}

#' Reject grouping of bq_data
#'
#' `group_by()` returns a `grouped_df` built from scratch, which drops both the
#' class and the registry. Failing here is better than handing back an object
#' that silently lost its metadata.
#'
#' @param .data A `bq_data` object.
#' @param ... Ignored.
#'
#' @return Never returns; always raises an error.
#' @exportS3Method dplyr::group_by
group_by.bq_data <- function(.data, ...) {
  bq_abort(
    "bq_error_unsupported_operation",
    paste0(
      "`group_by()` is not supported on bq_data: grouping would drop the ",
      "variable registry.\n",
      "For analysis, mark the grouping column with role \"group\" instead.\n",
      "For manual data wrangling, call `as_tibble()` first."
    )
  )
}

#' Drop analytic metadata and return a plain tibble
#'
#' @param x A `bq_data` object.
#' @param ... Ignored.
#'
#' @return A tibble.
#' @exportS3Method tibble::as_tibble
as_tibble.bq_data <- function(x, ...) {
  attr(x, "variables") <- NULL
  attr(x, "levels") <- NULL
  class(x) <- setdiff(class(x), "bq_data")
  x
}
