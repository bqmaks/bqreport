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
  reconciled <- reconcile_variables(
    attr(template, "variables"),
    names(data),
    attr(template, "next_var_number")
  )

  new_bq_data(
    data,
    reconciled$variables,
    reconcile_levels(attr(template, "levels"), reconciled$variables$var_id),
    reconcile_summary_formats(
      attr(template, "summary_formats"),
      reconciled$variables$var_id
    ),
    reconciled$next_var_number
  )
}

#' Invalidate value-dependent metadata after mutate()
#'
#' `type`, its source, event, event source, reference, declared levels, unit and
#' rounding policy describe the values of a column, so overwriting those values
#' makes them stale. `label` and `role` state the analyst's intent, are
#' independent of the values, and are kept.
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
  variables$unit[variables$name %in% rewritten] <- NA_character_
  variables$rounding[variables$name %in% rewritten] <- NA_character_
  variables$digits[variables$name %in% rewritten] <- NA_integer_
  attr(out, "variables") <- variables
  levels <- attr(out, "levels")
  attr(out, "levels") <- levels[!levels$var_id %in% rewritten_ids, ]

  out
}

#' Restore metadata after a base replacement operation
#'
#' @param out Result returned by the next replacement method.
#' @param template The `bq_data` object before replacement.
#' @param rewritten_names Names of existing columns whose values may have
#'   changed.
#'
#' @return A reconstructed `bq_data` object.
#' @noRd
restore_replaced_bq_data <- function(out, template, rewritten_names) {
  old_variables <- attr(template, "variables")
  rewritten_ids <- old_variables$var_id[old_variables$name %in% rewritten_names]
  reconciled <- reconcile_variables(
    old_variables,
    names(out),
    attr(template, "next_var_number")
  )
  variables <- reconciled$variables
  rewritten <- variables$var_id %in% rewritten_ids
  variables$type[rewritten] <- NA_character_
  variables$event[rewritten] <- NA_character_
  variables$event_source[rewritten] <- NA_character_
  variables$reference[rewritten] <- NA_character_
  variables$type_source[rewritten] <- NA_character_
  variables$unit[rewritten] <- NA_character_
  variables$rounding[rewritten] <- NA_character_
  variables$digits[rewritten] <- NA_integer_

  levels <- reconcile_levels(attr(template, "levels"), variables$var_id)
  levels <- levels[!levels$var_id %in% rewritten_ids, ]

  new_bq_data(
    tibble::as_tibble(out),
    variables,
    levels,
    reconcile_summary_formats(
      attr(template, "summary_formats"),
      variables$var_id
    ),
    reconciled$next_var_number
  )
}

#' Resolve a replacement subscript to existing column names
#'
#' @param names Existing column names.
#' @param index A column subscript accepted by a data frame replacement method.
#'
#' @return Names of existing columns selected by `index`.
#' @noRd
replacement_names <- function(names, index) {
  if (is.matrix(index)) {
    return(names)
  }

  if (is.character(index)) {
    return(unique(intersect(index, names)))
  }

  selected <- tryCatch(names[index], error = function(error) names)
  unique(selected[!is.na(selected) & selected %in% names])
}

#' Replace a column with `$<-`
#'
#' New columns receive fresh identifiers. Replacing an existing column clears
#' metadata that described its previous values.
#'
#' @param x A `bq_data` object.
#' @param name Name of the column to replace.
#' @param value Replacement value, or `NULL` to remove the column.
#'
#' @return A `bq_data` object.
#' @export
`$<-.bq_data` <- function(x, name, value) {
  out <- NextMethod()
  restore_replaced_bq_data(out, x, intersect(name, names(x)))
}

#' Replace a column with `[[<-`
#'
#' New columns receive fresh identifiers. Replacing existing values clears
#' metadata that described their previous values.
#'
#' @param x A `bq_data` object.
#' @param ... Column, or row and column, subscripts.
#' @param value Replacement value, or `NULL` to remove a column.
#'
#' @return A `bq_data` object.
#' @export
`[[<-.bq_data` <- function(x, ..., value) {
  indices <- list(...)
  column_index <- indices[[length(indices)]]
  rewritten_names <- replacement_names(names(x), column_index)
  out <- NextMethod()

  restore_replaced_bq_data(out, x, rewritten_names)
}

#' Replace values with `[<-`
#'
#' New columns receive fresh identifiers. Metadata is cleared for each
#' existing column touched by either a column replacement or a row-and-column
#' replacement.
#'
#' @param x A `bq_data` object.
#' @param i Row or one-dimensional column subscript.
#' @param j Column subscript for two-dimensional replacement.
#' @param ... Passed on unchanged.
#' @param value Replacement value.
#'
#' @return A `bq_data` object.
#' @export
`[<-.bq_data` <- function(x, i, j, ..., value) {
  one_dimensional <- nargs() <= 3L
  column_index <- if (one_dimensional) {
    if (missing(i)) seq_along(x) else i
  } else {
    if (missing(j)) seq_along(x) else j
  }

  # add_column() inserts temporary columns by positions beyond ncol() and then
  # restores every custom attribute from its original input. tibble exposes no
  # class hook for that final restore, so allowing the call would silently put
  # the old registry on the enlarged data.
  if (
    one_dimensional && is.numeric(column_index) &&
      any(column_index > length(x), na.rm = TRUE)
  ) {
    bq_abort(
      "bq_error_unsupported_operation",
      paste0(
        "Adding columns by numeric position is not supported on bq_data.\n",
        "Use `dplyr::mutate()` or a named replacement instead; this also ",
        "applies to `tibble::add_column()`."
      )
    )
  }

  rewritten_names <- replacement_names(names(x), column_index)
  out <- NextMethod()

  restore_replaced_bq_data(out, x, rewritten_names)
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

  reconciled <- reconcile_variables(
    attr(x, "variables"),
    names(out),
    attr(x, "next_var_number")
  )

  new_bq_data(
    out,
    reconciled$variables,
    reconcile_levels(attr(x, "levels"), reconciled$variables$var_id),
    reconcile_summary_formats(
      attr(x, "summary_formats"),
      reconciled$variables$var_id
    ),
    reconciled$next_var_number
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
  if (
    !is.character(value) || length(value) != ncol(x) || anyNA(value) ||
      any(!nzchar(value)) || anyDuplicated(value)
  ) {
    bq_abort(
      "bq_error_invalid_data",
      "Column names of a bq_data object must be unique and non-empty."
    )
  }

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

#' Reject rowwise grouping of bq_data
#'
#' @param data A `bq_data` object.
#' @param ... Ignored.
#'
#' @return Never returns; always raises an error.
#' @exportS3Method dplyr::rowwise
rowwise.bq_data <- function(data, ...) {
  bq_abort(
    "bq_error_unsupported_operation",
    paste0(
      "`rowwise()` is not supported on bq_data: rowwise grouping would drop ",
      "the bq_data class.\n",
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
  attr(x, "summary_formats") <- NULL
  attr(x, "next_var_number") <- NULL
  class(x) <- setdiff(class(x), "bq_data")
  x
}
