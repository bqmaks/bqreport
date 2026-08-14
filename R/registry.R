#' Align a variable registry with a new set of columns
#'
#' The registry holds exactly one row per column, in column order. After a
#' verb has added, dropped or reordered columns, this brings the registry back
#' in line: rows are matched to the new columns by name, dropped columns lose
#' their row, and columns that were not there before get a fresh row.
#'
#' Matching is by name because that is all a column subset leaves behind.
#' Renaming is handled separately, in `names<-.bq_data()`, where positions are
#' known to be unchanged.
#'
#' @param variables The registry to align.
#' @param names Character vector of the new column names, in column order.
#'
#' @return A registry with one row per element of `names`.
#' @noRd
reconcile_variables <- function(variables, names) {
  position <- match(names, variables$name)

  # Unmatched names index an all-NA row, which is exactly the blank record a
  # brand new column should start from.
  out <- variables[position, ]
  out$name <- names

  fresh <- is.na(position)
  out$var_id[fresh] <- next_var_ids(variables$var_id, sum(fresh))

  out
}

#' Generate identifiers that no existing column uses
#'
#' Numbering continues past the highest id ever issued for this object rather
#' than filling gaps, so an id is never silently reused by a different column.
#'
#' @param existing Character vector of identifiers already in the registry.
#' @param n Number of new identifiers required.
#'
#' @return Character vector of length `n`.
#' @noRd
next_var_ids <- function(existing, n) {
  if (n == 0L) {
    return(character(0))
  }

  numbered <- grepl("^v[0-9]+$", existing)
  used <- as.integer(sub("^v", "", existing[numbered]))
  start <- if (length(used) > 0L) max(used) + 1L else 1L

  sprintf("v%03d", seq.int(start, length.out = n))
}
