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
#' @param next_var_number Next number that has never been issued for this
#'   `bq_data` lineage.
#'
#' @return A list containing the aligned registry and the next unused number.
#' @noRd
reconcile_variables <- function(variables, names, next_var_number) {
  position <- match(names, variables$name)

  # Unmatched names index an all-NA row, which is exactly the blank record a
  # brand new column should start from.
  out <- variables[position, ]
  out$name <- names

  fresh <- is.na(position)
  out$var_id[fresh] <- next_var_ids(next_var_number, sum(fresh))

  list(
    variables = out,
    next_var_number = next_var_number + sum(fresh)
  )
}

#' Remove levels whose variables no longer exist
#'
#' @param levels The level registry.
#' @param var_ids Identifiers present in the reconciled variable registry.
#'
#' @return A level registry containing only existing variables.
#' @noRd
reconcile_levels <- function(levels, var_ids) {
  levels[levels$var_id %in% var_ids, ]
}

#' Generate identifiers that no existing column uses
#'
#' Numbering continues past the highest id ever issued for this object rather
#' than filling gaps, so an id is never silently reused by a different column.
#'
#' @param start First number that has never been issued.
#' @param n Number of new identifiers required.
#'
#' @return Character vector of length `n`.
#' @noRd
next_var_ids <- function(start, n) {
  if (n == 0L) {
    return(character(0))
  }

  sprintf("v%03d", seq.int(start, length.out = n))
}
