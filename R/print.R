#' Header shown when a bq_data object is printed
#'
#' tibble builds its header from this generic, so extending it is enough to
#' announce the registry: the rest of the printing (row limits, column
#' widths, truncation) keeps working unchanged.
#'
#' @param x A `bq_data` object.
#' @param ... Passed on to the tibble method.
#'
#' @return A named character vector of header lines.
#' @exportS3Method tibble::tbl_sum
tbl_sum.bq_data <- function(x, ...) {
  # The tibble method already formats the dimensions; only its label changes.
  dimensions <- NextMethod()

  c(
    "A bq_data" = unname(dimensions[[1L]]),
    "Variables" = summarise_roles(variables(x))
  )
}

#' Describe how many columns hold each role
#'
#' @param variables A variable registry.
#'
#' @return A character scalar such as `"1 group, 2 outcomes"`.
#' @noRd
summarise_roles <- function(variables) {
  if (nrow(variables) == 0L) {
    return("none")
  }

  # Alphabetical for now; this will follow the role vocabulary once one is
  # defined.
  counts <- table(variables$role[!is.na(variables$role)])
  parts <- sprintf("%d %s%s", counts, names(counts), ifelse(counts == 1L, "", "s"))

  # Reported last, and separately: "unassigned" is a state of the column, not
  # a role, so it is not pluralised along with the role names.
  waiting <- sum(is.na(variables$role))
  if (waiting > 0L) {
    parts <- c(parts, sprintf("%d unassigned", waiting))
  }

  paste(parts, collapse = ", ")
}
