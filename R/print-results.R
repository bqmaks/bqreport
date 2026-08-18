#' Print a preflight result
#'
#' @param x A `bq_preflight` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
print.bq_preflight <- function(x, ...) {
  cat("<bq preflight: ", x$analysis, ">\n", sep = "")
  cat("Ready to run: ", if (isTRUE(x$ok)) "yes" else "no", "\n", sep = "")
  severity <- x$diagnostics$severity
  cat(
    "Diagnostics: ", sum(severity == "error"), " blocking, ",
    sum(severity == "warning"), " warning\n",
    sep = ""
  )
  if (!is.null(x$cells)) {
    cat("Cells: ", nrow(x$cells), "\n", sep = "")
  }
  invisible(x)
}

#' Print an analysis result
#'
#' @param x A `bq_result` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
print.bq_result <- function(x, ...) {
  cat("<bq result: ", x$analysis, ">\n", sep = "")
  for (name in setdiff(names(x), c("analysis", "plan"))) {
    if (is.data.frame(x[[name]])) {
      cat(name, ": ", nrow(x[[name]]), " rows\n", sep = "")
    }
  }
  invisible(x)
}

#' Lay a composed table out as one wide tibble
#'
#' The table object keeps rows, columns and body as separate flat registries
#' so that renderers can decide their own layout. This method offers the
#' plainest possible layout for inspection: one line per table row, one
#' column per cell, header text built from the cell's axis values.
#'
#' @param x A `bq_table` object.
#' @param ... Ignored.
#'
#' @return A tibble with `variable`, `row` and one character column per cell.
#' @exportS3Method tibble::as_tibble
as_tibble.bq_table <- function(x, ...) {
  rows <- x$rows
  columns <- x$columns
  axes <- x$column_axes

  headers <- vapply(columns$cell_id, function(cell_id) {
    cell_axes <- axes[axes$cell_id == cell_id, ]
    cell_axes <- cell_axes[order(cell_axes$axis_position), ]
    parts <- ifelse(cell_axes$is_overall, "Overall", cell_axes$value)
    n <- columns$n[columns$cell_id == cell_id]
    paste0(paste(parts, collapse = " / "), " (n = ", n, ")")
  }, character(1))
  headers <- make.unique(unname(headers), sep = " #")

  row_text <- ifelse(
    rows$row_kind == "summary_format",
    ifelse(is.na(rows$row_label), rows$template, rows$row_label),
    ifelse(rows$row_kind == "enumeration", "values", rows$component)
  )
  variable_text <- ifelse(
    is.na(rows$unit), rows$variable_label,
    paste0(rows$variable_label, ", ", rows$unit)
  )

  out <- tibble::tibble(variable = variable_text, row = row_text)
  for (position in seq_len(nrow(columns))) {
    cell_id <- columns$cell_id[position]
    values <- rep(NA_character_, nrow(rows))
    for (row_position in seq_len(nrow(rows))) {
      body <- x$body[
        x$body$row_id == rows$row_id[row_position] & x$body$cell_id == cell_id,
      ]
      if (nrow(body) > 0L) {
        values[row_position] <- body$value[1L]
        next
      }
      # A cell that is entirely missing or empty carries one status text for
      # all rows of the variable instead of repeated body entries.
      display <- x$cell_displays[
        x$cell_displays$cell_id == cell_id &
          x$cell_displays$var_id == rows$var_id[row_position],
      ]
      if (nrow(display) > 0L && !is.na(display$status_text[1L])) {
        values[row_position] <- display$status_text[1L]
      }
    }
    out[[headers[position]]] <- values
  }
  out
}

#' Print a composed table
#'
#' Prints the wide layout produced by `as_tibble()`. The flat registries stay
#' the source of truth; this is a convenience for inspection.
#'
#' @param x A `bq_table` object.
#' @param ... Passed on to the tibble print method.
#'
#' @return `x`, invisibly.
#' @export
print.bq_table <- function(x, ...) {
  cat("<bq table: ", x$analysis, ">\n", sep = "")
  print(tibble::as_tibble(x), ...)
  invisible(x)
}
