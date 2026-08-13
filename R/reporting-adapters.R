#' Render a bq table with gt
#' @param x A `bq_table`.
#' @param ... Additional arguments passed to [gt::gt()].
#' @return A `gt_tbl`.
#' @export
as_gt <- function(x, ...) {
  check_bq_table(x)
  if (!requireNamespace("gt", quietly=TRUE)) {
    stop_missing_reporting_backend("gt")
  }
  out <- gt::gt(table_body(x), ...)
  labels <- stats::setNames(as.list(x$table_header$label), x$table_header$column)
  do.call(gt::cols_label, c(list(.data=out), labels))
}

#' Render a bq table with flextable
#' @param x A `bq_table`.
#' @param ... Additional arguments passed to [flextable::flextable()].
#' @return A `flextable`.
#' @export
as_flextable <- function(x, ...) {
  check_bq_table(x)
  if (!requireNamespace("flextable", quietly=TRUE)) {
    stop_missing_reporting_backend("flextable")
  }
  out <- flextable::flextable(table_body(x), ...)
  labels <- stats::setNames(as.list(x$table_header$label), x$table_header$column)
  do.call(flextable::set_header_labels, c(list(x=out), labels))
}

stop_missing_reporting_backend <- function(package) {
  stop(structure(
    list(message=paste0("Reporting backend `",package,"` is not installed."), call=sys.call(-1L)),
    class=c("bq_error_missing_backend","error","condition")
  ))
}
