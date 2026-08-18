#' Print an analytic function specification
#'
#' Shows decisions fixed by a comparison constructor without printing the
#' executable closure that implements them.
#'
#' @param x A `bq_analysis_function` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
print.bq_analysis_function <- function(x, ...) {
  specification <- attr(x, "specification")
  capabilities <- attr(x, "capabilities")
  format_value <- function(value) {
    if (is.null(value)) {
      return("none")
    }
    if (is.list(value)) {
      fields <- vapply(names(value), function(name) {
        paste0(name, "=", format_value(value[[name]]))
      }, character(1))
      return(paste(fields, collapse = ", "))
    }
    if (length(value) == 1L && is.na(value)) {
      return("none")
    }
    if (is.character(value)) {
      return(paste(encodeString(value, quote = '"'), collapse = ", "))
    }
    paste(as.character(value), collapse = ", ")
  }

  cat("<bq analysis function>\n")
  cat("Kind: ", specification$kind, "\n", sep = "")
  for (name in setdiff(names(specification), "kind")) {
    cat(name, ": ", format_value(specification[[name]]), "\n", sep = "")
  }
  dependencies <- capabilities$suggested_dependencies
  cat(
    "Dependencies: ",
    if (length(dependencies) == 0L) "none" else paste(dependencies, collapse = ", "),
    "\n",
    sep = ""
  )

  invisible(x)
}
