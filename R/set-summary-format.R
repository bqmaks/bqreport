#' Set summary presentation formats
#'
#' Records one or more ordered templates for presenting descriptive statistic
#' components of selected variables. Templates use component placeholders such
#' as `{mean}` and `{sd}`. Their existence is checked later against an analysis
#' plan, when the assigned statistics are known.
#'
#' A completely unnamed `formats` vector is allowed. If names are supplied,
#' every format must have one unique, non-empty name. Names remain separate row
#' metadata and are not automatically added to the formatted value.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more columns.
#' @param formats A non-empty character vector of presentation templates,
#'   optionally with unique non-empty names.
#'
#' @return `data` with updated summary format metadata.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55)))
#' set_summary_format(
#'   data,
#'   age,
#'   c(
#'     "Mean (SD)" = "{mean} ({sd})",
#'     "Median (Q1; Q3)" = "{median} ({q1}; {q3})"
#'   )
#' )
set_summary_format <- function(data, variables, formats) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  if (
    missing(formats) || !is.character(formats) || length(formats) == 0L ||
      anyNA(formats) || any(!nzchar(formats))
  ) {
    bq_abort(
      "bq_error_invalid_summary_format",
      "`formats` must be a non-empty character vector without missing or empty templates."
    )
  }

  format_names <- names(formats)
  if (!is.null(format_names)) {
    if (
      anyNA(format_names) || any(!nzchar(format_names)) ||
        anyDuplicated(format_names)
    ) {
      bq_abort(
        "bq_error_invalid_summary_format",
        paste0(
          "Named `formats` must have one unique, non-empty name for every ",
          "template."
        )
      )
    }
  } else {
    format_names <- rep(NA_character_, length(formats))
  }

  placeholder <- "\\{[A-Za-z][A-Za-z0-9_.]*\\}"
  invalid_template <- vapply(
    formats,
    function(template) {
      !grepl(placeholder, template, perl = TRUE) ||
        grepl("[{}]", gsub(placeholder, "", template, perl = TRUE))
    },
    logical(1)
  )
  if (any(invalid_template)) {
    bq_abort(
      "bq_error_invalid_summary_format",
      sprintf(
        paste0(
          "Summary format `%s` must contain at least one placeholder such ",
          "as `{mean}` and must not contain unmatched braces."
        ),
        formats[which(invalid_template)[1L]]
      )
    )
  }

  selection <- resolve_variables(
    data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )
  registry <- attr(data, "summary_formats")
  registry <- registry[!registry$var_id %in% selection$var_id, ]
  records <- lapply(selection$var_id, function(var_id) {
    tibble::tibble(
      var_id = rep(var_id, length(formats)),
      format_name = unname(format_names),
      template = unname(formats),
      position = seq_along(formats)
    )
  })
  attr(data, "summary_formats") <- dplyr::bind_rows(
    c(list(registry), records)
  )

  data
}

#' Inspect summary presentation formats
#'
#' Returns the flat registry containing the ordered templates attached to
#' variables. `var_id` links each row to [variables()].
#'
#' @param data A `bq_data` object.
#'
#' @return A tibble with columns `var_id`, `format_name`, `template` and
#'   `position`.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55)))
#' data <- set_summary_format(data, age, "{mean} ({sd})")
#' summary_formats(data)
summary_formats <- function(data) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  attr(data, "summary_formats")
}
