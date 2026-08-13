#' Build a backend-independent descriptive table
#'
#' `tbl_descriptive()` applies registered display templates to numerical
#' descriptive results. It does not modify or round values stored in the
#' `analysis_result`; formatting exists only in the returned table model.
#'
#' @param x An `analysis_result` containing descriptive tasks.
#' @param overall_label Display label for the overall population.
#' @param locale Output locale, `en` or `ru`.
#' @param missing Text used for unavailable statistics.
#' @param digits Default number of decimal places when variable metadata do not
#'   provide `digits`.
#' @param percent_digits Decimal places for `{p}`, which is displayed as a
#'   percentage.
#' @param p_value_digits Decimal places for placeholders ending in `.p.value` or
#'   named `p.value`.
#'
#' @return A `tbl_descriptive` and `bq_table` object.
#' @export
tbl_descriptive <- function(
  x,
  overall_label = NULL,
  missing = "NA",
  digits = 2L,
  percent_digits = 1L,
  p_value_digits = 3L,
  locale = "en"
) {
  check_analysis_result(x)
  locale <- check_reporting_locale(locale)
  if (is.null(overall_label)) {
    overall_label <- if (locale == "ru") "\u0412\u0441\u0435 \u043f\u0430\u0446\u0438\u0435\u043d\u0442\u044b" else "All patients"
  }
  check_table_string(overall_label, "overall_label")
  check_table_string(missing, "missing")
  digits <- check_table_digits(digits, "digits")
  percent_digits <- check_table_digits(percent_digits, "percent_digits")
  p_value_digits <- check_table_digits(p_value_digits, "p_value_digits")
  plan <- tibble::as_tibble(x$plan)
  plan <- plan[plan$analysis_type == "descriptive", , drop = FALSE]
  if (nrow(plan) == 0L) {
    stop_invalid_table("`x` contains no descriptive analysis tasks.")
  }
  values <- descriptives(x)
  header <- descriptive_table_header(values, overall_label)
  body_rows <- lapply(seq_len(nrow(plan)), function(i) {
    build_descriptive_table_rows(
      plan[i, , drop = FALSE], values, header, missing, digits,
      percent_digits, p_value_digits
    )
  })
  body <- vctrs::vec_rbind(!!!body_rows)
  footnotes <- values[
    values$status != "observed" & !is.na(values$message),
    c("analysis_id", "variable", "group_level", "statistic", "message"),
    drop = FALSE
  ]
  footnotes <- unique(tibble::as_tibble(footnotes))
  structure(list(
    table_body = body,
    table_header = header,
    footnotes = footnotes,
    call = match.call()
  ), class = c("tbl_descriptive", "bq_table"))
}

descriptive_table_header <- function(values, overall_label) {
  populations <- values[c("overall", "group_level")]
  populations <- unique(populations)
  populations <- populations[order(!populations$overall), , drop = FALSE]
  tibble::tibble(
    column = paste0("stat_", seq_len(nrow(populations))),
    label = ifelse(
      populations$overall,
      overall_label,
      populations$group_level
    ),
    overall = populations$overall,
    group_level = populations$group_level
  )
}

build_descriptive_table_rows <- function(
  spec, values, header, missing, default_digits, percent_digits,
  p_value_digits
) {
  task_values <- values[values$analysis_id == spec$analysis_id[[1]], , drop = FALSE]
  templates <- spec$descriptive_templates[[1]]
  variable_type <- spec$variable_type[[1]]
  rows <- list()
  for (template_index in seq_along(templates)) {
    template <- templates[[template_index]]
    placeholders <- descriptive_placeholders(template)
    row_levels <- if (
      variable_type %in% c("binary", "ordinal", "nominal") &&
      any(placeholders %in% c("n", "N", "p"))
    ) {
      unique(task_values$level[!is.na(task_values$level)])
    } else {
      NA_character_
    }
    for (level in row_levels) {
      row <- tibble::tibble(
        analysis_id = spec$analysis_id[[1]],
        variable_id = spec$variable_id[[1]],
        variable = spec$variable[[1]],
        variable_label = if (is.na(spec$variable_label[[1]])) {
          spec$variable[[1]]
        } else {
          spec$variable_label[[1]]
        },
        unit = spec$variable_unit[[1]],
        level = level,
        template_id = as.integer(template_index),
        template = template
      )
      for (column_index in seq_len(nrow(header))) {
        population_values <- task_values[
          task_values$overall == header$overall[[column_index]] &
            if (header$overall[[column_index]]) {
              TRUE
            } else {
              !is.na(task_values$group_level) &
                task_values$group_level == header$group_level[[column_index]]
            },
          , drop = FALSE
        ]
        row[[header$column[[column_index]]]] <- render_descriptive_template(
          template, placeholders, population_values, level,
          if (is.na(spec$variable_digits[[1]])) {
            default_digits
          } else {
            spec$variable_digits[[1]]
          },
          percent_digits, p_value_digits, missing
        )
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  vctrs::vec_rbind(!!!rows)
}

render_descriptive_template <- function(
  template, placeholders, values, level, digits, percent_digits,
  p_value_digits, missing
) {
  rendered <- template
  for (placeholder in placeholders) {
    candidates <- values[
      values$statistic == placeholder &
        if (is.na(level)) is.na(values$level) else values$level == level,
      , drop = FALSE
    ]
    value <- if (nrow(candidates) == 1L) candidates$value[[1]] else NA_real_
    replacement <- format_descriptive_value(
      value, placeholder, digits, percent_digits, p_value_digits, missing
    )
    rendered <- gsub(
      paste0("{", placeholder, "}"), replacement, rendered, fixed = TRUE
    )
  }
  rendered
}

format_descriptive_value <- function(
  value, statistic, digits, percent_digits, p_value_digits, missing
) {
  if (length(value) != 1L || is.na(value) || !is.finite(value)) return(missing)
  if (statistic %in% c("n", "N", "n_missing")) {
    return(formatC(value, format = "f", digits = 0L))
  }
  if (statistic == "p") {
    return(formatC(100 * value, format = "f", digits = percent_digits))
  }
  if (identical(statistic, "p.value") || grepl("\\.p\\.value$", statistic)) {
    return(formatC(value, format = "f", digits = p_value_digits))
  }
  formatC(value, format = "f", digits = digits)
}

#' Access a backend-independent table component
#'
#' @param x A `bq_table` object.
#'
#' @return A tibble.
#' @export
table_body <- function(x) {
  check_bq_table(x)
  x$table_body
}

#' @rdname table_body
#' @export
table_header <- function(x) {
  check_bq_table(x)
  x$table_header
}

#' @export
print.bq_table <- function(x, ...) {
  check_bq_table(x)
  print(x$table_body, ...)
  invisible(x)
}

check_bq_table <- function(x) {
  if (!inherits(x, "bq_table")) {
    stop_invalid_table("`x` must be a bq_table object.")
  }
  invisible(x)
}

check_table_string <- function(x, argument) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop_invalid_table(paste0("`", argument, "` must be one non-empty string."))
  }
  x
}

check_table_digits <- function(x, argument) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 0 || x != floor(x)) {
    stop_invalid_table(paste0(
      "`", argument, "` must be one non-negative whole number."
    ))
  }
  as.integer(x)
}

stop_invalid_table <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_table", "error", "condition")
  ))
}
