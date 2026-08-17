#' Format prepared analysis results
#'
#' Applies declared numeric presentation policies without changing raw
#' estimates or enumerated values. Measurement units remain separate variable
#' metadata and are not repeated after individual numbers.
#'
#' @param presentation A `bq_presentation` object.
#' @param fallback_digits Optional digits used when a variable-scale value has
#'   no explicit variable or component policy. `NULL` rejects such values.
#' @param fallback_method Either `"decimal"` or `"significant"`.
#' @param decimal_mark One character used as the decimal mark.
#' @param value_separator `NULL` for an automatic separator, or a character
#'   scalar used between enumerated values. The automatic separator is `"; "`
#'   for a decimal comma and `", "` otherwise.
#' @param trim_trailing_zeros Whether zeros at the end of a fractional part
#'   should be removed after formatting.
#' @param missing Text used for missing numeric results and all-missing cells.
#' @param empty Text used for structurally empty cells.
#'
#' @return A `bq_formatted_presentation` object.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(value = c(1.2, 3)))
#' data <- set_type(data, value, continuous())
#' data <- set_rounding(data, value, 2, "decimal")
#' statistic <- continuous_statistic(
#'   "mean",
#'   function(x) data.frame(mean = mean(x))
#' )
#' plan <- plan_summary(data) |>
#'   add_statistic(value, statistic)
#' plan <- add_display_rule(plan, value, enumerate_values())
#' presentation <- prepare_presentation(run_analysis(plan))
#' format_presentation(
#'   presentation,
#'   decimal_mark = ",",
#'   trim_trailing_zeros = TRUE
#' )
format_presentation <- function(
  presentation,
  fallback_digits = NULL,
  fallback_method = "decimal",
  decimal_mark = ".",
  value_separator = NULL,
  trim_trailing_zeros = FALSE,
  missing = "NA",
  empty = "\u2014"
) {
  if (!inherits(presentation, "bq_presentation")) {
    bq_abort(
      "bq_error_invalid_presentation",
      sprintf(
        "`presentation` must be a bq_presentation object, not %s.",
        class(presentation)[1L]
      )
    )
  }

  UseMethod("format_presentation")
}

#' Format prepared summary results
#'
#' @param presentation A `bq_presentation_summary` object.
#' @inheritParams format_presentation
#'
#' @return A `bq_formatted_presentation_summary` object.
#' @exportS3Method
#' @noRd
format_presentation.bq_presentation_summary <- function(
  presentation,
  fallback_digits = NULL,
  fallback_method = "decimal",
  decimal_mark = ".",
  value_separator = NULL,
  trim_trailing_zeros = FALSE,
  missing = "NA",
  empty = "\u2014"
) {
  if (
    !is.character(fallback_method) || length(fallback_method) != 1L ||
      is.na(fallback_method) ||
      !fallback_method %in% c("decimal", "significant")
  ) {
    bq_abort(
      "bq_error_invalid_rounding",
      "`fallback_method` must be either \"decimal\" or \"significant\"."
    )
  }

  if (!is.null(fallback_digits)) {
    minimum_digits <- if (fallback_method == "decimal") 0L else 1L
    if (
      !is.numeric(fallback_digits) || length(fallback_digits) != 1L ||
        is.na(fallback_digits) || !is.finite(fallback_digits) ||
        fallback_digits != floor(fallback_digits) ||
        fallback_digits < minimum_digits ||
        fallback_digits > .Machine$integer.max
    ) {
      requirement <- if (fallback_method == "decimal") {
        "a non-negative whole number"
      } else {
        "a positive whole number"
      }
      bq_abort(
        "bq_error_invalid_rounding",
        sprintf(
          "`fallback_digits` must be %s for method \"%s\".",
          requirement,
          fallback_method
        )
      )
    }
    fallback_digits <- as.integer(fallback_digits)
  }

  if (
    !is.character(decimal_mark) || length(decimal_mark) != 1L ||
      is.na(decimal_mark) || nchar(decimal_mark, type = "chars") != 1L ||
      grepl("[[:alnum:][:space:]+-]", decimal_mark)
  ) {
    bq_abort(
      "bq_error_invalid_format",
      paste0(
        "`decimal_mark` must be one non-alphanumeric, non-whitespace ",
        "character."
      )
    )
  }

  if (is.null(value_separator)) {
    value_separator <- if (decimal_mark == ",") "; " else ", "
  } else if (
    !is.character(value_separator) || length(value_separator) != 1L ||
      is.na(value_separator)
  ) {
    bq_abort(
      "bq_error_invalid_format",
      "`value_separator` must be NULL or one non-missing character value."
    )
  }

  if (
    !is.logical(trim_trailing_zeros) || length(trim_trailing_zeros) != 1L ||
      is.na(trim_trailing_zeros)
  ) {
    bq_abort(
      "bq_error_invalid_format",
      "`trim_trailing_zeros` must be either TRUE or FALSE."
    )
  }

  for (argument in c("missing", "empty")) {
    value <- get(argument)
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      bq_abort(
        "bq_error_invalid_format",
        sprintf("`%s` must be one non-missing character value.", argument)
      )
    }
  }

  format_number <- function(value, method, digits) {
    if (is.na(value)) {
      return(missing)
    }

    rounded <- if (method == "decimal") {
      round(value, digits)
    } else {
      signif(value, digits)
    }
    if (!is.na(rounded) && rounded == 0) {
      rounded <- 0
    }

    text <- if (method == "decimal") {
      formatC(rounded, format = "f", digits = digits, decimal.mark = ".")
    } else {
      sprintf("%#.*g", digits, rounded)
    }

    exponent <- ""
    exponent_start <- regexpr("[eE]", text)
    if (exponent_start[1L] > 0L) {
      exponent <- substring(text, exponent_start[1L])
      text <- substring(text, 1L, exponent_start[1L] - 1L)
    }

    if (trim_trailing_zeros && grepl(".", text, fixed = TRUE)) {
      text <- sub("0+$", "", text)
      text <- sub("[.]$", "", text)
    }

    if (decimal_mark != ".") {
      text <- sub(".", decimal_mark, text, fixed = TRUE)
    }
    paste0(text, exponent)
  }

  result <- presentation$result
  plan <- result$plan
  registry <- attr(plan$data, "variables")
  formatted_estimate_records <- list()
  formatted_value_records <- list()
  enumeration_records <- list()

  for (estimate_row in seq_len(nrow(result$estimates))) {
    cell_id <- result$estimates$cell_id[estimate_row]
    var_id <- result$estimates$var_id[estimate_row]
    statistic_id <- result$estimates$statistic_id[estimate_row]
    component <- result$estimates$component[estimate_row]
    value <- result$estimates$value[estimate_row]
    component_row <- which(
      plan$statistic_components$statistic_id == statistic_id &
        plan$statistic_components$component == component
    )
    scale <- plan$statistic_components$scale[component_row]

    if (scale == "count") {
      method <- "decimal"
      digits <- 0L
    } else {
      method <- plan$statistic_components$rounding[component_row]
      digits <- plan$statistic_components$digits[component_row]

      if (scale == "variable" && is.na(method)) {
        variable_row <- match(var_id, registry$var_id)
        method <- registry$rounding[variable_row]
        digits <- registry$digits[variable_row]
      }

      if (is.na(method)) {
        if (is.null(fallback_digits)) {
          bq_abort(
            "bq_error_missing_rounding",
            sprintf(
              paste0(
                "Component `%s` for variable `%s` has no rounding policy; ",
                "set one or supply `fallback_digits`."
              ),
              component,
              registry$name[match(var_id, registry$var_id)]
            ),
            cell_id = cell_id,
            var_id = var_id,
            statistic_id = statistic_id,
            component = component
          )
        }
        method <- fallback_method
        digits <- fallback_digits
      }
    }

    formatted_estimate_records[[estimate_row]] <- tibble::tibble(
      cell_id = cell_id,
      var_id = var_id,
      statistic_id = statistic_id,
      component = component,
      value = format_number(value, method, digits)
    )
  }

  for (value_row in seq_len(nrow(presentation$display_values))) {
    cell_id <- presentation$display_values$cell_id[value_row]
    var_id <- presentation$display_values$var_id[value_row]
    variable_row <- match(var_id, registry$var_id)
    method <- registry$rounding[variable_row]
    digits <- registry$digits[variable_row]

    if (is.na(method)) {
      if (is.null(fallback_digits)) {
        bq_abort(
          "bq_error_missing_rounding",
          sprintf(
            paste0(
              "Enumerated values for variable `%s` have no rounding policy; ",
              "set one or supply `fallback_digits`."
            ),
            registry$name[variable_row]
          ),
          cell_id = cell_id,
          var_id = var_id
        )
      }
      method <- fallback_method
      digits <- fallback_digits
    }

    formatted_value_records[[value_row]] <- tibble::tibble(
      cell_id = cell_id,
      var_id = var_id,
      position = presentation$display_values$position[value_row],
      value = format_number(
        presentation$display_values$value[value_row],
        method,
        digits
      )
    )
  }

  formatted_estimates <- dplyr::bind_rows(
    c(
      list(tibble::tibble(
        cell_id = character(),
        var_id = character(),
        statistic_id = character(),
        component = character(),
        value = character()
      )),
      formatted_estimate_records
    )
  )
  formatted_values <- dplyr::bind_rows(
    c(
      list(tibble::tibble(
        cell_id = character(),
        var_id = character(),
        position = integer(),
        value = character()
      )),
      formatted_value_records
    )
  )

  visible_values <- presentation$display_cells[
    presentation$display_cells$show_values,
    c("cell_id", "var_id")
  ]
  for (display_row in seq_len(nrow(visible_values))) {
    cell_id <- visible_values$cell_id[display_row]
    var_id <- visible_values$var_id[display_row]
    values <- formatted_values$value[
      formatted_values$cell_id == cell_id &
        formatted_values$var_id == var_id
    ]
    enumeration_records[[display_row]] <- tibble::tibble(
      cell_id = cell_id,
      var_id = var_id,
      value = paste(values, collapse = value_separator)
    )
  }

  enumerations <- dplyr::bind_rows(
    c(
      list(tibble::tibble(
        cell_id = character(),
        var_id = character(),
        value = character()
      )),
      enumeration_records
    )
  )
  display_cells <- presentation$display_cells
  display_cells$status_text <- NA_character_
  display_cells$status_text[display_cells$status == "all_missing"] <- missing
  display_cells$status_text[display_cells$status == "empty"] <- empty

  structure(
    list(
      analysis = "summary",
      presentation = presentation,
      settings = tibble::tibble(
        fallback_method = fallback_method,
        fallback_digits = if (is.null(fallback_digits)) {
          NA_integer_
        } else {
          fallback_digits
        },
        decimal_mark = decimal_mark,
        value_separator = value_separator,
        trim_trailing_zeros = trim_trailing_zeros,
        missing = missing,
        empty = empty
      ),
      display_cells = display_cells,
      formatted_estimates = formatted_estimates,
      formatted_values = formatted_values,
      enumerations = enumerations
    ),
    class = c(
      "bq_formatted_presentation_summary",
      "bq_formatted_presentation"
    )
  )
}
