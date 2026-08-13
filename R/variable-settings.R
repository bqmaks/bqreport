valid_variable_types <- c(
  "continuous", "binary", "ordinal", "nominal", "count", "date",
  "datetime", "identifier", "unknown"
)

#' Configure descriptive statistic display templates
#'
#' Templates are stored as metadata and are not evaluated at configuration
#' time. Each template represents one display row. Placeholders such as
#' `"{mean} ({sd})"` refer to unrounded statistics that will be supplied by the
#' descriptive analysis and formatted only by the presentation layer.
#' Model-based fields use the same contract, for example
#' `"{estimate} ({conf.low}; {conf.high})"`. Requesting such fields does not
#' store fitted values in variable metadata: a descriptive analysis plan must
#' declare and run the model that supplies them. For categorical variables,
#' `{n}` is the level count, `{N}` is the non-missing denominator in the
#' corresponding population, and `{p}` is the unrounded proportion `n / N`.
#' Quantitative templates may additionally request `{mad}`, `{skewness}`, and
#' `{kurtosis}`. Skewness uses the adjusted Fisher--Pearson coefficient;
#' kurtosis is bias-corrected excess kurtosis, equal to zero for the reference
#' normal distribution.
#'
#' @param .data A `bq_data` object.
#' @param .cols Columns selected using tidyselect syntax.
#' @param templates A non-empty character vector of templates, or `NULL` to
#'   clear the configured templates. Each template must contain at least one
#'   simple `{statistic}` placeholder. Placeholder names may consist of
#'   identifier components separated by dots, as in `{conf.low}`.
#'
#' @return `.data` with updated variable metadata.
#' @export
set_descriptive_statistics <- function(.data, .cols, templates) {
  check_bq_data(.data)
  if (!is.null(templates)) {
    templates <- validate_descriptive_templates(templates)
  }

  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(names(selected), registry$name)
  for (row in rows) {
    registry$descriptive_templates[row] <- list(templates)
  }
  attr(.data, "variable_registry") <- registry
  .data
}

validate_descriptive_templates <- function(templates) {
  valid <- is.character(templates) && length(templates) > 0L &&
    !anyNA(templates) && all(nzchar(trimws(templates)))
  if (!valid) {
    stop_invalid_descriptive_statistics(
      "`templates` must be a non-empty character vector without missing or empty values."
    )
  }

  placeholder_pattern <- paste0(
    "\\{[A-Za-z][A-Za-z0-9_]*",
    "(?:\\.[A-Za-z][A-Za-z0-9_]*)*\\}"
  )
  without_placeholders <- gsub(placeholder_pattern, "", templates, perl = TRUE)
  has_placeholder <- grepl(placeholder_pattern, templates, perl = TRUE)
  balanced <- !grepl("[{}]", without_placeholders)
  if (any(!has_placeholder | !balanced)) {
    stop_invalid_descriptive_statistics(
      paste0(
        "Each template must contain only balanced, simple placeholders such as ",
        "`{mean}`, `{sd}`, or `{conf.low}`."
      )
    )
  }

  templates
}

stop_invalid_descriptive_statistics <- function(message) {
  condition <- structure(
    list(message = message, call = sys.call(-1L)),
    class = c(
      "bq_error_invalid_descriptive_statistics",
      "error",
      "condition"
    )
  )
  stop(condition)
}

#' Define outcome variables
#'
#' @param .data A `bq_data` object.
#' @param .cols Columns selected using tidyselect syntax.
#' @param type An optional analytical variable type. If omitted, the current
#'   inferred or configured type is retained.
#' @param event An optional event value for binary outcomes.
#' @param reference Optional reference category for ordinal or nominal outcomes.
#'
#' @return `.data` with updated variable metadata.
#' @export
set_outcome <- function(.data, .cols, type = NULL, event = NULL, reference = NULL) {
  check_bq_data(.data)
  type <- check_optional_variable_type(type)
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  selected_names <- names(selected)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)

  resolved_types <- if (is.null(type)) registry$type[rows] else rep(type, length(rows))
  if (!is.null(event) && any(resolved_types != "binary")) {
    stop_variable_setting(
      "`event` can only be set for binary outcomes.",
      "bq_error_invalid_outcome"
    )
  }
  if (!is.null(event)) {
    check_scalar_setting(event, "event", "bq_error_invalid_outcome")
  }
  if (!is.null(reference) && any(!resolved_types %in% c("ordinal", "nominal"))) {
    stop_variable_setting(
      "`reference` can only be set for ordinal or nominal outcomes.",
      "bq_error_invalid_outcome"
    )
  }
  if (!is.null(reference)) check_scalar_setting(reference, "reference", "bq_error_invalid_outcome")

  .data <- add_role_by_name(.data, selected_names, "outcome")
  .data <- set_explicit_types(.data, selected_names, type)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)
  if (!is.null(event)) {
    for (row in rows) registry$event_value[[row]] <- event
  }
  if (!is.null(reference)) {
    for (row in rows) registry$reference[[row]] <- reference
  }
  attr(.data, "variable_registry") <- registry
  .data
}

#' Define predictor variables
#'
#' @param .data A `bq_data` object.
#' @param .cols Columns selected using tidyselect syntax.
#' @param type An optional analytical variable type. If omitted, the current
#'   inferred or configured type is retained.
#' @param reference An optional reference value for binary, ordinal, or nominal
#'   predictors.
#'
#' @return `.data` with updated variable metadata.
#' @export
set_predictor <- function(.data, .cols, type = NULL, reference = NULL) {
  check_bq_data(.data)
  type <- check_optional_variable_type(type)
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  selected_names <- names(selected)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)

  resolved_types <- if (is.null(type)) registry$type[rows] else rep(type, length(rows))
  categorical_types <- c("binary", "ordinal", "nominal")
  if (!is.null(reference) && any(!resolved_types %in% categorical_types)) {
    stop_variable_setting(
      "`reference` can only be set for binary, ordinal, or nominal predictors.",
      "bq_error_invalid_predictor"
    )
  }
  if (!is.null(reference)) {
    check_scalar_setting(reference, "reference", "bq_error_invalid_predictor")
  }

  .data <- add_role_by_name(.data, selected_names, "predictor")
  .data <- set_explicit_types(.data, selected_names, type)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)
  if (!is.null(reference)) {
    for (row in rows) registry$reference[[row]] <- reference
  }
  attr(.data, "variable_registry") <- registry
  .data
}

add_role_by_name <- function(.data, selected_names, role) {
  update_roles(.data, selected_names, function(roles) {
    unique(c(setdiff(roles, "auxiliary"), role))
  })
}

set_explicit_types <- function(.data, selected_names, type) {
  if (is.null(type)) {
    return(.data)
  }
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)
  registry$type[rows] <- type
  registry$source[rows] <- "explicit"
  registry$status[rows] <- "valid"
  registry$locked[rows] <- TRUE
  attr(.data, "variable_registry") <- registry
  .data
}

check_optional_variable_type <- function(type) {
  if (is.null(type)) {
    return(NULL)
  }
  valid <- is.character(type) && length(type) == 1L && !is.na(type) &&
    type %in% valid_variable_types && type != "unknown"
  if (!valid) {
    stop_variable_setting(
      paste0(
        "`type` must be one of: ",
        paste(setdiff(valid_variable_types, "unknown"), collapse = ", "),
        "."
      ),
      "bq_error_invalid_variable_type"
    )
  }
  type
}

check_scalar_setting <- function(x, argument, class) {
  if (length(x) != 1L || is.na(x)) {
    stop_variable_setting(
      paste0("`", argument, "` must be a single non-missing value."),
      class
    )
  }
  invisible(x)
}

stop_variable_setting <- function(message, class) {
  condition <- structure(
    list(message = message, call = sys.call(-1L)),
    class = c(class, "error", "condition")
  )
  stop(condition)
}

categorical_levels <- function(x) {
  value <- analysis_vector(x)
  if (is.factor(value)) return(levels(value))
  unique(as.character(value[!special_missing_mask(x)]))
}

#' Set colors for categorical variables
#'
#' `colors` may be an unnamed character vector in level order, a named vector
#' keyed by level, or a function receiving the current character vector of
#' levels and returning either form. Functional specifications are retained in
#' metadata while their resolved mapping is snapshotted for reproducibility.
#'
#' @param .data A `bq_data` object.
#' @param .cols Categorical columns selected with tidyselect.
#' @param colors A character vector or palette function.
#' @return `.data` with updated variable metadata.
#' @export
set_colors <- function(.data, .cols, colors) {
  check_bq_data(.data)
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(names(selected), registry$name)
  if (any(!registry$type[rows] %in% c("binary", "ordinal", "nominal"))) {
    stop_invalid_colors("Colors can only be set for binary, ordinal, or nominal variables.")
  }
  for (i in seq_along(rows)) {
    levels <- categorical_levels(.data[[names(selected)[[i]]]])
    registry$colors[[rows[[i]]]] <- new_color_spec(colors, levels)
  }
  attr(.data, "variable_registry") <- registry
  .data
}

new_color_spec <- function(colors, levels) {
  input <- colors
  resolved <- if (is.function(colors)) {
    tryCatch(colors(levels), error = function(error) {
      stop_invalid_colors(paste0("Palette function failed: ", conditionMessage(error)))
    })
  } else colors
  if (!is.character(resolved) || length(resolved) != length(levels) ||
      anyNA(resolved) || any(!nzchar(resolved))) {
    stop_invalid_colors("Colors must resolve to one non-missing string per level.")
  }
  valid_colors <- tryCatch({
    grDevices::col2rgb(resolved)
    TRUE
  }, error = function(error) FALSE)
  if (!valid_colors) {
    stop_invalid_colors("Every supplied color must be understood by R graphics.")
  }
  if (!is.null(names(resolved)) && any(nzchar(names(resolved)))) {
    if (any(!nzchar(names(resolved))) || anyDuplicated(names(resolved)) ||
        !setequal(names(resolved), levels)) {
      stop_invalid_colors("Named colors must contain each level exactly once.")
    }
    resolved <- resolved[levels]
  } else names(resolved) <- levels
  structure(list(
    input = input, resolved = unname(resolved) |> stats::setNames(levels),
    function_hash = if (is.function(input)) digest::digest(input) else NA_character_
  ), class = "bq_color_spec")
}

#' Resolve colors registered for a categorical variable
#' @param .data A `bq_data` object.
#' @param .col Exactly one column selected with tidyselect.
#' @return A named character vector or `NULL`.
#' @export
variable_colors <- function(.data, .col) {
  check_bq_data(.data)
  selected <- tidyselect::eval_select(rlang::enquo(.col), .data)
  if (length(selected) != 1L) stop_invalid_colors("`.col` must select exactly one variable.")
  registry <- variables(.data)
  spec <- registry$colors[[match(names(selected), registry$name)]]
  if (is.null(spec)) NULL else spec$resolved
}

stop_invalid_colors <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_colors", "error", "condition")))
}
