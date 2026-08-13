#' Construct a custom descriptive statistic provider
#'
#' A descriptive function receives a read-only context for one variable in one
#' population and returns one row per declared field. Providers are explicit:
#' they are compiled into the analysis plan and never discovered from template
#' names at runtime.
#'
#' @param id Stable provider identifier.
#' @param fields Unique statistic names supplied by the provider.
#' @param compute Function accepting a `descriptive_context` and returning a
#'   data frame with `statistic`, `value`, and `statistic_method` columns.
#' @param types Supported analytical variable types.
#' @param required_packages Packages required to execute the provider.
#' @param source Result source: `custom`, `diagnostic`, or `model`.
#'
#' @return A `descriptive_function` specification.
#' @export
descriptive_function <- function(
  id,
  fields,
  compute,
  types = c("continuous", "count"),
  required_packages = character(),
  source = c("custom", "diagnostic", "model")
) {
  source <- match.arg(source)
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop_descriptive_function("`id` must be one non-empty string.")
  }
  valid_fields <- is.character(fields) && length(fields) > 0L &&
    !anyNA(fields) && all(grepl(
      "^[A-Za-z][A-Za-z0-9_]*(?:\\.[A-Za-z][A-Za-z0-9_]*)*$",
      fields,
      perl = TRUE
    )) && !anyDuplicated(fields)
  if (!valid_fields) {
    stop_descriptive_function(
      "`fields` must be a non-empty vector of unique statistic identifiers."
    )
  }
  valid_types <- setdiff(valid_variable_types, "unknown")
  if (!is.character(types) || length(types) == 0L || anyNA(types) ||
      any(!types %in% valid_types) || anyDuplicated(types)) {
    stop_descriptive_function(
      "`types` must contain unique supported analytical variable types."
    )
  }
  if (!is.function(compute)) {
    stop_descriptive_function("`compute` must be a function.")
  }
  if (!is.character(required_packages) || anyNA(required_packages) ||
      any(!nzchar(required_packages))) {
    stop_descriptive_function(
      "`required_packages` must be a character vector without missing values."
    )
  }
  structure(list(
    id = id,
    fields = fields,
    compute = compute,
    types = types,
    required_packages = unique(required_packages),
    source = source,
    function_hash = digest::digest(compute)
  ), class = "descriptive_function")
}

#' Configure the Shapiro--Wilk normality diagnostic
#'
#' The provider returns `shapiro.statistic` and `shapiro.p.value`. It records
#' non-computable populations explicitly and never updates distribution
#' metadata or selects a downstream analysis method.
#'
#' @param id Stable provider identifier.
#'
#' @return A `descriptive_function` specification.
#' @export
shapiro_wilk <- function(id = "shapiro_wilk") {
  descriptive_function(
    id = id,
    fields = c("shapiro.statistic", "shapiro.p.value"),
    types = c("continuous", "count"),
    source = "diagnostic",
    compute = function(context) {
      n <- length(context$values)
      reason <- if (n < 3L) {
        "Shapiro-Wilk requires at least 3 non-missing observations."
      } else if (n > 5000L) {
        "Shapiro-Wilk supports at most 5000 non-missing observations."
      } else if (length(unique(context$values)) < 2L) {
        "Shapiro-Wilk is not defined for a constant variable."
      } else {
        NA_character_
      }
      if (!is.na(reason)) {
        return(tibble::tibble(
          statistic = c("shapiro.statistic", "shapiro.p.value"),
          value = c(NA_real_, NA_real_),
          statistic_method = "shapiro_wilk",
          status = "not_computed",
          message = reason
        ))
      }
      test <- stats::shapiro.test(context$values)
      tibble::tibble(
        statistic = c("shapiro.statistic", "shapiro.p.value"),
        value = c(unname(test$statistic), test$p.value),
        statistic_method = "shapiro_wilk",
        status = "observed",
        message = NA_character_
      )
    }
  )
}

normalize_descriptive_function_output <- function(output, provider, context) {
  if (!inherits(output, "data.frame")) {
    stop_descriptive_output("A descriptive function must return a data frame.")
  }
  output <- tibble::as_tibble(output)
  required <- c("statistic", "value", "statistic_method")
  missing <- setdiff(required, names(output))
  if (length(missing)) {
    stop_descriptive_output(paste0(
      "Descriptive output is missing columns: ",
      paste(missing, collapse = ", "), "."
    ))
  }
  valid_statistics <- is.character(output$statistic) &&
    !anyNA(output$statistic) && !anyDuplicated(output$statistic) &&
    setequal(output$statistic, provider$fields)
  if (!valid_statistics) {
    stop_descriptive_output(paste0(
      "Provider `", provider$id,
      "` must return each declared field exactly once."
    ))
  }
  if (!is.numeric(output$value)) {
    stop_descriptive_output("`value` must be numeric.")
  }
  if (!is.character(output$statistic_method) ||
      length(output$statistic_method) != nrow(output) ||
      anyNA(output$statistic_method) || any(!nzchar(output$statistic_method))) {
    stop_descriptive_output(
      "`statistic_method` must contain one non-missing string per result row."
    )
  }
  optional_defaults <- list(
    numerator = rep(NA_integer_, nrow(output)),
    denominator = rep(NA_integer_, nrow(output)),
    status = rep("observed", nrow(output)),
    message = rep(NA_character_, nrow(output))
  )
  for (name in names(optional_defaults)) {
    if (!name %in% names(output)) output[[name]] <- optional_defaults[[name]]
  }
  valid_status <- is.character(output$status) &&
    length(output$status) == nrow(output) && !anyNA(output$status) &&
    all(output$status %in% c("observed", "not_computed", "warning"))
  if (!valid_status) {
    stop_descriptive_output(
      "`status` must contain observed, not_computed, or warning."
    )
  }
  if (!is.character(output$message) || length(output$message) != nrow(output)) {
    stop_descriptive_output("`message` must be a character vector.")
  }
  tryCatch(
    descriptive_rows(
      context$spec,
      context$population,
      level = rep(NA_character_, nrow(output)),
      statistic = output$statistic,
      value = output$value,
      numerator = output$numerator,
      denominator = output$denominator,
      statistic_method = output$statistic_method,
      source = provider$source,
      method = provider$id,
      status = output$status,
      message = output$message
    ),
    error = function(error) {
      stop_descriptive_output(
        "Descriptive output contains incompatible column types."
      )
    }
  )
}

stop_descriptive_function <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_descriptive_function", "error", "condition")
  ))
}

stop_descriptive_output <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_descriptive_output", "error", "condition")
  ))
}

stop_duplicate_descriptive_field <- function(fields) {
  stop(structure(
    list(
      message = paste0(
        "Descriptive fields must have one provider: ",
        paste(unique(fields), collapse = ", "), "."
      ),
      call = sys.call(-1L)
    ),
    class = c("bq_error_duplicate_descriptive_field", "error", "condition")
  ))
}
