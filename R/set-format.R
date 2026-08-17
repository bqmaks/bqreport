#' Set measurement units for variables
#'
#' Records the unit in which one or more variables are expressed. The values
#' themselves are not converted. Units are value-dependent metadata and are
#' cleared if a column is later overwritten.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more columns.
#' @param unit One non-missing, non-empty character value, such as `"years"`,
#'   `"kg/m^2"` or `"mmHg"`.
#'
#' @return `data` with updated unit metadata.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55), bmi = c(22, 31)))
#' data <- set_unit(data, age, "years")
#' set_unit(data, bmi, "kg/m^2")
set_unit <- function(data, variables, unit) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  if (
    missing(unit) || !is.character(unit) || length(unit) != 1L ||
      is.na(unit) || !nzchar(unit)
  ) {
    bq_abort(
      "bq_error_invalid_unit",
      "`unit` must be one non-missing, non-empty character value."
    )
  }

  selection <- resolve_variables(
    data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )
  registry <- attr(data, "variables")
  registry$unit[match(selection$var_id, registry$var_id)] <- unit
  attr(data, "variables") <- registry

  data
}

#' Set a rounding policy for variables
#'
#' Records how numeric estimates for one or more variables should eventually
#' be presented. `"decimal"` means a fixed number of digits after the decimal
#' point; `"significant"` means a number of significant digits. Stored values
#' and analytic results are never rounded by this setter.
#'
#' The policy is value-dependent metadata and is cleared if a column is later
#' overwritten. Formatting of counts, p-values and other dimensionless output
#' belongs to their own result components rather than to this variable-level
#' policy.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more columns.
#' @param digits Number of decimal places or significant digits. Decimal places
#'   may be zero; significant digits must be at least one.
#' @param method Either `"decimal"` or `"significant"`.
#'
#' @return `data` with updated rounding metadata.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55), marker = c(0.0123, 0.0456)))
#' data <- set_rounding(data, age, digits = 0)
#' set_rounding(data, marker, digits = 3, method = "significant")
set_rounding <- function(data, variables, digits, method = "decimal") {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  if (
    !is.character(method) || length(method) != 1L || is.na(method) ||
      !method %in% c("decimal", "significant")
  ) {
    bq_abort(
      "bq_error_invalid_rounding",
      "`method` must be either \"decimal\" or \"significant\"."
    )
  }

  minimum_digits <- if (method == "decimal") 0L else 1L
  if (
    missing(digits) || !is.numeric(digits) || length(digits) != 1L ||
      is.na(digits) || !is.finite(digits) || digits != floor(digits) ||
      digits < minimum_digits || digits > .Machine$integer.max
  ) {
    requirement <- if (method == "decimal") {
      "a non-negative whole number"
    } else {
      "a positive whole number"
    }
    bq_abort(
      "bq_error_invalid_rounding",
      sprintf("`digits` must be %s for method \"%s\".", requirement, method)
    )
  }

  selection <- resolve_variables(
    data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )
  registry <- attr(data, "variables")
  rows <- match(selection$var_id, registry$var_id)
  registry$rounding[rows] <- method
  registry$digits[rows] <- as.integer(digits)
  attr(data, "variables") <- registry

  data
}
