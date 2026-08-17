#' Set rounding for statistic components
#'
#' Records an explicit presentation policy for selected components of a custom
#' statistic. Variable-scale components otherwise inherit the policy of their
#' source variable. Dimensionless components require an explicit policy before
#' analysis, while count components always use integer formatting and cannot be
#' changed by this setter.
#'
#' @param statistic A `bq_continuous_statistic` specification.
#' @param components Character vector naming one or more returned components.
#' @param digits Number of decimal places or significant digits. Decimal places
#'   may be zero; significant digits must be at least one.
#' @param method Either `"decimal"` or `"significant"`.
#'
#' @return A copy of `statistic` with updated component rounding metadata.
#' @export
#' @examples
#' statistic <- continuous_statistic(
#'   "summary",
#'   function(x) {
#'     data.frame(
#'       mean = if (length(x) == 0L) NA_real_ else mean(x),
#'       cv = NA_real_
#'     )
#'   },
#'   scale = c(mean = "variable", cv = "dimensionless")
#' )
#' set_component_rounding(statistic, "cv", 3, "significant")
set_component_rounding <- function(
  statistic,
  components,
  digits,
  method = "decimal"
) {
  expected_fields <- c(
    "kind", "name", "components", "component_types", "component_scales",
    "component_rounding", "component_digits", "source", "missing", "fun"
  )
  valid_statistic <- inherits(statistic, "bq_continuous_statistic") &&
    inherits(statistic, "bq_statistic") &&
    identical(names(statistic), expected_fields) &&
    is.character(statistic$name) && length(statistic$name) == 1L &&
    !is.na(statistic$name) && nzchar(statistic$name) &&
    is.character(statistic$components) &&
    length(statistic$components) > 0L &&
    !anyNA(statistic$components) && all(nzchar(statistic$components)) &&
    !anyDuplicated(statistic$components) &&
    is.character(statistic$component_types) &&
    identical(names(statistic$component_types), statistic$components) &&
    length(statistic$component_types) == length(statistic$components) &&
    all(statistic$component_types %in% c("double", "integer")) &&
    is.character(statistic$component_scales) &&
    identical(names(statistic$component_scales), statistic$components) &&
    length(statistic$component_scales) == length(statistic$components) &&
    all(
      statistic$component_scales %in%
        c("variable", "count", "dimensionless")
    ) &&
    all(
      statistic$component_scales != "count" |
        statistic$component_types == "integer"
    ) &&
    is.character(statistic$component_rounding) &&
    identical(names(statistic$component_rounding), statistic$components) &&
    length(statistic$component_rounding) == length(statistic$components) &&
    all(
      is.na(statistic$component_rounding) |
        statistic$component_rounding %in% c("decimal", "significant")
    ) &&
    is.integer(statistic$component_digits) &&
    identical(names(statistic$component_digits), statistic$components) &&
    length(statistic$component_digits) == length(statistic$components) &&
    all(
      is.na(statistic$component_rounding) ==
        is.na(statistic$component_digits)
    ) &&
    all(
      is.na(statistic$component_digits) |
        (statistic$component_rounding == "decimal" &
          statistic$component_digits >= 0L) |
        (statistic$component_rounding == "significant" &
          statistic$component_digits >= 1L)
    ) &&
    all(
      statistic$component_scales != "count" |
        is.na(statistic$component_rounding)
    ) &&
    is.function(statistic$fun)

  if (!valid_statistic) {
    bq_abort(
      "bq_error_invalid_statistic",
      "`statistic` must be created by `continuous_statistic()`."
    )
  }

  if (
    !is.character(components) || length(components) == 0L ||
      anyNA(components) || any(!nzchar(components)) ||
      anyDuplicated(components)
  ) {
    bq_abort(
      "bq_error_invalid_statistic",
      "`components` must name one or more unique statistic components."
    )
  }

  unknown_components <- setdiff(components, statistic$components)
  if (length(unknown_components) > 0L) {
    bq_abort(
      "bq_error_invalid_statistic",
      sprintf(
        "Component `%s` is not returned by statistic `%s`.",
        unknown_components[1L],
        statistic$name
      )
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

  selected <- match(components, statistic$components)
  count_components <- statistic$component_scales[selected] == "count"
  if (any(count_components)) {
    component <- components[which(count_components)[1L]]
    bq_abort(
      "bq_error_invalid_rounding",
      sprintf(
        "Count component `%s` always uses integer formatting and cannot be overridden.",
        component
      )
    )
  }

  statistic$component_rounding[selected] <- method
  statistic$component_digits[selected] <- as.integer(digits)
  statistic
}
