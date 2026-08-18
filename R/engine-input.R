#' Validate engine data and context, and count observations
#'
#' Every comparison provider receives the same engine frame (`.row_id`,
#' `.outcome`, `.group`) and a compiled context of identifiers. The contract
#' is checked once here so that providers cannot drift apart in what they
#' accept, and so that a provider is independent of any other one that might
#' have validated the same input earlier in a workflow.
#'
#' Context fields are matched by name, not by position: the context is a
#' named list, and its assembly order is not part of the contract.
#'
#' @param data Engine data.
#' @param context Compiled analysis context.
#' @param analysis_name Public provider name used in diagnostics.
#' @param group_levels Integer vector `c(min, max)` of accepted declared group
#'   levels; `NA` for no upper bound.
#' @param reference Whether `context$reference_value` is required and must
#'   name one declared level.
#' @param estimate_id `"missing"` when `context$estimate_id` must be `NA`,
#'   `"required"` when it must be a non-empty identifier.
#'
#' @return A list with declared group values, `comparison_value` (the level
#'   other than the reference, or `NA` when no reference is used), the
#'   missing-outcome mask, the used-row mask, per-group counts and a
#'   sample-flow tibble.
#' @noRd
prepare_engine_input <- function(
  data,
  context,
  analysis_name,
  group_levels = c(2L, NA_integer_),
  reference = FALSE,
  estimate_id = "missing"
) {
  input_error <- function(...) {
    bq_abort("bq_error_invalid_analysis_input", paste0(...))
  }
  if (
    !tibble::is_tibble(data) ||
      !identical(names(data), c(".row_id", ".outcome", ".group"))
  ) {
    input_error(
      "`data` for `", analysis_name, "()` must be a tibble with columns ",
      "`.row_id`, `.outcome` and `.group`, in that order."
    )
  }
  if (
    anyNA(data$.row_id) || anyDuplicated(data$.row_id) ||
      !is.atomic(data$.row_id) || !is.null(dim(data$.row_id))
  ) {
    input_error("`.row_id` must contain unique, non-missing atomic values.")
  }
  if (
    !is.numeric(data$.outcome) || is.object(data$.outcome) ||
      !is.null(dim(data$.outcome)) ||
      any(!is.finite(data$.outcome[!is.na(data$.outcome)]))
  ) {
    input_error(
      "`.outcome` must be one plain numeric vector with finite observed values."
    )
  }
  if (!is.factor(data$.group) || anyNA(data$.group)) {
    input_error("`.group` must be a factor without missing values.")
  }

  required_context <- c(
    "analysis_id", "test_id", "estimate_id", "outcome_var_id",
    "group_var_id", "strata_var_id", "group_levels",
    if (reference) "reference_value"
  )
  if (
    !is.list(context) || is.null(names(context)) ||
      !setequal(names(context), required_context) ||
      anyDuplicated(names(context))
  ) {
    input_error(
      "`context` for `", analysis_name, "()` must contain exactly ",
      paste0("`", required_context, "`", collapse = ", "), "."
    )
  }
  is_id <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      nzchar(value)
  }
  is_na_id <- function(value) {
    is.character(value) && length(value) == 1L && is.na(value)
  }
  ids <- context[c("analysis_id", "test_id", "outcome_var_id", "group_var_id")]
  if (!all(vapply(ids, is_id, logical(1)))) {
    input_error(
      "Analysis, test and variable IDs must be non-empty character scalars."
    )
  }
  if (estimate_id == "missing" && !is_na_id(context$estimate_id)) {
    input_error(
      "`estimate_id` must be NA because `", analysis_name,
      "()` supplies no separate estimate."
    )
  }
  if (estimate_id == "required" && !is_id(context$estimate_id)) {
    input_error(
      "`estimate_id` must be a non-empty character scalar for the requested ",
      "effect size."
    )
  }
  if (!is_na_id(context$strata_var_id)) {
    input_error(
      "`strata_var_id` must be NA because `", analysis_name,
      "()` does not support strata."
    )
  }

  group_values <- levels(data$.group)
  level_n <- length(group_values)
  if (
    level_n < group_levels[1L] ||
      (!is.na(group_levels[2L]) && level_n > group_levels[2L])
  ) {
    input_error(
      "`", analysis_name, "()` requires ",
      if (is.na(group_levels[2L])) {
        paste0("at least ", group_levels[1L])
      } else if (group_levels[1L] == group_levels[2L]) {
        paste0("exactly ", group_levels[1L])
      } else {
        paste0("between ", group_levels[1L], " and ", group_levels[2L])
      },
      " declared group levels."
    )
  }
  if (
    !tibble::is_tibble(context$group_levels) ||
      !identical(
        names(context$group_levels), c("var_id", "value", "position")
      ) ||
      !is.character(context$group_levels$var_id) ||
      !is.character(context$group_levels$value) ||
      !is.integer(context$group_levels$position) ||
      anyNA(context$group_levels) ||
      !identical(
        context$group_levels$var_id, rep(context$group_var_id, level_n)
      ) ||
      !identical(context$group_levels$value, group_values) ||
      !identical(context$group_levels$position, seq_len(level_n))
  ) {
    input_error(
      "`group_levels` must describe every `.group` level once, in factor ",
      "order, for `group_var_id`."
    )
  }
  comparison_value <- NA_character_
  if (reference) {
    if (
      !is_id(context$reference_value) ||
        !context$reference_value %in% group_values
    ) {
      input_error(
        "`reference_value` must identify one declared `.group` level."
      )
    }
    comparison_value <- setdiff(group_values, context$reference_value)
  }

  missing_outcome <- is.na(data$.outcome)
  n_total <- vapply(group_values, function(value) {
    sum(data$.group == value)
  }, integer(1))
  n_missing <- vapply(group_values, function(value) {
    sum(data$.group == value & missing_outcome)
  }, integer(1))
  n_used <- n_total - n_missing
  if (any(n_used == 0L)) {
    input_error(
      "Group level `", group_values[which(n_used == 0L)[1L]], "` has no ",
      "observed outcome values; provide data for every declared level."
    )
  }

  list(
    group_values = group_values,
    comparison_value = comparison_value,
    missing_outcome = missing_outcome,
    used = !missing_outcome,
    n_total = n_total,
    n_missing = n_missing,
    n_used = n_used,
    sample_flow = tibble::tibble(
      analysis_id = rep(context$analysis_id, level_n),
      outcome_var_id = rep(context$outcome_var_id, level_n),
      group_value = group_values,
      n_total = unname(n_total),
      n_missing = unname(n_missing),
      n_used = unname(n_used)
    )
  )
}
