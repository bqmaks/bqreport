#' Validate common post hoc inputs and count observations
#'
#' Post hoc providers are independent analytic entities, so they cannot rely
#' on an omnibus provider to validate their shared data and context contract.
#'
#' @param data Engine data.
#' @param context Compiled analysis context.
#' @param analysis_name Public provider name used in diagnostics.
#'
#' @return A list with declared group values, the missing-outcome mask, the
#'   used-row mask and a sample-flow tibble.
#' @noRd
prepare_post_hoc_input <- function(data, context, analysis_name) {
  if (
    !tibble::is_tibble(data) ||
      !identical(names(data), c(".row_id", ".outcome", ".group"))
  ) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      paste0(
        "`data` for `", analysis_name, "()` must be a tibble with columns ",
        "`.row_id`, `.outcome` and `.group`, in that order."
      )
    )
  }
  if (
    anyNA(data$.row_id) || anyDuplicated(data$.row_id) ||
      !is.atomic(data$.row_id) || !is.null(dim(data$.row_id))
  ) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`.row_id` must contain unique, non-missing atomic values."
    )
  }
  if (
    !is.numeric(data$.outcome) || is.object(data$.outcome) ||
      !is.null(dim(data$.outcome)) ||
      any(!is.finite(data$.outcome[!is.na(data$.outcome)]))
  ) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`.outcome` must be one plain numeric vector with finite observed values."
    )
  }
  if (!is.factor(data$.group) || anyNA(data$.group)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`.group` must be a factor without missing values."
    )
  }

  required_context <- c(
    "analysis_id", "test_id", "estimate_id", "outcome_var_id",
    "group_var_id", "strata_var_id", "group_levels"
  )
  if (!is.list(context) || !identical(names(context), required_context)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      paste0(
        "`context` for `", analysis_name, "()` must contain `analysis_id`, ",
        "`test_id`, `estimate_id`, `outcome_var_id`, `group_var_id`, ",
        "`strata_var_id` and `group_levels`, in that order."
      )
    )
  }
  ids <- context[c(
    "analysis_id", "test_id", "outcome_var_id", "group_var_id"
  )]
  valid_ids <- vapply(ids, function(value) {
    is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(value)
  }, logical(1))
  if (
    !all(valid_ids) || !is.character(context$estimate_id) ||
      length(context$estimate_id) != 1L || !is.na(context$estimate_id) ||
      !is.character(context$strata_var_id) ||
      length(context$strata_var_id) != 1L || !is.na(context$strata_var_id)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      paste0(
        "Post hoc IDs must be non-empty character scalars; `estimate_id` ",
        "and `strata_var_id` must be NA."
      )
    )
  }
  group_values <- levels(data$.group)
  if (
    length(group_values) < 2L ||
      !tibble::is_tibble(context$group_levels) ||
      !identical(
        names(context$group_levels), c("var_id", "value", "position")
      ) ||
      !is.character(context$group_levels$var_id) ||
      !is.character(context$group_levels$value) ||
      !is.integer(context$group_levels$position) ||
      anyNA(context$group_levels) ||
      !identical(
        context$group_levels$var_id,
        rep(context$group_var_id, length(group_values))
      ) ||
      !identical(context$group_levels$value, group_values) ||
      !identical(
        context$group_levels$position,
        seq_along(group_values)
      )
  ) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      paste0(
        "`group_levels` must describe at least two `.group` levels once, ",
        "in factor order, for `group_var_id`."
      )
    )
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
    group_value <- group_values[which(n_used == 0L)[1L]]
    bq_abort(
      "bq_error_invalid_analysis_input",
      paste0(
        "Group level `", group_value, "` has no observed outcome values; ",
        "provide data for every declared level."
      )
    )
  }

  list(
    group_values = group_values,
    missing_outcome = missing_outcome,
    used = !missing_outcome,
    sample_flow = tibble::tibble(
      analysis_id = rep(context$analysis_id, length(group_values)),
      outcome_var_id = rep(context$outcome_var_id, length(group_values)),
      group_value = group_values,
      n_total = unname(n_total),
      n_missing = unname(n_missing),
      n_used = unname(n_used)
    )
  )
}
