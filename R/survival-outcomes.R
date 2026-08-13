#' Register a composite survival outcome
#'
#' A survival outcome references separate follow-up time and event columns by
#' stable variable identifiers. The outcome name is an analytical identifier;
#' it does not create or mutate a data column.
#'
#' @param .data A `bq_data` object.
#' @param name Bare name for the composite outcome.
#' @param time Exactly one follow-up time column selected with tidyselect.
#' @param event Exactly one event indicator column selected with tidyselect.
#' @param event_value Scalar value identifying an event.
#' @param time_unit Non-empty unit string.
#'
#' @return Updated `bq_data`.
#' @export
add_survival_outcome <- function(
  .data, name, time, event, event_value, time_unit
) {
  check_bq_data(.data)
  outcome_name <- rlang::as_name(rlang::enquo(name))
  if (!nzchar(outcome_name)) {
    stop_invalid_survival_outcome("Survival outcome name must not be empty.")
  }
  time_selection <- tidyselect::eval_select(rlang::enquo(time), .data)
  event_selection <- tidyselect::eval_select(rlang::enquo(event), .data)
  if (length(time_selection) != 1L || length(event_selection) != 1L) {
    stop_invalid_survival_outcome(
      "`time` and `event` must each select exactly one column."
    )
  }
  time_name <- names(time_selection)
  event_name <- names(event_selection)
  if (identical(time_name, event_name)) {
    stop_invalid_survival_outcome(
      "Survival time and event must reference different columns."
    )
  }
  if (length(event_value) != 1L || is.na(event_value)) {
    stop_invalid_survival_outcome("`event_value` must be one non-missing value.")
  }
  if (!is.character(time_unit) || length(time_unit) != 1L ||
      is.na(time_unit) || !nzchar(time_unit)) {
    stop_invalid_survival_outcome("`time_unit` must be one non-empty string.")
  }
  registry <- variables(.data)
  outcomes_registry <- attr(.data, "outcome_registry", exact = TRUE)
  if (nrow(outcomes_registry) && outcome_name %in% outcomes_registry$name) {
    stop_invalid_survival_outcome(paste0(
      "Outcome `", outcome_name, "` is already registered."
    ))
  }
  row <- tibble::tibble(
    outcome_id = paste0("outcome_", uuid::UUIDgenerate()),
    name = outcome_name,
    type = "survival",
    time_var_id = registry$var_id[match(time_name, registry$name)],
    event_var_id = registry$var_id[match(event_name, registry$name)],
    event_value = list(event_value),
    time_unit = time_unit,
    status = "valid",
    reason = NA_character_
  )
  attr(.data, "outcome_registry") <- if (nrow(outcomes_registry)) {
    vctrs::vec_rbind(outcomes_registry, row)
  } else row
  .data <- add_role_by_name(.data, time_name, "outcome")
  .data <- add_role_by_name(.data, event_name, "event")
  .data
}

#' Register a composite competing-risk outcome
#'
#' @inheritParams add_survival_outcome
#' @param censor_value Scalar value identifying censoring. Every other observed
#'   event value is treated as an explicit competing event cause.
#'
#' @return Updated `bq_data`.
#' @export
add_competing_risk_outcome <- function(
  .data, name, time, event, censor_value, time_unit
) {
  check_bq_data(.data)
  outcome_name <- rlang::as_name(rlang::enquo(name))
  time_selection <- tidyselect::eval_select(rlang::enquo(time), .data)
  event_selection <- tidyselect::eval_select(rlang::enquo(event), .data)
  if (!nzchar(outcome_name) || length(time_selection) != 1L ||
      length(event_selection) != 1L || identical(names(time_selection), names(event_selection))) {
    stop_invalid_survival_outcome(
      "Competing-risk outcome requires a name and distinct single time and event columns."
    )
  }
  if (length(censor_value) != 1L || is.na(censor_value)) {
    stop_invalid_survival_outcome("`censor_value` must be one non-missing value.")
  }
  if (!is.character(time_unit) || length(time_unit) != 1L ||
      is.na(time_unit) || !nzchar(time_unit)) {
    stop_invalid_survival_outcome("`time_unit` must be one non-empty string.")
  }
  registry <- variables(.data)
  outcome_registry <- attr(.data, "outcome_registry", exact = TRUE)
  if (nrow(outcome_registry) && outcome_name %in% outcome_registry$name) {
    stop_invalid_survival_outcome(paste0("Outcome `", outcome_name, "` is already registered."))
  }
  row <- tibble::tibble(
    outcome_id = paste0("outcome_", uuid::UUIDgenerate()), name = outcome_name,
    type = "competing_risk",
    time_var_id = registry$var_id[match(names(time_selection), registry$name)],
    event_var_id = registry$var_id[match(names(event_selection), registry$name)],
    censor_value = list(censor_value), time_unit = time_unit,
    status = "valid", reason = NA_character_
  )
  attr(.data, "outcome_registry") <- vctrs::vec_rbind(outcome_registry, row)
  .data <- add_role_by_name(.data, names(time_selection), "outcome")
  add_role_by_name(.data, names(event_selection), "event")
}

#' Access the composite outcome registry
#'
#' Component names are resolved from stable variable identifiers when accessed.
#' Missing components invalidate the returned registry without mutating the
#' source object.
#'
#' @param x A `bq_data` object.
#'
#' @return A tidy outcome registry.
#' @export
outcomes <- function(x) {
  resolve_outcomes(x)
}

resolve_outcomes <- function(x) {
  check_bq_data(x)
  registry <- tibble::as_tibble(
    attr(x, "outcome_registry", exact = TRUE)
  )
  if (nrow(registry) == 0L) return(registry)
  variables_registry <- variables(x)
  if ("time_var_id" %in% names(registry)) {
    survival_rows <- registry$type %in% c("survival", "competing_risk")
    time_rows <- match(registry$time_var_id, variables_registry$var_id)
    event_rows <- match(registry$event_var_id, variables_registry$var_id)
    registry$time <- variables_registry$name[time_rows]
    registry$event <- variables_registry$name[event_rows]
    missing_time <- survival_rows & is.na(time_rows)
    missing_event <- survival_rows & is.na(event_rows)
    invalid <- missing_time | missing_event
    registry$status[invalid] <- "invalid"
    registry$reason[missing_time & !missing_event] <-
      "The survival time component is absent from the data."
    registry$reason[!missing_time & missing_event] <-
      "The survival event component is absent from the data."
    registry$reason[missing_time & missing_event] <-
      "The survival time and event components are absent from the data."
  }
  if ("value_var_ids" %in% names(registry)) {
    longitudinal_rows <- which(registry$type == "longitudinal")
    registry$values <- rep(list(character()), nrow(registry))
    for (row in longitudinal_rows) {
      ids <- registry$value_var_ids[[row]]
      source_rows <- match(ids, variables_registry$var_id)
      registry$values[[row]] <- variables_registry$name[source_rows]
      if (anyNA(source_rows)) {
        registry$status[[row]] <- "invalid"
        registry$reason[[row]] <-
          "One or more repeated source columns are absent from the data."
      }
    }
  }
  registry
}

stop_invalid_survival_outcome <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_outcome", "error", "condition")
  ))
}
