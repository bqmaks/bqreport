#' Compile an Aalen--Johansen cumulative-incidence plan
#'
#' @param .data A `bq_data` object.
#' @param outcomes Composite competing-risk outcomes selected with tidyselect.
#' @param groups Optional single grouping column.
#' @param times Optional positive evaluation times; omitted for the full curve.
#' @param confidence_level Confidence level.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_cumulative_incidence <- function(
  .data, outcomes = tidyselect::everything(),
  groups = tidyselect::any_of(character()), times = NULL,
  confidence_level = 0.95
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (!is.null(times)) {
    valid <- is.numeric(times) && length(times) && !anyNA(times) &&
      all(is.finite(times)) && all(times > 0) && !anyDuplicated(times)
    if (!valid) stop_invalid_survival_plan("`times` must contain unique positive finite values.")
    times <- sort(as.numeric(times))
  }
  group_selection <- tidyselect::eval_select(rlang::enquo(groups), .data)
  if (length(group_selection) > 1L) stop_invalid_survival_plan("Select at most one grouping variable.")
  outcome_registry <- resolve_outcomes(.data)
  registry <- outcome_registry[outcome_registry$type == "competing_risk", , drop = FALSE]
  selection_data <- stats::setNames(as.data.frame(rep(list(logical()), nrow(registry))), registry$name)
  selected <- tidyselect::eval_select(rlang::enquo(outcomes), selection_data)
  if (!length(selected)) return(empty_analysis_plan())
  variables_registry <- variables(.data)
  group_spec <- if (length(group_selection)) {
    variables_registry[match(names(group_selection), variables_registry$name), , drop = FALSE]
  } else NULL
  rows <- lapply(names(selected), function(name) {
    outcome <- registry[match(name, registry$name), , drop = FALSE]
    fallback <- variables_registry[match(outcome$time_var_id[[1]], variables_registry$var_id), , drop = FALSE]
    method <- new_method_spec(
      "aalen_johansen", "survfit", "aalen_johansen", "greenwood",
      NA_character_, NA_character_, "cumulative_incidence",
      scale = "probability", model_scale = "probability", required_packages = "survival"
    )
    row <- analysis_plan_row(
      tibble::tibble(var_id = outcome$outcome_id, name = outcome$name),
      if (is.null(group_spec)) fallback else group_spec, method,
      status = if (outcome$status[[1]] == "valid") "ready" else "review",
      reason = outcome$reason[[1]], confidence_level = confidence_level
    )
    row$analysis_type <- "cumulative_incidence"
    row$survival_outcome_id <- outcome$outcome_id
    row$time_id <- outcome$time_var_id
    row$event_id <- outcome$event_var_id
    row$censor_value <- outcome$censor_value
    row$time_unit <- outcome$time_unit
    row$group_id <- if (is.null(group_spec)) NA_character_ else group_spec$var_id[[1]]
    row$group <- if (is.null(group_spec)) NA_character_ else group_spec$name[[1]]
    row$evaluation_times <- list(times)
    row$predictor_id <- NA_character_; row$predictor <- NA_character_; row$formula <- list(NULL)
    row
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_cumulative_incidence_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  outcomes <- resolve_outcomes(data)
  row <- match(plan$survival_outcome_id[[i]], outcomes$outcome_id)
  issues <- character()
  if (is.na(row) || outcomes$status[[row]] != "valid") {
    issues <- "The competing-risk outcome or one of its components is absent."
  } else {
    outcome <- outcomes[row, , drop = FALSE]
    plan$outcome[[i]] <- outcome$name[[1]]
    time <- analysis_vector(data[[outcome$time[[1]]]])
    event <- analysis_vector(data[[outcome$event[[1]]]])
    complete <- !special_missing_mask(data[[outcome$time[[1]]]]) &
      !special_missing_mask(data[[outcome$event[[1]]]])
    if (any(!is.finite(time[complete]) | time[complete] < 0)) issues <- c(issues, "Follow-up time must be finite and non-negative.")
    if (!any(event[complete] != outcome$censor_value[[1]])) issues <- c(issues, "At least one event cause must be observed.")
    if (!is.na(plan$group_id[[i]])) {
      group_row <- match(plan$group_id[[i]], registry$var_id)
      if (is.na(group_row)) issues <- c(issues, "The grouping variable is absent from the data.") else {
        plan$group[[i]] <- registry$name[[group_row]]
        group <- analysis_vector(data[[registry$name[[group_row]]]])
        complete <- complete & !special_missing_mask(data[[registry$name[[group_row]]]])
        if (n_distinct_values(group[complete]) < 2L) issues <- c(issues, "Grouping variable has no variation in analyzed data.")
      }
    }
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

execute_cumulative_incidence <- function(spec, data) {
  outcomes <- resolve_outcomes(data)
  outcome <- outcomes[outcomes$outcome_id == spec$survival_outcome_id[[1]], , drop = FALSE]
  time_original <- data[[outcome$time[[1]]]]
  event_original <- data[[outcome$event[[1]]]]
  event <- analysis_vector(event_original)
  observed <- unique(event[!special_missing_mask(event_original)])
  causes <- setdiff(observed, outcome$censor_value[[1]])
  status <- factor(event, levels = c(outcome$censor_value[[1]], causes))
  frame <- tibble::tibble(..bq_time = analysis_vector(time_original), ..bq_status = status)
  frame$..bq_time[special_missing_mask(time_original)] <- NA
  frame$..bq_status[special_missing_mask(event_original)] <- NA
  grouped <- !is.na(spec$group_id[[1]])
  if (grouped) {
    variable_registry <- variables(data)
    group_name <- variable_registry$name[match(spec$group_id[[1]], variable_registry$var_id)]
    original <- data[[group_name]]
    frame$..bq_group <- factor(analysis_vector(original))
    frame$..bq_group[special_missing_mask(original)] <- NA
  }
  formula <- if (grouped) survival::Surv(..bq_time, ..bq_status) ~ ..bq_group else survival::Surv(..bq_time, ..bq_status) ~ 1
  fit <- survival::survfit(formula, data = frame, conf.int = spec$confidence_level[[1]], na.action = stats::na.omit)
  requested <- spec$evaluation_times[[1]]
  summary_fit <- if (is.null(requested)) summary(fit, censored = TRUE) else summary(fit, times = requested, extend = FALSE)
  cause_columns <- seq_along(summary_fit$states)[-1L]
  n_time <- length(summary_fit$time)
  strata <- if (grouped) sub("^[^=]+=", "", as.character(summary_fit$strata)) else rep(NA_character_, n_time)
  rows <- lapply(cause_columns, function(column) tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    group = spec$group[[1]], group_level = strata,
    cause = summary_fit$states[[column]], time = as.numeric(summary_fit$time),
    n_risk = as.integer(summary_fit$n.risk[, 1]),
    n_event = as.integer(summary_fit$n.event[, column]),
    n_censor = as.integer(summary_fit$n.censor[, 1]),
    estimate = as.numeric(summary_fit$pstate[, column]),
    std_error = as.numeric(summary_fit$std.err[, column]),
    conf_low = as.numeric(summary_fit$lower[, column]), conf_high = as.numeric(summary_fit$upper[, column]),
    quantile_probability = NA_real_, restriction_time = NA_real_,
    estimate_type = "cumulative_incidence", scale = "probability",
    time_unit = spec$time_unit[[1]], method = "aalen_johansen"
  ))
  list(model = fit, estimates = vctrs::vec_rbind(!!!rows))
}
