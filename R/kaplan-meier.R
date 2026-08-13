#' Compile a Kaplan--Meier analysis plan
#'
#' @param .data A `bq_data` object.
#' @param outcomes Composite survival outcomes selected with tidyselect.
#' @param groups Optional single grouping column.
#' @param times Optional positive evaluation times. If omitted, the full
#'   Kaplan--Meier step curve is returned.
#' @param confidence_level Confidence level.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_kaplan_meier <- function(
  .data,
  outcomes = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  times = NULL,
  confidence_level = 0.95
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (!is.null(times)) {
    valid_times <- is.numeric(times) && length(times) > 0L && !anyNA(times) &&
      all(is.finite(times)) && all(times > 0) && !anyDuplicated(times)
    if (!valid_times) {
      stop_invalid_survival_plan(
        "`times` must contain unique positive finite values."
      )
    }
    times <- sort(as.numeric(times))
  }
  group_selection <- tidyselect::eval_select(rlang::enquo(groups), .data)
  if (length(group_selection) > 1L) {
    stop_invalid_survival_plan("Select at most one grouping variable.")
  }
  outcome_registry <- resolve_outcomes(.data)
  survival_registry <- outcome_registry[outcome_registry$type == "survival", , drop = FALSE]
  selection_data <- stats::setNames(
    as.data.frame(rep(list(logical()), nrow(survival_registry))),
    survival_registry$name
  )
  outcome_selection <- tidyselect::eval_select(
    rlang::enquo(outcomes), selection_data
  )
  if (length(outcome_selection) == 0L) return(empty_analysis_plan())
  variable_registry <- variables(.data)
  group_spec <- if (length(group_selection)) {
    variable_registry[match(names(group_selection), variable_registry$name), , drop = FALSE]
  } else NULL
  rows <- lapply(names(outcome_selection), function(outcome_name) {
    outcome <- survival_registry[
      match(outcome_name, survival_registry$name), , drop = FALSE
    ]
    fallback <- variable_registry[
      match(outcome$time_var_id[[1]], variable_registry$var_id), , drop = FALSE
    ]
    predictor_spec <- if (is.null(group_spec)) fallback else group_spec
    pseudo_outcome <- tibble::tibble(
      var_id = outcome$outcome_id, name = outcome$name
    )
    method <- new_method_spec(
      "kaplan_meier", "survfit", "product_limit", "greenwood",
      NA_character_, NA_character_, "survival_probability",
      scale = "probability", model_scale = "probability",
      required_packages = "survival"
    )
    row <- analysis_plan_row(
      pseudo_outcome, predictor_spec, method,
      status = if (outcome$status[[1]] == "valid") "ready" else "review",
      reason = if (outcome$status[[1]] == "valid") NA_character_ else
        "Survival outcome metadata require review.",
      confidence_level = confidence_level
    )
    row$analysis_type <- "kaplan_meier"
    row$survival_outcome_id <- outcome$outcome_id
    row$time_id <- outcome$time_var_id
    row$event_id <- outcome$event_var_id
    row$event_value <- outcome$event_value
    row$time_unit <- outcome$time_unit
    row$group_id <- if (is.null(group_spec)) NA_character_ else group_spec$var_id[[1]]
    row$group <- if (is.null(group_spec)) NA_character_ else group_spec$name[[1]]
    row$evaluation_times <- list(times)
    row$predictor_id <- NA_character_
    row$predictor <- NA_character_
    row$formula <- list(NULL)
    row
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_kaplan_meier_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  issues <- character()
  outcome_registry <- resolve_outcomes(data)
  outcome_row <- match(plan$survival_outcome_id[[i]], outcome_registry$outcome_id)
  if (is.na(outcome_row) || outcome_registry$status[[outcome_row]] != "valid") {
    issues <- c(issues, "The survival outcome or one of its components is absent.")
  } else {
    outcome <- outcome_registry[outcome_row, , drop = FALSE]
    plan$outcome[[i]] <- outcome$name[[1]]
    time <- data[[outcome$time[[1]]]]
    event <- data[[outcome$event[[1]]]]
    complete <- !special_missing_mask(time) & !special_missing_mask(event)
    if (!is.na(plan$group_id[[i]])) {
      group_row <- match(plan$group_id[[i]], registry$var_id)
      if (is.na(group_row)) {
        issues <- c(issues, "The grouping variable is absent from the data.")
      } else {
        plan$group[[i]] <- registry$name[[group_row]]
        group <- data[[registry$name[[group_row]]]]
        complete <- complete & !special_missing_mask(group)
        if (n_distinct_values(analysis_vector(group)[complete]) < 2L) {
          issues <- c(issues, "Grouping variable has no variation in analyzed data.")
        }
      }
    }
    time_values <- analysis_vector(time)[complete]
    if (!is.numeric(time_values)) {
      issues <- c(issues, "Survival time must be numeric.")
    } else if (any(time_values <= 0)) {
      issues <- c(issues, "Survival time must contain only positive values.")
    }
    plan$n_total[[i]] <- nrow(data)
    plan$n_eligible[[i]] <- nrow(data)
    plan$n_analyzed[[i]] <- sum(complete)
    plan$n_missing_outcome[[i]] <- sum(
      special_missing_mask(time) | special_missing_mask(event)
    )
    plan$n_missing_predictor[[i]] <- if (is.na(plan$group_id[[i]])) 0L else
      sum(special_missing_mask(data[[plan$group[[i]]]]))
    if (!any(complete)) issues <- c(issues, "No complete observations are available.")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    issues <- c(issues, "Missing required package: survival.")
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

execute_kaplan_meier <- function(spec, data) {
  registry <- variables(data)
  outcome <- resolve_outcomes(data)
  outcome <- outcome[outcome$outcome_id == spec$survival_outcome_id[[1]], , drop = FALSE]
  time_original <- data[[outcome$time[[1]]]]
  event_original <- data[[outcome$event[[1]]]]
  frame <- tibble::tibble(
    ..bq_time = analysis_vector(time_original),
    ..bq_event = analysis_vector(event_original) == outcome$event_value[[1]]
  )
  frame$..bq_time[special_missing_mask(time_original)] <- NA
  frame$..bq_event[special_missing_mask(event_original)] <- NA
  grouped <- !is.na(spec$group_id[[1]])
  if (grouped) {
    group_name <- registry$name[match(spec$group_id[[1]], registry$var_id)]
    group_original <- data[[group_name]]
    frame$..bq_group <- factor(analysis_vector(group_original))
    frame$..bq_group[special_missing_mask(group_original)] <- NA
  }
  formula <- if (grouped) {
    survival::Surv(..bq_time, ..bq_event) ~ ..bq_group
  } else {
    survival::Surv(..bq_time, ..bq_event) ~ 1
  }
  fit <- survival::survfit(
    formula, data = frame, conf.int = spec$confidence_level[[1]],
    na.action = stats::na.omit
  )
  requested_times <- spec$evaluation_times[[1]]
  curve_summary <- if (is.null(requested_times)) summary(fit, censored = TRUE) else
    summary(fit, times = requested_times, extend = FALSE)
  estimate_type <- if (is.null(requested_times)) {
    "survival_curve"
  } else "survival_probability"
  curve <- km_summary_rows(curve_summary, spec, estimate_type, grouped)
  medians <- km_median_rows(fit, spec, grouped)
  estimates <- vctrs::vec_rbind(curve, medians)
  tests <- if (grouped) {
    difference <- survival::survdiff(formula, data = frame, rho = 0,
      na.action = stats::na.omit)
    degrees <- length(difference$n) - 1L
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$group[[1]], test = "log_rank",
      statistic = unname(difference$chisq), df = as.numeric(degrees),
      p_value = stats::pchisq(difference$chisq, degrees, lower.tail = FALSE),
      method = "log_rank"
    )
  } else tests_prototype()
  list(model = fit, estimates = estimates, tests = tests)
}

km_summary_rows <- function(summary_fit, spec, estimate_type, grouped) {
  strata <- if (grouped) as.character(summary_fit$strata) else
    rep(NA_character_, length(summary_fit$time))
  group_level <- if (grouped) sub("^[^=]+=", "", strata) else strata
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    group = spec$group[[1]], group_level = group_level,
    time = as.numeric(summary_fit$time), n_risk = as.integer(summary_fit$n.risk),
    n_event = as.integer(summary_fit$n.event),
    n_censor = as.integer(summary_fit$n.censor),
    estimate = as.numeric(summary_fit$surv),
    std_error = as.numeric(summary_fit$std.err),
    conf_low = as.numeric(summary_fit$lower),
    conf_high = as.numeric(summary_fit$upper), estimate_type = estimate_type,
    scale = "probability", time_unit = spec$time_unit[[1]],
    method = "kaplan_meier"
  )
}

km_median_rows <- function(fit, spec, grouped) {
  table <- summary(fit)$table
  if (is.null(dim(table))) {
    table <- matrix(table, nrow = 1L, dimnames = list(NA_character_, names(table)))
  }
  group_level <- if (grouped) sub("^[^=]+=", "", rownames(table)) else NA_character_
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    group = spec$group[[1]], group_level = group_level,
    time = NA_real_, n_risk = NA_integer_, n_event = NA_integer_,
    n_censor = NA_integer_, estimate = as.numeric(table[, "median"]),
    std_error = NA_real_, conf_low = as.numeric(table[, "0.95LCL"]),
    conf_high = as.numeric(table[, "0.95UCL"]),
    estimate_type = "median_survival", scale = "time",
    time_unit = spec$time_unit[[1]], method = "kaplan_meier"
  )
}

stop_invalid_survival_plan <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_survival_plan", "error", "condition")
  ))
}
