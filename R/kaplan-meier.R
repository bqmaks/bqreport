#' Compile a Kaplan--Meier analysis plan
#'
#' @param .data A `bq_data` object.
#' @param outcomes Composite survival outcomes selected with tidyselect.
#' @param groups Optional single grouping column.
#' @param times Optional positive evaluation times. If omitted, the full
#'   Kaplan--Meier step curve is returned.
#' @param confidence_level Confidence level.
#' @param quantiles Optional event-time distribution probabilities strictly
#'   between zero and one.
#' @param rmst_tau Optional positive restriction time for restricted mean
#'   survival time. RMST is not computed unless this estimand is explicit.
#' @param estimates One or both of `survival` and `cumulative_risk`.
#'   Cumulative risk is the single-event complement `1 - S(t)`; it is neither
#'   cumulative hazard nor a competing-risks cumulative incidence function.
#' @param comparisons Optional pairwise group-comparison specification. It is
#'   applied to pairwise log-rank tests and, when `rmst_tau` is supplied, to
#'   differences in restricted mean survival time.
#' @param adjust Multiplicity adjustment accepted by [stats::p.adjust()].
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_kaplan_meier <- function(
  .data,
  outcomes = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  times = NULL,
  confidence_level = 0.95,
  quantiles = NULL,
  rmst_tau = NULL,
  estimates = "survival",
  comparisons = NULL,
  adjust = "none"
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (!is.null(comparisons) && (!inherits(comparisons, "contrast_spec") ||
      !comparisons$type %in% c("against_reference", "all_pairwise", "consecutive"))) {
    stop_invalid_survival_plan("`comparisons` must specify supported target group pairs.")
  }
  if (!is.character(adjust) || length(adjust) != 1L ||
      !adjust %in% stats::p.adjust.methods) {
    stop_invalid_survival_plan("`adjust` is not supported.")
  }
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
  if (!is.null(quantiles)) {
    valid_quantiles <- is.numeric(quantiles) && length(quantiles) > 0L &&
      !anyNA(quantiles) && all(is.finite(quantiles)) &&
      all(quantiles > 0 & quantiles < 1) && !anyDuplicated(quantiles)
    if (!valid_quantiles) {
      stop_invalid_survival_plan(
        "`quantiles` must contain unique probabilities strictly between zero and one."
      )
    }
    quantiles <- sort(as.numeric(quantiles))
  }
  if (!is.null(rmst_tau)) {
    valid_tau <- is.numeric(rmst_tau) && length(rmst_tau) == 1L &&
      !is.na(rmst_tau) && is.finite(rmst_tau) && rmst_tau > 0
    if (!valid_tau) {
      stop_invalid_survival_plan("`rmst_tau` must be one positive finite value.")
    }
    rmst_tau <- as.numeric(rmst_tau)
  }
  valid_estimates <- is.character(estimates) && length(estimates) > 0L &&
    !anyNA(estimates) && !anyDuplicated(estimates) &&
    all(estimates %in% c("survival", "cumulative_risk"))
  if (!valid_estimates) {
    stop_invalid_survival_plan(
      "`estimates` must contain survival, cumulative_risk, or both."
    )
  }
  group_selection <- tidyselect::eval_select(rlang::enquo(groups), .data)
  if (length(group_selection) > 1L) {
    stop_invalid_survival_plan("Select at most one grouping variable.")
  }
  if (!is.null(comparisons) && length(group_selection) == 0L) {
    stop_invalid_survival_plan("Pairwise log-rank comparisons require a grouping variable.")
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
    row$quantile_probabilities <- list(quantiles)
    row$rmst_tau <- if (is.null(rmst_tau)) NA_real_ else rmst_tau
    row$survival_estimands <- list(estimates)
    row$pairwise_comparison_spec <- list(comparisons)
    row$pairwise_adjust_method <- adjust
    row$predictor_id <- NA_character_
    row$predictor <- NA_character_
    row$formula <- list(NULL)
    row <- refine_analysis_id(
      row, row$survival_outcome_id[[1]], row$group_id[[1]], times, quantiles,
      row$rmst_tau[[1]], estimates, adjust
    )
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
        comparison_spec <- plan$pairwise_comparison_spec[[i]]
        if (!is.null(comparison_spec) && comparison_spec$type == "against_reference") {
          observed <- as.character(analysis_vector(group)[complete])
          if (!as.character(comparison_spec$reference) %in% observed) {
            issues <- c(issues, "Pairwise log-rank reference is not observed.")
          }
        }
      }
    }
    time_values <- analysis_vector(time)[complete]
    if (!is.numeric(time_values)) {
      issues <- c(issues, "Survival time must be numeric.")
    } else if (any(time_values <= 0)) {
      issues <- c(issues, "Survival time must contain only positive values.")
    }
    if (!is.na(plan$rmst_tau[[i]]) && is.numeric(time_values) && length(time_values)) {
      maximum_supported <- if (is.na(plan$group_id[[i]])) {
        max(time_values)
      } else {
        group_values <- analysis_vector(data[[plan$group[[i]]]])[complete]
        min(vapply(split(time_values, group_values), max, numeric(1)))
      }
      if (plan$rmst_tau[[i]] > maximum_supported) {
        issues <- c(issues,
          "`rmst_tau` exceeds observed follow-up support in at least one population.")
      }
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
  survival_curve <- km_summary_rows(curve_summary, spec, estimate_type, grouped)
  curve <- list()
  if ("survival" %in% spec$survival_estimands[[1]]) {
    curve[[length(curve) + 1L]] <- survival_curve
  }
  if ("cumulative_risk" %in% spec$survival_estimands[[1]]) {
    curve[[length(curve) + 1L]] <- km_cumulative_risk_rows(survival_curve)
  }
  curve <- vctrs::vec_rbind(!!!curve)
  medians <- km_median_rows(fit, spec, grouped)
  quantile_rows <- km_quantile_rows(fit, spec, grouped)
  rmst_rows <- km_rmst_rows(fit, spec, grouped)
  estimates <- vctrs::vec_rbind(curve, medians, quantile_rows, rmst_rows)
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
  pairwise_spec <- spec$pairwise_comparison_spec[[1]]
  if (grouped && !is.null(pairwise_spec)) {
    levels <- levels(frame$..bq_group)
    reference <- if (pairwise_spec$type == "against_reference") {
      pairwise_spec$reference
    } else NULL
    pairs <- contrast_level_pairs(levels, pairwise_spec$type, reference)
    pairwise_tests <- lapply(seq_len(nrow(pairs)), function(i) {
      keep <- frame$..bq_group %in% c(pairs$numerator[[i]], pairs$denominator[[i]])
      pair_frame <- frame[keep, , drop = FALSE]
      pair_frame$..bq_group <- droplevels(pair_frame$..bq_group)
      difference <- survival::survdiff(
        formula, data = pair_frame, rho = 0, na.action = stats::na.omit
      )
      tibble::tibble(
        analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
        predictor = spec$group[[1]],
        contrast = paste0(pairs$numerator[[i]], " - ", pairs$denominator[[i]]),
        numerator = pairs$numerator[[i]], denominator = pairs$denominator[[i]],
        test = "pairwise_log_rank", statistic = unname(difference$chisq),
        df = 1, p_value = stats::pchisq(difference$chisq, 1, lower.tail = FALSE),
        p_adjusted = NA_real_, adjust_method = spec$pairwise_adjust_method[[1]],
        method = "log_rank"
      )
    })
    pairwise_tests <- vctrs::vec_rbind(!!!pairwise_tests)
    pairwise_tests$p_adjusted <- stats::p.adjust(
      pairwise_tests$p_value, method = spec$pairwise_adjust_method[[1]]
    )
    tests <- vctrs::vec_rbind(tests, pairwise_tests)
  }
  contrasts <- km_rmst_contrasts(rmst_rows, spec, grouped)
  list(model = fit, estimates = estimates, tests = tests, contrasts = contrasts)
}

km_cumulative_risk_rows <- function(survival_rows) {
  out <- survival_rows
  lower <- 1 - survival_rows$conf_high
  upper <- 1 - survival_rows$conf_low
  out$estimate <- 1 - survival_rows$estimate
  out$conf_low <- lower
  out$conf_high <- upper
  out$estimate_type <- ifelse(
    survival_rows$estimate_type == "survival_curve",
    "cumulative_risk_curve",
    "cumulative_risk"
  )
  out$method <- "kaplan_meier_complement"
  out
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
    quantile_probability = NA_real_, restriction_time = NA_real_,
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
    quantile_probability = NA_real_, restriction_time = NA_real_,
    estimate_type = "median_survival", scale = "time",
    time_unit = spec$time_unit[[1]], method = "kaplan_meier"
  )
}

km_quantile_rows <- function(fit, spec, grouped) {
  probabilities <- spec$quantile_probabilities[[1]]
  if (is.null(probabilities)) return(survival_estimates_prototype())
  result <- stats::quantile(fit, probs = probabilities)
  quantiles <- as.matrix(result$quantile)
  lower <- as.matrix(result$lower)
  upper <- as.matrix(result$upper)
  if (!grouped) {
    quantiles <- matrix(quantiles, nrow = 1L)
    lower <- matrix(lower, nrow = 1L)
    upper <- matrix(upper, nrow = 1L)
  }
  group_level <- if (grouped) {
    sub("^[^=]+=", "", rownames(quantiles))
  } else NA_character_
  n_groups <- nrow(quantiles)
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    group = spec$group[[1]],
    group_level = rep(group_level, each = length(probabilities)),
    time = NA_real_, n_risk = NA_integer_, n_event = NA_integer_,
    n_censor = NA_integer_, estimate = as.numeric(t(quantiles)),
    std_error = NA_real_, conf_low = as.numeric(t(lower)),
    conf_high = as.numeric(t(upper)),
    quantile_probability = rep(probabilities, times = n_groups),
    restriction_time = NA_real_, estimate_type = "survival_quantile",
    scale = "time", time_unit = spec$time_unit[[1]], method = "kaplan_meier"
  )
}

km_rmst_rows <- function(fit, spec, grouped) {
  tau <- spec$rmst_tau[[1]]
  if (is.na(tau)) return(survival_estimates_prototype())
  table <- summary(fit, rmean = tau)$table
  if (is.null(dim(table))) {
    table <- matrix(table, nrow = 1L, dimnames = list(NA_character_, names(table)))
  }
  group_level <- if (grouped) sub("^[^=]+=", "", rownames(table)) else NA_character_
  estimate <- as.numeric(table[, "rmean"])
  standard_error <- as.numeric(table[, "se(rmean)"])
  critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    group = spec$group[[1]], group_level = group_level,
    time = NA_real_, n_risk = NA_integer_, n_event = NA_integer_,
    n_censor = NA_integer_, estimate = estimate, std_error = standard_error,
    conf_low = estimate - critical * standard_error,
    conf_high = estimate + critical * standard_error,
    quantile_probability = NA_real_, restriction_time = tau,
    estimate_type = "restricted_mean_survival_time", scale = "time",
    time_unit = spec$time_unit[[1]], method = "kaplan_meier"
  )
}

km_rmst_contrasts <- function(rmst_rows, spec, grouped) {
  comparison_spec <- spec$pairwise_comparison_spec[[1]]
  if (!grouped || is.null(comparison_spec) || nrow(rmst_rows) == 0L) {
    return(contrasts_prototype())
  }
  reference <- if (comparison_spec$type == "against_reference") {
    comparison_spec$reference
  } else NULL
  pairs <- contrast_level_pairs(
    rmst_rows$group_level, comparison_spec$type, reference
  )
  critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    numerator <- rmst_rows[rmst_rows$group_level == pairs$numerator[[i]], ]
    denominator <- rmst_rows[rmst_rows$group_level == pairs$denominator[[i]], ]
    estimate <- numerator$estimate[[1]] - denominator$estimate[[1]]
    standard_error <- sqrt(
      numerator$std_error[[1]]^2 + denominator$std_error[[1]]^2
    )
    statistic <- estimate / standard_error
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$group[[1]],
      contrast_id = bq_id(
        "contrast", spec$analysis_id[[1]], "rmst_difference",
        pairs$numerator[[i]], pairs$denominator[[i]]
      ),
      contrast = paste0(pairs$numerator[[i]], " - ", pairs$denominator[[i]]),
      numerator = pairs$numerator[[i]], denominator = pairs$denominator[[i]],
      modifier = NA_character_, modifier_level = NA_character_,
      inner_contrast = NA_character_, outer_contrast = NA_character_,
      estimand = "rmst_difference", exponentiated = FALSE,
      estimate = estimate, std_error = standard_error,
      std_error_scale = "time",
      conf_low = estimate - critical * standard_error,
      conf_high = estimate + critical * standard_error,
      p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      p_adjusted = NA_real_, adjust_method = spec$pairwise_adjust_method[[1]],
      effect_measure = "rmst_difference", scale = "time"
    )
  })
  out <- vctrs::vec_rbind(!!!rows)
  out$p_adjusted <- stats::p.adjust(
    out$p_value, method = spec$pairwise_adjust_method[[1]]
  )
  out
}

stop_invalid_survival_plan <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_survival_plan", "error", "condition")
  ))
}
