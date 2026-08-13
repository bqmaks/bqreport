#' Construct built-in descriptive group comparisons
#'
#' These specifications declare the estimand and scale before execution. They
#' support exactly two groups in the initial descriptive comparison slice.
#'
#' @param ci_method Confidence interval implementation identifier.
#'
#' @return A `group_comparison_spec`.
#' @export
mean_difference <- function(ci_method = "welch_t") {
  new_group_comparison_spec(
    "welch_mean_difference", c("continuous", "count"),
    "mean_difference", "identity", ci_method
  )
}

#' @rdname mean_difference
#' @export
standardized_mean_difference <- function(ci_method = "large_sample") {
  new_group_comparison_spec(
    "hedges_g", c("continuous", "count"),
    "standardized_mean_difference", "standard_deviation", ci_method
  )
}

#' @rdname mean_difference
#' @export
risk_difference <- function(ci_method = "wald") {
  new_group_comparison_spec(
    "wald_risk_difference", "binary",
    "risk_difference", "probability_difference", ci_method
  )
}

#' @rdname mean_difference
#' @export
risk_ratio <- function(ci_method = "log_wald") {
  new_group_comparison_spec(
    "log_wald_risk_ratio", "binary", "risk_ratio", "ratio", ci_method,
    requires_positive_cells = TRUE
  )
}

#' @rdname mean_difference
#' @export
odds_ratio <- function(ci_method = "log_wald") {
  new_group_comparison_spec(
    "log_wald_odds_ratio", "binary", "odds_ratio", "ratio", ci_method,
    requires_positive_cells = TRUE
  )
}

new_group_comparison_spec <- function(
  id, types, effect_measure, scale, ci_method, compute = NULL,
  required_packages = character(), function_hash = NA_character_,
  requires_positive_cells = FALSE
) {
  structure(list(
    id = id, types = types, effect_measure = effect_measure, scale = scale,
    ci_method = ci_method, compute = compute,
    required_packages = required_packages, function_hash = function_hash,
    requires_positive_cells = requires_positive_cells
  ), class = "group_comparison_spec")
}

#' Construct a custom descriptive group comparison
#'
#' @param id Stable comparison identifier.
#' @param types Supported analytical variable types.
#' @param effect_measure Declared effect measure.
#' @param scale Declared result scale.
#' @param compute Function accepting a read-only `group_comparison_context` and
#'   returning a value created by `group_comparison_output()`.
#' @param ci_method Confidence interval implementation identifier.
#' @param required_packages Required packages checked during preflight.
#'
#' @return A `group_comparison_spec`.
#' @export
group_comparison_function <- function(
  id, types, effect_measure, scale, compute,
  ci_method = "custom", required_packages = character()
) {
  values <- list(id = id, effect_measure = effect_measure, scale = scale,
    ci_method = ci_method)
  if (any(!vapply(values, function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, logical(1)))) {
    stop_group_comparison_contract(
      "`id`, `effect_measure`, `scale`, and `ci_method` must be non-empty strings."
    )
  }
  valid_types <- setdiff(valid_variable_types, "unknown")
  if (!is.character(types) || length(types) == 0L || anyNA(types) ||
      any(!types %in% valid_types) || anyDuplicated(types)) {
    stop_group_comparison_contract("`types` contains invalid variable types.")
  }
  if (!is.function(compute)) {
    stop_group_comparison_contract("`compute` must be a function.")
  }
  if (!is.character(required_packages) || anyNA(required_packages)) {
    stop_group_comparison_contract(
      "`required_packages` must be a character vector."
    )
  }
  new_group_comparison_spec(
    id, types, effect_measure, scale, ci_method, compute,
    unique(required_packages), digest::digest(compute)
  )
}

#' Construct custom group comparison output
#'
#' @param estimate,conf_low,conf_high,p_value Numeric scalar results.
#' @param std_error Optional standard error on `std_error_scale`.
#' @param std_error_scale Scale on which the standard error is defined.
#' @param statistic_method Non-empty method identifier.
#' @param statistic,df Optional test statistic and degrees of freedom.
#' @param test Optional test name; omit it when no separate test is returned.
#'
#' @return A `group_comparison_output`.
#' @export
group_comparison_output <- function(
  estimate, conf_low, conf_high, p_value, statistic_method,
  statistic = NA_real_, df = NA_real_, test = NA_character_,
  std_error = NA_real_, std_error_scale = NA_character_
) {
  numeric_values <- c(estimate, conf_low, conf_high, p_value, statistic, df)
  if (!is.numeric(numeric_values) || length(numeric_values) != 6L) {
    stop_group_comparison_output("Comparison output values must be numeric scalars.")
  }
  if (!is.character(statistic_method) || length(statistic_method) != 1L ||
      is.na(statistic_method) || !nzchar(statistic_method)) {
    stop_group_comparison_output("`statistic_method` must be one non-empty string.")
  }
  if (!is.character(test) || length(test) != 1L) {
    stop_group_comparison_output("`test` must be one character value.")
  }
  structure(list(
    estimate = as.numeric(estimate), conf_low = as.numeric(conf_low),
    conf_high = as.numeric(conf_high), p_value = as.numeric(p_value),
    statistic_method = statistic_method, statistic = as.numeric(statistic),
    df = as.numeric(df), test = test, std_error = as.numeric(std_error),
    std_error_scale = std_error_scale
  ), class = "group_comparison_output")
}

default_group_comparison_spec <- function(type) {
  if (type %in% c("continuous", "count")) return(mean_difference())
  if (type == "binary") return(risk_difference())
  NULL
}

stop_group_comparison_contract <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_group_comparison", "error", "condition")
  ))
}

stop_group_comparison_output <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c(
      "bq_error_invalid_group_comparison_output", "error", "condition"
    )
  ))
}
