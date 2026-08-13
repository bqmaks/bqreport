#' Compile a descriptive analysis plan
#'
#' `plan_descriptives()` creates one inspectable task per selected variable.
#' Results may be computed for the complete population, levels of one grouping
#' variable, or both. The grouping label used by a report is deliberately kept
#' out of the numerical result.
#'
#' @param .data A `bq_data` object.
#' @param variables Variables selected with tidyselect.
#' @param groups Optional single grouping variable selected with tidyselect.
#' @param overall Whether to include statistics for the complete population.
#' @param confidence_level Confidence level reserved for model-based providers.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_descriptives <- function(
  .data,
  variables = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  overall = TRUE,
  confidence_level = 0.95
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (!is.logical(overall) || length(overall) != 1L || is.na(overall)) {
    stop_descriptive_plan("`overall` must be TRUE or FALSE.")
  }
  variable_selection <- tidyselect::eval_select(rlang::enquo(variables), .data)
  group_selection <- tidyselect::eval_select(rlang::enquo(groups), .data)
  if (length(group_selection) > 1L) {
    stop_descriptive_plan("Select at most one grouping variable.")
  }
  if (!overall && length(group_selection) == 0L) {
    stop_descriptive_plan(
      "A descriptive plan without groups must include the overall population."
    )
  }

  registry <- tibble::as_tibble(
    attr(.data, "variable_registry", exact = TRUE)
  )
  variable_names <- names(variable_selection)
  if (length(variable_names) == 0L) {
    return(empty_analysis_plan())
  }
  group_spec <- if (length(group_selection)) {
    registry[match(names(group_selection), registry$name), , drop = FALSE]
  } else {
    NULL
  }
  rows <- lapply(variable_names, function(variable_name) {
    variable_spec <- registry[
      match(variable_name, registry$name), , drop = FALSE
    ]
    descriptive_plan_row(
      variable_spec, group_spec, overall, confidence_level
    )
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

descriptive_plan_row <- function(
  variable_spec,
  group_spec,
  overall,
  confidence_level
) {
  row <- analysis_plan_row(
    outcome_spec = variable_spec,
    predictor_spec = variable_spec,
    method = NULL,
    status = if (variable_spec$status[[1]] == "valid") "ready" else "review",
    reason = if (variable_spec$status[[1]] == "valid") {
      NA_character_
    } else {
      "Variable metadata require review."
    },
    confidence_level = confidence_level
  )
  templates <- variable_spec$descriptive_templates[[1]]
  if (is.null(templates)) {
    templates <- default_descriptive_templates(variable_spec$type[[1]])
  }
  row$analysis_type <- "descriptive"
  row$variable_id <- variable_spec$var_id[[1]]
  row$variable <- variable_spec$name[[1]]
  row$variable_type <- variable_spec$type[[1]]
  row$group_id <- if (is.null(group_spec)) NA_character_ else group_spec$var_id[[1]]
  row$group <- if (is.null(group_spec)) NA_character_ else group_spec$name[[1]]
  row$overall <- overall
  row$descriptive_templates <- list(templates)
  row$requested_statistics <- list(descriptive_placeholders(templates))
  row$outcome_id <- variable_spec$var_id[[1]]
  row$predictor_id <- NA_character_
  row$outcome <- variable_spec$name[[1]]
  row$predictor <- NA_character_
  row$method_policy <- "descriptive_default"
  row$method <- "observed_descriptives"
  row$engine <- "descriptive"
  row$estimator <- "empirical"
  row$ci_method <- NA_character_
  row$formula <- list(NULL)
  row$family <- NA_character_
  row$link <- NA_character_
  row$effect_measure <- "descriptive_statistic"
  row$model_scale <- "observed"
  row$scale <- "observed"
  row$selection_reason <- "Observed descriptive statistics."
  row$required_packages <- list(character())
  row$method_object <- list(NULL)
  row$validated <- FALSE
  row
}

default_descriptive_templates <- function(type) {
  if (type %in% c("continuous", "count")) {
    return("{mean} ({sd})")
  }
  if (type %in% c("binary", "ordinal", "nominal")) {
    return("{n}/{N} ({p}%)")
  }
  NULL
}

descriptive_placeholders <- function(templates) {
  if (is.null(templates)) return(character())
  matches <- gregexpr(
    "\\{[A-Za-z][A-Za-z0-9_]*(?:\\.[A-Za-z][A-Za-z0-9_]*)*\\}",
    templates,
    perl = TRUE
  )
  unique(unlist(Map(function(template, positions) {
    if (identical(positions[[1]], -1L)) return(character())
    values <- regmatches(template, list(positions))[[1]]
    substring(values, 2L, nchar(values) - 1L)
  }, templates, matches), use.names = FALSE))
}

validate_descriptive_plan_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  issues <- character()
  variable_row <- match(plan$variable_id[[i]], registry$var_id)
  if (is.na(variable_row)) {
    issues <- c(issues, "The variable referenced by stable id is absent from the data.")
  } else {
    plan$variable[[i]] <- registry$name[[variable_row]]
    plan$outcome[[i]] <- registry$name[[variable_row]]
    plan$variable_type[[i]] <- registry$type[[variable_row]]
  }
  group_row <- if (is.na(plan$group_id[[i]])) NA_integer_ else {
    match(plan$group_id[[i]], registry$var_id)
  }
  if (!is.na(plan$group_id[[i]]) && is.na(group_row)) {
    issues <- c(issues, "The grouping variable referenced by stable id is absent from the data.")
  } else if (!is.na(group_row)) {
    plan$group[[i]] <- registry$name[[group_row]]
  }

  supported_types <- c("continuous", "count", "binary", "ordinal", "nominal")
  if (!is.na(variable_row) && !registry$type[[variable_row]] %in% supported_types) {
    issues <- c(issues, paste0(
      "Descriptive statistics do not support variable type `",
      registry$type[[variable_row]], "`."
    ))
  }
  observed_fields <- c(
    "n", "n_missing", "mean", "sd", "median", "q1", "q3", "iqr",
    "min", "max", "mad", "skewness", "kurtosis", "N", "p"
  )
  model_fields <- c(
    "estimate", "std.error", "conf.low", "conf.high", "statistic",
    "df", "p.value"
  )
  requested <- plan$requested_statistics[[i]]
  unknown <- setdiff(requested, c(observed_fields, model_fields))
  if (length(unknown)) {
    issues <- c(issues, paste0(
      "Unknown descriptive statistics: ", paste(unknown, collapse = ", "), "."
    ))
  }
  requested_model_fields <- intersect(requested, model_fields)
  if (length(requested_model_fields)) {
    issues <- c(issues, paste0(
      "The current descriptive plan has no model-based provider for: ",
      paste(requested_model_fields, collapse = ", "), "."
    ))
  }

  if (!is.na(variable_row)) {
    values <- data[[registry$name[[variable_row]]]]
    missing <- special_missing_mask(values)
    plan$n_total[[i]] <- length(values)
    plan$n_eligible[[i]] <- length(values)
    plan$n_analyzed[[i]] <- sum(!missing)
    plan$n_missing_outcome[[i]] <- sum(missing)
    plan$n_missing_predictor[[i]] <- 0L
    if (length(values) == 0L) {
      issues <- c(issues, "No observations are available for descriptive analysis.")
    }
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

compute_observed_descriptives <- function(spec, data) {
  registry <- variables(data)
  variable_row <- match(spec$variable_id[[1]], registry$var_id)
  variable_name <- registry$name[[variable_row]]
  type <- registry$type[[variable_row]]
  populations <- list()
  if (isTRUE(spec$overall[[1]])) {
    populations[[length(populations) + 1L]] <- list(
      mask = rep(TRUE, nrow(data)), overall = TRUE, group_level = NA_character_
    )
  }
  if (!is.na(spec$group_id[[1]])) {
    group_row <- match(spec$group_id[[1]], registry$var_id)
    group_name <- registry$name[[group_row]]
    group_values <- data[[group_name]]
    nonmissing_group <- !special_missing_mask(group_values)
    levels <- unique(as.character(group_values[nonmissing_group]))
    populations <- c(populations, lapply(levels, function(level) {
      list(
        mask = nonmissing_group & as.character(group_values) == level,
        overall = FALSE,
        group_level = level
      )
    }))
  }
  rows <- lapply(populations, function(population) {
    values <- data[[variable_name]][population$mask]
    if (type %in% c("continuous", "count")) {
      continuous_descriptive_rows(values, spec, population)
    } else {
      categorical_descriptive_rows(values, spec, population)
    }
  })
  vctrs::vec_rbind(!!!rows)
}

continuous_descriptive_rows <- function(values, spec, population) {
  original <- values
  values <- analysis_vector(original)
  missing <- special_missing_mask(original)
  values[missing] <- NA
  observed <- values[!missing]
  n <- length(observed)
  quantiles <- if (n) {
    stats::quantile(observed, c(0.25, 0.75), names = FALSE, type = 7)
  } else {
    c(NA_real_, NA_real_)
  }
  statistics <- c(
    n = n,
    n_missing = sum(missing),
    mean = if (n) mean(observed) else NA_real_,
    sd = if (n > 1L) stats::sd(observed) else NA_real_,
    median = if (n) stats::median(observed) else NA_real_,
    q1 = quantiles[[1]], q3 = quantiles[[2]],
    iqr = if (n) stats::IQR(observed, type = 7) else NA_real_,
    min = if (n) min(observed) else NA_real_,
    max = if (n) max(observed) else NA_real_,
    mad = if (n) stats::mad(observed, constant = 1.4826) else NA_real_,
    skewness = adjusted_sample_skewness(observed),
    kurtosis = adjusted_excess_kurtosis(observed)
  )
  descriptive_rows(
    spec, population, level = NA_character_, statistic = names(statistics),
    value = unname(statistics),
    numerator = c(
      as.integer(n), as.integer(sum(missing)),
      rep(NA_integer_, length(statistics) - 2L)
    ),
    denominator = rep(as.integer(length(values)), length(statistics))
  )
}

adjusted_sample_skewness <- function(x) {
  n <- length(x)
  if (n < 3L) return(NA_real_)
  standard_deviation <- stats::sd(x)
  if (!is.finite(standard_deviation) || standard_deviation == 0) {
    return(NA_real_)
  }
  standardized <- (x - mean(x)) / standard_deviation
  n / ((n - 1) * (n - 2)) * sum(standardized^3)
}

adjusted_excess_kurtosis <- function(x) {
  n <- length(x)
  if (n < 4L) return(NA_real_)
  standard_deviation <- stats::sd(x)
  if (!is.finite(standard_deviation) || standard_deviation == 0) {
    return(NA_real_)
  }
  standardized <- (x - mean(x)) / standard_deviation
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) *
    sum(standardized^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

categorical_descriptive_rows <- function(values, spec, population) {
  original <- values
  values <- analysis_vector(original)
  missing <- special_missing_mask(original)
  values[missing] <- NA
  observed <- values[!missing]
  levels <- if (is.factor(values)) {
    levels(values)
  } else {
    unique(as.character(observed))
  }
  denominator <- length(observed)
  rows <- lapply(levels, function(level) {
    numerator <- sum(as.character(observed) == level)
    descriptive_rows(
      spec, population, level = level, statistic = c("n", "N", "p"),
      value = c(
        numerator,
        denominator,
        if (denominator) numerator / denominator else NA_real_
      ),
      numerator = rep(as.integer(numerator), 3L),
      denominator = rep(as.integer(denominator), 3L)
    )
  })
  missing_row <- descriptive_rows(
    spec, population, level = NA_character_, statistic = "n_missing",
    value = sum(missing), numerator = as.integer(sum(missing)),
    denominator = as.integer(length(values))
  )
  vctrs::vec_rbind(!!!rows, missing_row)
}

descriptive_rows <- function(
  spec,
  population,
  level,
  statistic,
  value,
  numerator,
  denominator
) {
  n <- length(statistic)
  tibble::tibble(
    analysis_id = rep(spec$analysis_id[[1]], n),
    variable_id = rep(spec$variable_id[[1]], n),
    variable = rep(spec$variable[[1]], n),
    variable_type = rep(spec$variable_type[[1]], n),
    group_id = rep(spec$group_id[[1]], n),
    group = rep(spec$group[[1]], n),
    group_level = rep(population$group_level, n),
    overall = rep(population$overall, n),
    level = rep(level, length.out = n),
    statistic = statistic,
    value = as.numeric(value),
    numerator = as.integer(numerator),
    denominator = as.integer(denominator),
    statistic_method = descriptive_statistic_methods(statistic),
    source = rep("observed", n),
    method = rep("observed_descriptives", n)
  )
}

descriptive_statistic_methods <- function(statistic) {
  methods <- rep("empirical", length(statistic))
  methods[statistic == "q1" | statistic == "q3"] <- "quantile_type_7"
  methods[statistic == "iqr"] <- "iqr_quantile_type_7"
  methods[statistic == "sd"] <- "sample_standard_deviation"
  methods[statistic == "mad"] <- "median_absolute_deviation_constant_1.4826"
  methods[statistic == "skewness"] <- "adjusted_fisher_pearson"
  methods[statistic == "kurtosis"] <- "bias_corrected_excess"
  methods
}

descriptives_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), variable_id = character(),
    variable = character(), variable_type = character(),
    group_id = character(), group = character(), group_level = character(),
    overall = logical(), level = character(), statistic = character(),
    value = double(), numerator = integer(), denominator = integer(),
    statistic_method = character(), source = character(), method = character()
  )
}

stop_descriptive_plan <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_descriptive_plan", "error", "condition")
  ))
}
