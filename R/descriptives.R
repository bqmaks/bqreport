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
#' @param functions A list of explicit `descriptive_function` providers.
#' @param comparisons Whether to estimate an effect between two groups.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_descriptives <- function(
  .data,
  variables = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  overall = TRUE,
  confidence_level = 0.95,
  functions = list(),
  comparisons = FALSE
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (!is.logical(overall) || length(overall) != 1L || is.na(overall)) {
    stop_descriptive_plan("`overall` must be TRUE or FALSE.")
  }
  comparison_requested <- if (inherits(comparisons, "group_comparison_spec")) {
    TRUE
  } else if (is.logical(comparisons) && length(comparisons) == 1L &&
      !is.na(comparisons)) {
    comparisons
  } else {
    stop_descriptive_plan(
      "`comparisons` must be TRUE, FALSE, or a group_comparison_spec."
    )
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
  if (comparison_requested && length(group_selection) == 0L) {
    stop_descriptive_plan("Comparisons require one grouping variable.")
  }
  functions <- validate_descriptive_functions(functions)

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
      variable_spec, group_spec, overall, confidence_level, functions,
      comparisons
    )
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

descriptive_plan_row <- function(
  variable_spec,
  group_spec,
  overall,
  confidence_level,
  functions,
  comparisons
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
  row$variable_label <- variable_spec$label[[1]]
  row$variable_unit <- variable_spec$unit[[1]]
  row$variable_digits <- variable_spec$digits[[1]]
  row$group_id <- if (is.null(group_spec)) NA_character_ else group_spec$var_id[[1]]
  row$group <- if (is.null(group_spec)) NA_character_ else group_spec$name[[1]]
  row$overall <- overall
  row$descriptive_templates <- list(templates)
  row$requested_statistics <- list(descriptive_placeholders(templates))
  row$descriptive_functions <- list(functions)
  comparison_spec <- if (inherits(comparisons, "group_comparison_spec")) {
    comparisons
  } else if (isTRUE(comparisons)) {
    default_group_comparison_spec(variable_spec$type[[1]])
  } else NULL
  row$comparisons <- !is.null(comparison_spec)
  row$comparison_method <- if (is.null(comparison_spec)) NA_character_ else comparison_spec$id
  row$comparison_estimand <- if (is.null(comparison_spec)) NA_character_ else comparison_spec$effect_measure
  row$comparison_scale <- if (is.null(comparison_spec)) NA_character_ else comparison_spec$scale
  row$comparison_ci_method <- if (is.null(comparison_spec)) NA_character_ else comparison_spec$ci_method
  row$comparison_function_hash <- if (is.null(comparison_spec)) NA_character_ else comparison_spec$function_hash
  row$comparison_object <- list(comparison_spec)
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

validate_descriptive_functions <- function(functions) {
  if (!is.list(functions) ||
      any(!vapply(functions, inherits, logical(1), "descriptive_function"))) {
    stop_descriptive_function(
      "`functions` must be a list of descriptive_function objects."
    )
  }
  if (length(functions) == 0L) return(functions)
  ids <- vapply(functions, `[[`, character(1), "id")
  if (anyDuplicated(ids)) {
    stop_descriptive_function("Descriptive function ids must be unique.")
  }
  fields <- unlist(lapply(functions, `[[`, "fields"), use.names = FALSE)
  duplicates <- unique(fields[duplicated(fields)])
  if (length(duplicates)) stop_duplicate_descriptive_field(duplicates)
  functions
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
    plan$variable_label[[i]] <- registry$label[[variable_row]]
    plan$variable_unit[[i]] <- registry$unit[[variable_row]]
    plan$variable_digits[[i]] <- registry$digits[[variable_row]]
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
  requested <- plan$requested_statistics[[i]]
  providers <- plan$descriptive_functions[[i]]
  provided_fields <- unlist(lapply(providers, `[[`, "fields"), use.names = FALSE)
  unknown <- setdiff(requested, c(observed_fields, provided_fields))
  if (length(unknown)) {
    model_fields <- c(
      "estimate", "std.error", "conf.low", "conf.high", "statistic",
      "df", "p.value"
    )
    missing_model <- intersect(unknown, model_fields)
    other_unknown <- setdiff(unknown, model_fields)
    if (length(missing_model)) {
      issues <- c(issues, paste0(
        "The descriptive plan has no explicit model-based provider for: ",
        paste(missing_model, collapse = ", "), "."
      ))
    }
    if (length(other_unknown)) {
      issues <- c(issues, paste0(
        "Unknown descriptive statistics: ",
        paste(other_unknown, collapse = ", "), "."
      ))
    }
  }
  relevant_providers <- providers[vapply(providers, function(provider) {
    length(intersect(provider$fields, requested)) > 0L
  }, logical(1))]
  if (!is.na(variable_row)) {
    incompatible <- vapply(relevant_providers, function(provider) {
      !registry$type[[variable_row]] %in% provider$types
    }, logical(1))
    if (any(incompatible)) {
      issues <- c(issues, paste0(
        "Descriptive providers do not support variable type `",
        registry$type[[variable_row]], "`: ",
        paste(vapply(relevant_providers[incompatible], `[[`, character(1), "id"), collapse = ", "),
        "."
      ))
    }
  }
  required_packages <- unique(unlist(
    lapply(relevant_providers, `[[`, "required_packages"), use.names = FALSE
  ))
  plan$required_packages[i] <- list(required_packages)
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    issues <- c(issues, paste0(
      "Missing packages required by descriptive providers: ",
      paste(missing_packages, collapse = ", "), "."
    ))
  }

  if (isTRUE(plan$comparisons[[i]]) && !is.na(variable_row) &&
      !is.na(group_row)) {
    comparison_spec <- plan$comparison_object[[i]]
    group_values <- data[[registry$name[[group_row]]]]
    observed_groups <- unique(as.character(
      analysis_vector(group_values)[!special_missing_mask(group_values)]
    ))
    reference <- registry$reference[[group_row]]
    if (length(observed_groups) != 2L) {
      issues <- c(issues, "Group comparisons require exactly two observed groups.")
    }
    if (is.null(reference)) {
      issues <- c(issues, "The grouping variable has no configured reference value.")
    } else if (!as.character(reference) %in% observed_groups) {
      issues <- c(issues, "The configured group reference is not observed.")
    }
    if (is.null(comparison_spec)) {
      issues <- c(issues, paste0(
        "Group comparisons do not support variable type `",
        registry$type[[variable_row]], "`."
      ))
    } else if (!registry$type[[variable_row]] %in% comparison_spec$types) {
      issues <- c(issues, paste0(
        "Comparison `", comparison_spec$id, "` does not support variable type `",
        registry$type[[variable_row]], "`."
      ))
    }
    if (registry$type[[variable_row]] == "binary" &&
        is.null(registry$event_value[[variable_row]])) {
      issues <- c(issues,
        "Binary group comparison requires an explicit event value.")
    }
    if (!is.null(comparison_spec)) {
      comparison_packages <- comparison_spec$required_packages
      missing_comparison_packages <- comparison_packages[
        !vapply(comparison_packages, requireNamespace, logical(1), quietly = TRUE)
      ]
      if (length(missing_comparison_packages)) {
        issues <- c(issues, paste0(
          "Missing packages required by group comparison: ",
          paste(missing_comparison_packages, collapse = ", "), "."
        ))
      }
      plan$required_packages[i] <- list(unique(c(
        plan$required_packages[[i]], comparison_packages
      )))
      if (isTRUE(comparison_spec$requires_positive_cells) &&
          length(observed_groups) == 2L && !is.null(reference)) {
        variable_values <- data[[registry$name[[variable_row]]]]
        complete <- !special_missing_mask(variable_values) &
          !special_missing_mask(group_values)
        event <- registry$event_value[[variable_row]]
        table_values <- table(
          as.character(analysis_vector(group_values)[complete]),
          analysis_vector(variable_values)[complete] == event
        )
        if (length(table_values) < 4L || any(table_values == 0L)) {
          issues <- c(issues,
            "Ratio comparison has a zero cell; no continuity correction is applied.")
        }
      }
    }
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
    observed <- if (type %in% c("continuous", "count")) {
      continuous_descriptive_rows(values, spec, population)
    } else {
      categorical_descriptive_rows(values, spec, population)
    }
    custom <- compute_descriptive_functions(
      spec, values, population, type
    )
    vctrs::vec_rbind(observed, custom)
  })
  vctrs::vec_rbind(!!!rows)
}

compute_descriptive_functions <- function(spec, values, population, type) {
  requested <- spec$requested_statistics[[1]]
  providers <- spec$descriptive_functions[[1]]
  providers <- providers[vapply(providers, function(provider) {
    length(intersect(provider$fields, requested)) > 0L
  }, logical(1))]
  if (length(providers) == 0L) return(descriptives_prototype())
  missing <- special_missing_mask(values)
  prepared <- analysis_vector(values)[!missing]
  rows <- lapply(providers, function(provider) {
    context <- structure(list(
      analysis_id = spec$analysis_id[[1]],
      variable_id = spec$variable_id[[1]],
      variable = spec$variable[[1]],
      variable_type = type,
      values = prepared,
      n_total = length(values),
      n_analyzed = length(prepared),
      n_missing = sum(missing),
      group = spec$group[[1]],
      group_level = population$group_level,
      overall = population$overall,
      confidence_level = spec$confidence_level[[1]],
      spec = spec,
      population = population
    ), class = "descriptive_context")
    output <- provider$compute(context)
    normalize_descriptive_function_output(output, provider, context)
  })
  vctrs::vec_rbind(!!!rows)
}

compute_descriptive_comparison <- function(spec, data) {
  if (!isTRUE(spec$comparisons[[1]])) {
    return(list(contrasts = contrasts_prototype(), tests = tests_prototype()))
  }
  registry <- variables(data)
  variable_row <- match(spec$variable_id[[1]], registry$var_id)
  group_row <- match(spec$group_id[[1]], registry$var_id)
  values <- data[[registry$name[[variable_row]]]]
  groups <- data[[registry$name[[group_row]]]]
  complete <- !special_missing_mask(values) & !special_missing_mask(groups)
  values <- analysis_vector(values)[complete]
  groups <- as.character(analysis_vector(groups)[complete])
  reference <- as.character(registry$reference[[group_row]])
  numerator_group <- setdiff(unique(groups), reference)
  if (length(numerator_group) != 1L) {
    stop_descriptive_plan(
      "Comparison groups changed after complete-case filtering."
    )
  }
  numerator_group <- numerator_group[[1]]
  comparison_spec <- spec$comparison_object[[1]]
  if (comparison_spec$id == "welch_mean_difference") {
    return(welch_mean_difference_result(
      spec, values, groups, numerator_group, reference
    ))
  }
  if (comparison_spec$id == "hedges_g") {
    return(hedges_g_result(
      spec, values, groups, numerator_group, reference
    ))
  }
  if (comparison_spec$id == "wald_risk_difference") {
    return(wald_risk_difference_result(
      spec, values, groups, numerator_group, reference,
      registry$event_value[[variable_row]]
    ))
  }
  if (comparison_spec$id %in% c(
    "log_wald_risk_ratio", "log_wald_odds_ratio"
  )) {
    return(binary_ratio_result(
      spec, values, groups, numerator_group, reference,
      registry$event_value[[variable_row]], comparison_spec$id
    ))
  }
  if (is.function(comparison_spec$compute)) {
    return(custom_group_comparison_result(
      spec, values, groups, numerator_group, reference, comparison_spec
    ))
  }
  stop_descriptive_plan("Unknown descriptive comparison method.")
}

hedges_g_result <- function(spec, values, groups, numerator_group, reference) {
  x <- values[groups == numerator_group]
  y <- values[groups == reference]
  nx <- length(x)
  ny <- length(y)
  pooled_sd <- sqrt(
    ((nx - 1) * stats::var(x) + (ny - 1) * stats::var(y)) /
      (nx + ny - 2)
  )
  d <- (mean(x) - mean(y)) / pooled_sd
  correction <- 1 - 3 / (4 * (nx + ny) - 9)
  estimate <- correction * d
  variance_d <- (nx + ny) / (nx * ny) + d^2 / (2 * (nx + ny - 2))
  standard_error <- correction * sqrt(variance_d)
  critical <- stats::qnorm(1 - (1 - spec$confidence_level[[1]]) / 2)
  list(
    contrasts = descriptive_contrast_row(
      spec, numerator_group, reference, estimate,
      estimate - critical * standard_error,
      estimate + critical * standard_error,
      NA_real_, "standardized_mean_difference", "standard_deviation"
    ),
    tests = tests_prototype()
  )
}

welch_mean_difference_result <- function(
  spec, values, groups, numerator_group, reference
) {
  numerator_values <- values[groups == numerator_group]
  reference_values <- values[groups == reference]
  test <- stats::t.test(
    numerator_values, reference_values,
    conf.level = spec$confidence_level[[1]], var.equal = FALSE
  )
  list(
    contrasts = descriptive_contrast_row(
      spec, numerator_group, reference,
      mean(numerator_values) - mean(reference_values),
      test$conf.int[[1]], test$conf.int[[2]], test$p.value,
      "mean_difference", "identity"
    ),
    tests = descriptive_test_row(
      spec, "welch_t_test", unname(test$statistic),
      unname(test$parameter), test$p.value
    )
  )
}

wald_risk_difference_result <- function(
  spec, values, groups, numerator_group, reference, event
) {
  numerator_values <- values[groups == numerator_group]
  reference_values <- values[groups == reference]
  numerator_events <- sum(numerator_values == event)
  reference_events <- sum(reference_values == event)
  numerator_n <- length(numerator_values)
  reference_n <- length(reference_values)
  numerator_risk <- numerator_events / numerator_n
  reference_risk <- reference_events / reference_n
  estimate <- numerator_risk - reference_risk
  standard_error <- sqrt(
    numerator_risk * (1 - numerator_risk) / numerator_n +
      reference_risk * (1 - reference_risk) / reference_n
  )
  critical <- stats::qnorm(1 - (1 - spec$confidence_level[[1]]) / 2)
  group_test <- suppressWarnings(stats::prop.test(
    c(numerator_events, reference_events),
    c(numerator_n, reference_n), correct = FALSE
  ))
  list(
    contrasts = descriptive_contrast_row(
      spec, numerator_group, reference, estimate,
      estimate - critical * standard_error,
      estimate + critical * standard_error,
      group_test$p.value, "risk_difference", "probability_difference"
    ),
    tests = descriptive_test_row(
      spec, "pearson_chi_squared", unname(group_test$statistic),
      unname(group_test$parameter), group_test$p.value
    )
  )
}

binary_ratio_result <- function(
  spec, values, groups, numerator_group, reference, event, method
) {
  x <- values[groups == numerator_group]
  y <- values[groups == reference]
  a <- sum(x == event)
  b <- length(x) - a
  c <- sum(y == event)
  d <- length(y) - c
  critical <- stats::qnorm(1 - (1 - spec$confidence_level[[1]]) / 2)
  if (method == "log_wald_risk_ratio") {
    estimate <- (a / (a + b)) / (c / (c + d))
    standard_error <- sqrt(1 / a - 1 / (a + b) + 1 / c - 1 / (c + d))
    measure <- "risk_ratio"
  } else {
    estimate <- (a / b) / (c / d)
    standard_error <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
    measure <- "odds_ratio"
  }
  group_test <- suppressWarnings(stats::prop.test(
    c(a, c), c(a + b, c + d), correct = FALSE
  ))
  list(
    contrasts = descriptive_contrast_row(
      spec, numerator_group, reference, estimate,
      exp(log(estimate) - critical * standard_error),
      exp(log(estimate) + critical * standard_error),
      group_test$p.value, measure, "ratio"
    ),
    tests = descriptive_test_row(
      spec, "pearson_chi_squared", unname(group_test$statistic),
      unname(group_test$parameter), group_test$p.value
    )
  )
}

custom_group_comparison_result <- function(
  spec, values, groups, numerator_group, reference, comparison_spec
) {
  context <- structure(list(
    analysis_id = spec$analysis_id[[1]],
    variable_id = spec$variable_id[[1]], variable = spec$variable[[1]],
    variable_type = spec$variable_type[[1]], group = spec$group[[1]],
    numerator = numerator_group, denominator = reference,
    numerator_values = values[groups == numerator_group],
    denominator_values = values[groups == reference],
    confidence_level = spec$confidence_level[[1]]
  ), class = "group_comparison_context")
  output <- comparison_spec$compute(context)
  if (!inherits(output, "group_comparison_output")) {
    stop_group_comparison_output(
      "Custom group comparison must return group_comparison_output()."
    )
  }
  contrast <- descriptive_contrast_row(
    spec, numerator_group, reference, output$estimate, output$conf_low,
    output$conf_high, output$p_value, comparison_spec$effect_measure,
    comparison_spec$scale
  )
  test <- if (is.na(output$test)) tests_prototype() else {
    descriptive_test_row(
      spec, output$test, output$statistic, output$df, output$p_value
    )
  }
  list(contrasts = contrast, tests = test)
}

descriptive_contrast_row <- function(
  spec, numerator, denominator, estimate, conf_low, conf_high, p_value,
  effect_measure, scale
) {
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$variable[[1]],
    predictor = spec$group[[1]], contrast_id = NA_character_,
    contrast = paste0(numerator, " vs ", denominator),
    numerator = numerator, denominator = denominator,
    modifier = NA_character_, modifier_level = NA_character_,
    estimate = as.numeric(estimate), conf_low = as.numeric(conf_low),
    conf_high = as.numeric(conf_high), p_value = as.numeric(p_value),
    p_adjusted = as.numeric(p_value), adjust_method = "none",
    effect_measure = effect_measure, scale = scale
  )
}

descriptive_test_row <- function(spec, test, statistic, df, p_value) {
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$variable[[1]],
    predictor = spec$group[[1]], test = test,
    statistic = as.numeric(statistic), df = as.numeric(df),
    p_value = as.numeric(p_value), method = spec$comparison_method[[1]]
  )
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
  denominator,
  statistic_method = descriptive_statistic_methods(statistic),
  source = "observed",
  method = "observed_descriptives",
  status = "observed",
  message = NA_character_
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
    statistic_method = rep(statistic_method, length.out = n),
    source = rep(source, length.out = n),
    method = rep(method, length.out = n),
    status = rep(status, length.out = n),
    message = rep(message, length.out = n)
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
    statistic_method = character(), source = character(), method = character(),
    status = character(), message = character()
  )
}

stop_descriptive_plan <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_descriptive_plan", "error", "condition")
  ))
}
