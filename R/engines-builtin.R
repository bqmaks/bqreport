# Builtin model engines: analysis frames, fitting, tidy extraction.

build_analysis_frame <- function(spec, data) {
  data <- data[stratum_mask(data, spec$strata[[1]], spec$stratum_values[[1]]), , drop = FALSE]
  outcome_name <- spec$outcome[[1]]
  predictor_name <- spec$predictor[[1]]
  registry <- variables(data)
  outcome_spec <- registry[match(spec$outcome_id[[1]], registry$var_id), , drop = FALSE]
  predictor_spec <- registry[match(spec$predictor_id[[1]], registry$var_id), , drop = FALSE]
  outcome_original <- data[[outcome_name]]
  predictor_original <- data[[predictor_name]]
  outcome <- analysis_vector(outcome_original)
  predictor <- analysis_vector(predictor_original)
  outcome[special_missing_mask(outcome_original)] <- NA
  predictor[special_missing_mask(predictor_original)] <- NA
  predictor_transformation <- spec$transformation_specs[[1]][[spec$predictor_id[[1]]]]
  predictor <- apply_transformation_spec(
    predictor, predictor_transformation, predictor_name, spec$analysis_id[[1]]
  )

  if (outcome_spec$type[[1]] == "binary") {
    event <- outcome_spec$event_value[[1]]
    outcome <- ifelse(is.na(outcome), NA_integer_, as.integer(outcome == event))
  }
  if (outcome_spec$type[[1]] == "nominal") {
    outcome <- factor(outcome)
    reference <- outcome_spec$reference[[1]]
    if (!is.null(reference)) {
      outcome <- stats::relevel(outcome, ref = as.character(reference))
    }
  }
  if (predictor_spec$type[[1]] %in% c("binary", "nominal")) {
    predictor <- factor(predictor)
    predictor <- stats::relevel(
      predictor,
      ref = as.character(predictor_spec$reference[[1]])
    )
  }

  frame <- tibble::tibble(outcome, predictor)
  names(frame) <- c(outcome_name, predictor_name)
  for (name in spec$covariates[[1]]) {
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    covariate_id <- variables(data)$var_id[match(name, variables(data)$name)]
    value <- apply_transformation_spec(
      value, spec$transformation_specs[[1]][[covariate_id]],
      name, spec$analysis_id[[1]]
    )
    model_term <- spec$model_term_specs[[1]][[covariate_id]]
    if (is.null(model_term)) {
      frame[[name]] <- value
    } else {
      basis <- apply_model_term_spec(value, model_term)
      for (column in seq_along(model_term$output_names)) {
        frame[[model_term$output_names[[column]]]] <- basis[, column]
      }
    }
  }
  for (name in spec$effect_modifiers[[1]]) {
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    modifier_spec <- variables(data)[match(name, variables(data)$name), , drop = FALSE]
    if (modifier_spec$type[[1]] %in% c("binary", "nominal")) {
      value <- factor(value)
      reference <- modifier_spec$reference[[1]]
      if (!is.null(reference)) value <- stats::relevel(value, ref = as.character(reference))
    }
    frame[[name]] <- value
  }
  if (!is.na(spec$weight[[1]])) frame[["..bq_weight"]] <- data[[spec$weight[[1]]]]
  if (!is.na(spec$cluster[[1]])) frame[["..bq_cluster"]] <- data[[spec$cluster[[1]]]]
  frame
}

analysis_vector <- function(x) {
  if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else x
}

fit_builtin_engine <- function(spec, frame) {
  fit_formula <- spec$formula[[1]]
  environment(fit_formula) <- environment()
  fit_weights <- if ("..bq_weight" %in% names(frame)) frame[["..bq_weight"]] else NULL
  if (spec$engine[[1]] == "lm") {
    if (!is.null(fit_weights)) {
      return(stats::lm(fit_formula, data = frame, weights = fit_weights, na.action = stats::na.omit))
    }
    return(stats::lm(fit_formula, data = frame, na.action = stats::na.omit))
  }
  if (spec$engine[[1]] == "glm") {
    family <- builtin_glm_family(spec)
    if (!is.null(fit_weights)) {
      return(stats::glm(
        fit_formula, data = frame, family = family,
        weights = fit_weights, na.action = stats::na.omit
      ))
    }
    return(stats::glm(
      fit_formula,
      data = frame,
      family = family,
      na.action = stats::na.omit
    ))
  }
  stop(paste0("Unknown built-in engine `", spec$engine[[1]], "`."))
}

builtin_glm_family <- function(spec) {
  family <- spec$family[[1]]
  link <- spec$link[[1]]
  if (identical(family, "binomial")) return(stats::binomial(link))
  if (identical(family, "poisson")) return(stats::poisson(link))
  stop(paste0("Unsupported GLM family `", family, "`."))
}

tidy_builtin_estimates <- function(fit, spec) {
  coefficients <- summary(fit)$coefficients
  coefficient_metadata <- normalize_coefficient_metadata(fit, spec)
  beta <- unname(coefficients[, "Estimate"])
  standard_error <- unname(coefficients[, "Std. Error"])
  if (spec$variance[[1]] == "robust") {
    robust_covariance <- sandwich::vcovHC(fit, type = "HC0")
    standard_error <- unname(sqrt(diag(robust_covariance)))
  } else if (spec$variance[[1]] == "cluster_robust") {
    model_rows <- as.integer(rownames(stats::model.frame(fit)))
    frame_clusters <- attr(fit, "bq_clusters")
    robust_covariance <- sandwich::vcovCL(
      fit, cluster = frame_clusters[model_rows], type = "HC1", cadjust = TRUE
    )
    standard_error <- unname(sqrt(diag(robust_covariance)))
  }
  is_glm <- inherits(fit, "glm")
  statistic <- beta / standard_error
  if (!is_glm) {
    df <- rep.int(stats::df.residual(fit), length(beta))
    critical <- stats::qt((1 + spec$confidence_level[[1]]) / 2, df = df)
    p_value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
    estimate <- beta
    conf_low <- beta - critical * standard_error
    conf_high <- beta + critical * standard_error
    scale <- "identity"
    std_error_scale <- "identity"
    n_events <- NA_integer_
  } else {
    df <- rep(NA_real_, length(beta))
    critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
    p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    estimate <- beta
    conf_low <- beta - critical * standard_error
    conf_high <- beta + critical * standard_error
    scale <- "link"
    std_error_scale <- if (identical(spec$family[[1]], "poisson")) "log_rate" else "log_odds"
    n_events <- if (identical(spec$family[[1]], "binomial")) {
      as.integer(sum(stats::model.response(stats::model.frame(fit)) == 1))
    } else NA_integer_
  }
  if (isTRUE(spec$exponentiate[[1]])) {
    estimate <- exp(estimate)
    conf_low <- exp(conf_low)
    conf_high <- exp(conf_high)
    scale <- "ratio"
  }
  transformation_metadata <- coefficient_transformation_metadata(
    coefficient_metadata$term, spec
  )

  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]],
    stratum_label = spec$stratum_label[[1]],
    transformation_id = transformation_metadata$id,
    transformation_label = transformation_metadata$label,
    term = coefficient_metadata$term,
    level = coefficient_metadata$level,
    estimate = estimate,
    std_error = standard_error,
    std_error_scale = std_error_scale,
    conf_low = conf_low,
    conf_high = conf_high,
    statistic = statistic,
    df = as.numeric(df),
    p_value = p_value,
    effect_measure = spec$effect_measure[[1]],
    scale = scale,
    n = as.integer(stats::nobs(fit)),
    n_events = n_events,
    method = spec$method[[1]]
    , variance = spec$variance[[1]]
  )
}

coefficient_transformation_metadata <- function(terms, spec) {
  ids <- rep(NA_character_, length(terms))
  labels <- rep(NA_character_, length(terms))
  variable_ids <- c(spec$predictor_id[[1]], spec$covariate_ids[[1]])
  variable_names <- c(spec$predictor[[1]], spec$covariates[[1]])
  for (i in seq_along(variable_ids)) {
    transformation <- spec$transformation_specs[[1]][[variable_ids[[i]]]]
    rows <- terms == variable_names[[i]]
    if (!is.null(transformation)) {
      ids[rows] <- transformation$id
      labels[rows] <- transformation$label
    }
  }
  list(id = ids, label = labels)
}

normalize_coefficient_metadata <- function(fit, spec) {
  coefficient_names <- rownames(summary(fit)$coefficients)
  terms <- coefficient_names
  levels <- rep(NA_character_, length(coefficient_names))
  intercept <- coefficient_names == "(Intercept)"
  terms[intercept] <- "(Intercept)"

  model_frame <- stats::model.frame(fit)
  predictor_name <- spec$predictor[[1]]
  predictor <- model_frame[[predictor_name]]
  if (is.factor(predictor)) {
    design <- stats::model.matrix(fit)
    assignment <- attr(design, "assign")
    design_columns <- colnames(design)
    predictor_columns <- design_columns[assignment == 1L]
    coefficient_rows <- match(predictor_columns, coefficient_names)
    contrast_matrix <- stats::contrasts(predictor)
    contrast_columns <- colnames(contrast_matrix)
    represented_levels <- vapply(contrast_columns, function(column) {
      nonzero <- which(contrast_matrix[, column] != 0)
      if (length(nonzero) == 1L && contrast_matrix[nonzero, column] == 1) {
        rownames(contrast_matrix)[[nonzero]]
      } else {
        NA_character_
      }
    }, character(1))

    valid <- !is.na(coefficient_rows)
    terms[coefficient_rows[valid]] <- predictor_name
    levels[coefficient_rows[valid]] <- represented_levels[valid]
  } else {
    predictor_row <- match(predictor_name, coefficient_names)
    if (!is.na(predictor_row)) {
      terms[[predictor_row]] <- predictor_name
    }
  }

  for (i in seq_along(spec$covariates[[1]])) {
    covariate_id <- spec$covariate_ids[[1]][[i]]
    model_term <- spec$model_term_specs[[1]][[covariate_id]]
    if (is.null(model_term)) next
    for (j in seq_along(model_term$output_names)) {
      row <- match(model_term$output_names[[j]], coefficient_names)
      if (!is.na(row)) {
        terms[[row]] <- spec$covariates[[1]][[i]]
        levels[[row]] <- paste0("basis_", j)
      }
    }
  }

  tibble::tibble(term = unname(terms), level = unname(levels))
}

tidy_builtin_test <- function(fit, spec, frame) {
  null_formula <- rlang::new_formula(
    lhs = rlang::sym(spec$outcome[[1]]),
    rhs = 1,
    env = baseenv()
  )
  if (!inherits(fit, "glm")) {
    null_fit <- stats::lm(null_formula, data = frame, na.action = stats::na.omit)
    comparison <- stats::anova(null_fit, fit)
    statistic <- comparison$F[[2]]
    df <- comparison$Df[[2]]
    p_value <- comparison$`Pr(>F)`[[2]]
    test <- "partial_f"
  } else {
    family <- builtin_glm_family(spec)
    null_fit <- stats::glm(
      null_formula,
      data = frame,
      family = family,
      na.action = stats::na.omit
    )
    comparison <- stats::anova(null_fit, fit, test = "Chisq")
    statistic <- comparison$Deviance[[2]]
    df <- comparison$Df[[2]]
    p_value <- comparison$`Pr(>Chi)`[[2]]
    test <- "likelihood_ratio"
  }
  overall <- tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]],
    test = test,
    statistic = unname(statistic),
    df = unname(as.numeric(df)),
    p_value = unname(p_value),
    method = spec$method[[1]]
  )
  predictor_omnibus <- NULL
  model_predictor <- stats::model.frame(fit)[[spec$predictor[[1]]]]
  if (is.factor(model_predictor)) {
    reduced_terms <- c(formula_covariates <- model_formula_covariates(
      spec$covariate_ids[[1]], spec$covariates[[1]], spec$model_term_specs[[1]]
    ), spec$effect_modifiers[[1]])
    reduced_formula <- stats::reformulate(
      reduced_terms, response = spec$outcome[[1]]
    )
    if (!inherits(fit, "glm")) {
      reduced <- stats::lm(reduced_formula, data = frame, na.action = stats::na.omit)
      comparison <- stats::anova(reduced, fit)
      statistic <- comparison$F[[2]]; df <- comparison$Df[[2]]
      p_value <- comparison$`Pr(>F)`[[2]]
    } else {
      family <- builtin_glm_family(spec)
      reduced <- stats::glm(
        reduced_formula, data = frame, family = family,
        na.action = stats::na.omit
      )
      comparison <- stats::anova(reduced, fit, test = "Chisq")
      statistic <- comparison$Deviance[[2]]; df <- comparison$Df[[2]]
      p_value <- comparison$`Pr(>Chi)`[[2]]
    }
    predictor_omnibus <- tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], test = "predictor_omnibus",
      statistic = unname(statistic), df = as.numeric(df),
      p_value = unname(p_value), method = spec$method[[1]]
    )
  }
  formula_covariates <- model_formula_covariates(
    spec$covariate_ids[[1]], spec$covariates[[1]], spec$model_term_specs[[1]]
  )
  interaction_tests <- lapply(spec$effect_modifiers[[1]], function(modifier) {
    reduced_formula <- new_analysis_formula(
      spec$outcome[[1]], spec$predictor[[1]],
      c(formula_covariates, modifier), character()
    )
    if (!inherits(fit, "glm")) {
      reduced <- stats::lm(reduced_formula, data = frame, na.action = stats::na.omit)
      comparison <- stats::anova(reduced, fit)
      statistic <- comparison$F[[2]]
      p_value <- comparison$`Pr(>F)`[[2]]
      df <- comparison$Df[[2]]
    } else {
      family <- builtin_glm_family(spec)
      reduced <- stats::glm(reduced_formula, data = frame,
        family = family, na.action = stats::na.omit)
      comparison <- stats::anova(reduced, fit, test = "Chisq")
      statistic <- comparison$Deviance[[2]]
      p_value <- comparison$`Pr(>Chi)`[[2]]
      df <- comparison$Df[[2]]
    }
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], test = "interaction",
      statistic = unname(statistic), df = as.numeric(df),
      p_value = unname(p_value), method = spec$method[[1]]
    )
  })
  model_term_tests <- lapply(seq_along(spec$covariates[[1]]), function(i) {
    covariate_id <- spec$covariate_ids[[1]][[i]]
    term <- spec$model_term_specs[[1]][[covariate_id]]
    if (is.null(term)) return(NULL)
    reduced_covariates <- setdiff(formula_covariates, term$output_names)
    reduced_formula <- new_analysis_formula(
      spec$outcome[[1]], spec$predictor[[1]], reduced_covariates,
      spec$effect_modifiers[[1]]
    )
    if (!inherits(fit, "glm")) {
      reduced <- stats::lm(reduced_formula, data = frame, na.action = stats::na.omit)
      comparison <- stats::anova(reduced, fit)
      statistic <- comparison$F[[2]]
      p_value <- comparison$`Pr(>F)`[[2]]
      df <- comparison$Df[[2]]
    } else {
      family <- builtin_glm_family(spec)
      reduced <- stats::glm(
        reduced_formula, data = frame, family = family,
        na.action = stats::na.omit
      )
      comparison <- stats::anova(reduced, fit, test = "Chisq")
      statistic <- comparison$Deviance[[2]]
      p_value <- comparison$`Pr(>Chi)`[[2]]
      df <- comparison$Df[[2]]
    }
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$covariates[[1]][[i]], test = "model_term_omnibus",
      statistic = unname(statistic), df = as.numeric(df),
      p_value = unname(p_value), method = spec$method[[1]]
    )
  })
  vctrs::vec_rbind(
    overall, predictor_omnibus, !!!interaction_tests, !!!model_term_tests
  )
}

diagnose_builtin <- function(fit, spec) {
  if (!inherits(fit, "glm")) {
    fit_summary <- summary(fit)
    metrics <- c(
      sigma = fit_summary$sigma,
      r_squared = fit_summary$r.squared,
      adjusted_r_squared = fit_summary$adj.r.squared
    )
  } else {
    metrics <- c(
      converged = as.numeric(isTRUE(fit$converged)),
      deviance = fit$deviance,
      null_deviance = fit$null.deviance,
      dispersion_ratio = fit$deviance / fit$df.residual
    )
  }
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    metric = names(metrics),
    value = unname(as.numeric(metrics)),
    status = "observed",
    message = NA_character_
  )
}

