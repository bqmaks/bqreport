#' Run a validated analysis plan
#'
#' Only validated tasks with status `ready` are executed. Other tasks are
#' retained in the result plan and represented in the issues component. Engine
#' failures never trigger a fallback method.
#'
#' @param plan A validated `analysis_plan`.
#' @param data A `bq_data` object.
#' @param error Runtime engine error handling: collect, stop, or warn.
#'
#' @return An `analysis_result`.
#' @export
run_analysis <- function(plan, data, error = c("collect", "stop", "warn")) {
  error <- match.arg(error)
  if (!inherits(plan, "analysis_plan")) {
    stop_plan("`plan` must be an analysis_plan.", "bq_error_invalid_plan")
  }
  check_bq_data(data)
  if (any(!plan$validated)) {
    stop_plan(
      "All plan tasks must pass `validate_plan()` before execution.",
      "bq_error_unvalidated_plan"
    )
  }

  plan <- validate_plan(plan, data)
  model_list <- list()
  estimate_rows <- list()
  test_rows <- list()
  diagnostic_rows <- list()
  issue_rows <- list()
  provenance_rows <- list()
  contrast_rows <- list()

  for (i in seq_len(nrow(plan))) {
    spec <- plan[i, , drop = FALSE]
    analysis_id <- spec$analysis_id[[1]]

    if (spec$status[[1]] != "ready") {
      severity <- if (spec$status[[1]] == "invalid") "error" else "info"
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id,
        "preflight",
        severity,
        paste0("bq_", spec$status[[1]], "_analysis"),
        if (is.na(spec$reason[[1]])) {
          paste0("Analysis status is `", spec$status[[1]], "`.")
        } else {
          spec$reason[[1]]
        }
      )
      next
    }

    frame <- build_analysis_frame(spec, data)
    captured_warnings <- character()
    fit <- tryCatch(
      withCallingHandlers(
        fit_builtin_engine(spec, frame),
        warning = function(condition) {
          captured_warnings <<- c(captured_warnings, conditionMessage(condition))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(condition) condition
    )

    if (inherits(fit, "error")) {
      message <- paste0(
        "Engine `", spec$engine[[1]], "` failed for analysis `",
        analysis_id, "`: ", conditionMessage(fit)
      )
      if (error == "stop") {
        stop_engine(message, analysis_id)
      }
      if (error == "warn") {
        warning(engine_warning(message, analysis_id), call. = FALSE)
      }
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "fit", "error", class(fit)[[1]], message
      )
      next
    }

    if ("..bq_cluster" %in% names(frame)) {
      attr(fit, "bq_clusters") <- frame[["..bq_cluster"]]
    }

    model_list[[analysis_id]] <- fit
    post_fit <- withCallingHandlers(
      list(
        estimates = tidy_builtin_estimates(fit, spec),
        tests = tidy_builtin_test(fit, spec, frame),
        diagnostics = diagnose_builtin(fit, spec)
      ),
      warning = function(condition) {
        captured_warnings <<- c(captured_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    estimate_rows[[length(estimate_rows) + 1L]] <- post_fit$estimates
    computed_contrasts <- compute_builtin_contrasts(post_fit$estimates, spec, data)
    if (nrow(computed_contrasts) > 0L) {
      contrast_rows[[length(contrast_rows) + 1L]] <- computed_contrasts
    }
    test_rows[[length(test_rows) + 1L]] <- post_fit$tests
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <- post_fit$diagnostics
    provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)

    for (message in unique(captured_warnings)) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "fit", "warning", "bq_warning_engine", message
      )
    }
  }

  structure(
    list(
      plan = plan,
      models = model_list,
      estimates = bind_component(estimate_rows, estimates_prototype()),
      contrasts = bind_component(contrast_rows, contrasts_prototype()),
      tests = bind_component(test_rows, tests_prototype()),
      descriptives = descriptives_prototype(),
      diagnostics = bind_component(diagnostic_rows, diagnostics_prototype()),
      issues = bind_component(issue_rows, issues_prototype()),
      provenance = bind_component(provenance_rows, provenance_prototype())
    ),
    class = "analysis_result"
  )
}

build_analysis_frame <- function(spec, data) {
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

  if (outcome_spec$type[[1]] == "binary") {
    event <- outcome_spec$event_value[[1]]
    outcome <- ifelse(is.na(outcome), NA_integer_, as.integer(outcome == event))
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
    if (!is.null(fit_weights)) {
      return(stats::glm(
        fit_formula, data = frame, family = stats::binomial("logit"),
        weights = fit_weights, na.action = stats::na.omit
      ))
    }
    return(stats::glm(
      fit_formula,
      data = frame,
      family = stats::binomial("logit"),
      na.action = stats::na.omit
    ))
  }
  stop(paste0("Unknown built-in engine `", spec$engine[[1]], "`."))
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
    estimate <- exp(beta)
    conf_low <- exp(beta - critical * standard_error)
    conf_high <- exp(beta + critical * standard_error)
    scale <- "ratio"
    std_error_scale <- "log_odds"
    n_events <- as.integer(sum(stats::model.response(stats::model.frame(fit)) == 1))
  }

  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]],
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
    null_fit <- stats::glm(
      null_formula,
      data = frame,
      family = stats::binomial("logit"),
      na.action = stats::na.omit
    )
    comparison <- stats::anova(null_fit, fit, test = "Chisq")
    statistic <- comparison$Deviance[[2]]
    df <- comparison$Df[[2]]
    p_value <- comparison$`Pr(>Chi)`[[2]]
    test <- "likelihood_ratio"
  }
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]],
    test = test,
    statistic = unname(statistic),
    df = unname(as.numeric(df)),
    p_value = unname(p_value),
    method = spec$method[[1]]
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
      null_deviance = fit$null.deviance
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

provenance_row <- function(spec) {
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    method = spec$method[[1]],
    engine = spec$engine[[1]],
    function_id = spec$function_id[[1]],
    function_hash = spec$function_hash[[1]],
    r_version = as.character(getRversion()),
    required_packages = spec$required_packages,
    package_versions = list(c(stats = as.character(utils::packageVersion("stats"))))
  )
}

issue_row <- function(analysis_id, stage, severity, condition_class, message) {
  tibble::tibble(
    analysis_id = analysis_id,
    stage = stage,
    severity = severity,
    condition_class = condition_class,
    message = message
  )
}

bind_component <- function(rows, prototype) {
  if (length(rows) == 0L) return(prototype)
  vctrs::vec_rbind(!!!rows)
}

estimates_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
    term = character(), level = character(), estimate = double(),
    std_error = double(), std_error_scale = character(), conf_low = double(),
    conf_high = double(), statistic = double(), df = double(), p_value = double(),
    effect_measure = character(), scale = character(), n = integer(),
    n_events = integer(), method = character(), variance = character()
  )
}

contrasts_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
      contrast_id = character(), contrast = character(), numerator = character(), denominator = character(),
    estimate = double(), conf_low = double(), conf_high = double(),
    p_value = double(), p_adjusted = double(), adjust_method = character(),
    effect_measure = character(), scale = character()
  )
}

tests_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
    test = character(), statistic = double(), df = double(), p_value = double(),
    method = character()
  )
}

descriptives_prototype <- function() tibble::tibble(analysis_id = character())

diagnostics_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), metric = character(), value = double(),
    status = character(), message = character()
  )
}

issues_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), stage = character(), severity = character(),
    condition_class = character(), message = character()
  )
}

provenance_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), method = character(), engine = character(),
    function_id = character(), function_hash = character(), r_version = character(),
    required_packages = list(), package_versions = list()
  )
}

stop_engine <- function(message, analysis_id) {
  condition <- structure(
    list(message = message, call = sys.call(-1L), analysis_id = analysis_id),
    class = c("bq_error_engine", "error", "condition")
  )
  stop(condition)
}

engine_warning <- function(message, analysis_id) {
  structure(
    list(message = message, call = NULL, analysis_id = analysis_id),
    class = c("bq_warning_engine", "warning", "condition")
  )
}

check_analysis_result <- function(x) {
  if (!inherits(x, "analysis_result")) {
    stop_plan("`x` must be an analysis_result.", "bq_error_invalid_result")
  }
  invisible(x)
}

#' Access analysis result components
#'
#' @param x An `analysis_result`.
#'
#' @return The requested tidy result component, or a named list for `models()`.
#' @export
estimates <- function(x) {
  check_analysis_result(x)
  x$estimates
}

#' @rdname contrasts
#' @export
contrasts.analysis_result <- function(x) {
  check_analysis_result(x)
  x$contrasts
}

#' @rdname estimates
#' @export
tests <- function(x) {
  check_analysis_result(x)
  x$tests
}

#' @rdname estimates
#' @export
diagnostics <- function(x) {
  check_analysis_result(x)
  x$diagnostics
}

#' @rdname estimates
#' @export
issues <- function(x) {
  check_analysis_result(x)
  x$issues
}

#' @rdname estimates
#' @export
models <- function(x) {
  check_analysis_result(x)
  x$models
}
