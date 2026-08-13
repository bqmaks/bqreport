#' Construct longitudinal model specifications
#' @param reml Whether LMM estimation uses REML. Defaults to FALSE so nested
#'   fixed-effect tests are comparable.
#' @return A concrete longitudinal method specification.
#' @export
lmm_model <- function(reml = FALSE) {
  if (!is.logical(reml) || length(reml) != 1L || is.na(reml)) {
    stop_invalid_longitudinal_design("`reml` must be TRUE or FALSE.")
  }
  structure(list(
    id = "linear_mixed_model", engine = "lme4_lmer",
    estimand = "subject_specific", effect_measure = "mean_difference",
    scale = "outcome_units", required_packages = "lme4", reml = reml
  ), class = "longitudinal_method_spec")
}

#' @rdname lmm_model
#' @param correlation Working correlation structure for GEE.
#' @export
gee_model <- function(correlation = c("exchangeable", "independence", "ar1")) {
  correlation <- match.arg(correlation)
  structure(list(
    id = "gaussian_gee", engine = "geepack_geeglm",
    estimand = "population_average", effect_measure = "mean_difference",
    scale = "outcome_units", required_packages = "geepack",
    correlation = correlation
  ), class = "longitudinal_method_spec")
}

#' @rdname lmm_model
#' @export
glmm_model <- function() {
  structure(list(
    id = "logistic_mixed_model", engine = "lme4_glmer",
    estimand = "subject_specific", effect_measure = "odds_ratio",
    scale = "ratio", model_scale = "log_odds", required_packages = "lme4"
  ), class = "longitudinal_method_spec")
}

#' @rdname lmm_model
#' @param family Negative-binomial variance parameterization.
#' @export
negative_binomial_glmm_model <- function(
  family = c("nbinom2", "nbinom1")
) {
  family <- match.arg(family)
  structure(list(
    id = paste0("negative_binomial_mixed_model_", family),
    engine = paste0("glmmTMB_", family), estimand = "subject_specific",
    effect_measure = "rate_ratio", scale = "ratio", model_scale = "log_rate",
    required_packages = "glmmTMB", family = family
  ), class = "longitudinal_method_spec")
}

#' @rdname lmm_model
#' @export
binary_gee_model <- function(
  correlation = c("exchangeable", "independence", "ar1")
) {
  correlation <- match.arg(correlation)
  structure(list(
    id = "logistic_gee", engine = "geepack_geeglm",
    estimand = "population_average", effect_measure = "odds_ratio",
    scale = "ratio", model_scale = "log_odds", required_packages = "geepack",
    correlation = correlation, family = "binomial"
  ), class = "longitudinal_method_spec")
}

#' Compile a longitudinal analysis plan
#' @param .data A `bq_data` object with one longitudinal design.
#' @param outcomes Registered longitudinal outcomes selected by name.
#' @param method A longitudinal method specification.
#' @param confidence_level Confidence level.
#' @param comparisons Whether to compute change-from-baseline and
#'   difference-in-changes estimands.
#' @param adjust Multiplicity adjustment for the longitudinal contrast family.
#' @return An `analysis_plan` with one row per outcome.
#' @export
plan_longitudinal <- function(
  .data, outcomes = tidyselect::everything(), method = lmm_model(),
  confidence_level = 0.95, comparisons = TRUE, adjust = "none"
) {
  check_bq_data(.data); check_confidence_level(confidence_level)
  if (!inherits(method, "longitudinal_method_spec")) {
    stop_invalid_longitudinal_design("`method` must be a longitudinal method specification.")
  }
  if (!is.logical(comparisons) || length(comparisons) != 1L || is.na(comparisons) ||
      !is.character(adjust) || length(adjust) != 1L ||
      !adjust %in% stats::p.adjust.methods) {
    stop_invalid_longitudinal_design("Invalid longitudinal comparison settings.")
  }
  design <- designs(.data)
  registered <- resolve_outcomes(.data)
  registered <- registered[registered$type == "longitudinal", , drop = FALSE]
  selection_data <- stats::setNames(
    as.data.frame(rep(list(logical()), nrow(registered))), registered$name
  )
  selected <- tidyselect::eval_select(rlang::enquo(outcomes), selection_data)
  if (!length(selected)) return(empty_analysis_plan())
  registry <- variables(.data)
  predictor_id <- if (!is.na(design$group_var_id[[1]])) {
    design$group_var_id[[1]]
  } else design$time_var_id[[1]]
  predictor <- registry[match(predictor_id, registry$var_id), , drop = FALSE]
  rows <- lapply(names(selected), function(outcome_name) {
    outcome <- registered[registered$name == outcome_name, , drop = FALSE]
    pseudo <- tibble::tibble(var_id = outcome$outcome_id, name = outcome$name)
    row <- analysis_plan_row(
      pseudo, predictor, method = NULL, status = "ready", reason = NA_character_,
      confidence_level = confidence_level
    )
    row$analysis_type <- "longitudinal_regression"
    row$longitudinal_outcome_id <- outcome$outcome_id
    row$design <- design$design_id[[1]]
    row$data_layout <- design$layout[[1]]
    row$reshape_spec <- list(attr(build_longitudinal_frame(.data, outcome_name),
      "reshape_spec"))
    row$method <- method$id; row$engine <- method$engine
    row$estimator <- method$estimand; row$estimand <- method$estimand
    row$effect_measure <- method$effect_measure
    row$model_scale <- if (is.null(method$model_scale)) method$scale else method$model_scale
    row$scale <- method$scale
    row$required_packages <- list(method$required_packages)
    row$method_object <- list(method); row$formula <- list(NULL)
    row$longitudinal_comparisons <- comparisons
    row$adjust_method <- adjust
    row <- refine_analysis_id(
      row, row$longitudinal_outcome_id[[1]], row$design[[1]],
      row$method[[1]], row$engine[[1]], row$estimand[[1]], adjust
    )
    row
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_longitudinal_task <- function(plan, i, data) {
  plan$validated[[i]] <- TRUE
  issues <- character()
  missing_packages <- plan$required_packages[[i]][
    !vapply(plan$required_packages[[i]], requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) issues <- c(issues, paste0(
    "Missing packages required by longitudinal method: ",
    paste(missing_packages, collapse = ", "), "."
  ))
  frame <- tryCatch(
    build_longitudinal_frame(data, plan$outcome[[i]]),
    error = function(condition) condition
  )
  if (inherits(frame, "error")) {
    issues <- c(issues, conditionMessage(frame))
  } else {
    outcome_registry <- outcomes(data)
    outcome_row <- match(plan$longitudinal_outcome_id[[i]], outcome_registry$outcome_id)
    variable_type <- outcome_registry$variable_type[[outcome_row]]
    binary_engine <- plan$engine[[i]] == "lme4_glmer" ||
      identical(plan$method_object[[i]]$family, "binomial")
    count_engine <- startsWith(plan$engine[[i]], "glmmTMB_")
    if (binary_engine && variable_type != "binary") issues <- c(
      issues, "Binary longitudinal engines require a binary repeated outcome."
    )
    if (count_engine && variable_type != "count") issues <- c(
      issues, "Negative-binomial longitudinal engines require a count repeated outcome."
    )
    if (!binary_engine && !count_engine && variable_type != "continuous") issues <- c(
      issues, "Gaussian longitudinal engines require a continuous repeated outcome."
    )
    complete <- stats::complete.cases(frame)
    frame <- frame[complete, , drop = FALSE]
    plan$n_total[[i]] <- nrow(build_longitudinal_frame(data, plan$outcome[[i]]))
    plan$n_eligible[[i]] <- plan$n_total[[i]]
    plan$n_analyzed[[i]] <- nrow(frame)
    plan$n_missing_outcome[[i]] <- plan$n_total[[i]] - nrow(frame)
    plan$n_missing_predictor[[i]] <- 0L
    if (length(unique(frame$..bq_id)) < 2L) {
      issues <- c(issues, "Longitudinal analysis requires at least two subjects.")
    }
    if (length(unique(frame$..bq_time)) < 2L) {
      issues <- c(issues, "Longitudinal time has no variation.")
    }
    if ("..bq_group" %in% names(frame) && length(unique(frame$..bq_group)) < 2L) {
      issues <- c(issues, "Longitudinal group has no variation.")
    }
    fixed <- if ("..bq_group" %in% names(frame)) {
      "..bq_group * ..bq_time"
    } else "..bq_time"
    plan$formula[[i]] <- if (plan$engine[[i]] %in% c("lme4_lmer", "lme4_glmer") ||
        startsWith(plan$engine[[i]], "glmmTMB_")) {
      stats::as.formula(paste0("..bq_outcome ~ ", fixed, " + (1 | ..bq_id)"))
    } else stats::as.formula(paste0("..bq_outcome ~ ", fixed))
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

execute_longitudinal_analysis <- function(spec, data) {
  frame <- build_longitudinal_frame(data, spec$outcome[[1]])
  frame <- frame[stats::complete.cases(frame), , drop = FALSE]
  method <- spec$method_object[[1]]
  if (method$engine == "lme4_lmer") {
    fit <- lme4::lmer(spec$formula[[1]], data = frame, REML = method$reml)
    singular_fit <- lme4::isSingular(fit)
    beta <- lme4::fixef(fit); covariance <- as.matrix(stats::vcov(fit))
    std_error <- sqrt(diag(covariance)); statistic <- beta / std_error
    p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    variance <- "model_based"; df <- rep(NA_real_, length(beta))
  } else if (method$engine == "lme4_glmer") {
    fit <- lme4::glmer(
      spec$formula[[1]], data = frame, family = stats::binomial("logit")
    )
    singular_fit <- lme4::isSingular(fit)
    beta <- lme4::fixef(fit); covariance <- as.matrix(stats::vcov(fit))
    std_error <- sqrt(diag(covariance)); statistic <- beta / std_error
    p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    variance <- "model_based"; df <- rep(NA_real_, length(beta))
  } else if (startsWith(method$engine, "glmmTMB_")) {
    family <- if (method$family == "nbinom1") glmmTMB::nbinom1() else glmmTMB::nbinom2()
    fit <- glmmTMB::glmmTMB(spec$formula[[1]], data = frame, family = family)
    beta <- glmmTMB::fixef(fit)$cond
    covariance <- as.matrix(stats::vcov(fit)$cond)
    std_error <- sqrt(diag(covariance)); statistic <- beta / std_error
    p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    variance <- "model_based"; df <- rep(NA_real_, length(beta))
  } else {
    family <- if (identical(method$family, "binomial")) {
      stats::binomial("logit")
    } else stats::gaussian()
    fit <- geepack::geeglm(
      spec$formula[[1]], id = frame$..bq_id, data = frame,
      family = family, corstr = method$correlation
    )
    coefficient <- summary(fit)$coefficients
    beta <- coefficient[, "Estimate"]; std_error <- coefficient[, "Std.err"]
    statistic <- coefficient[, "Wald"]
    p_value <- coefficient[, "Pr(>|W|)"]
    covariance <- stats::vcov(fit); df <- rep(NA_real_, length(beta))
    variance <- "sandwich"
  }
  critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
  exponentiate <- method$scale == "ratio"
  estimate_output <- if (exponentiate) exp(beta) else beta
  conf_low <- beta - critical * std_error; conf_high <- beta + critical * std_error
  if (exponentiate) {
    conf_low <- exp(conf_low); conf_high <- exp(conf_high)
  }
  estimates <- tibble::tibble(
    analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
    predictor = spec$predictor[[1]], stratum_label = NA_character_,
    transformation_id = NA_character_, transformation_label = NA_character_,
    term = names(beta), level = NA_character_, estimate = unname(estimate_output),
    std_error = unname(std_error),
    std_error_scale = if (startsWith(method$engine, "glmmTMB_")) {
      "log_rate"
    } else if (exponentiate) "log_odds" else "outcome_units",
    conf_low = unname(conf_low), conf_high = unname(conf_high),
    statistic = unname(statistic), df = df, p_value = unname(p_value),
    effect_measure = method$effect_measure, scale = method$scale,
    n = as.integer(nrow(frame)), n_events = NA_integer_, method = method$id,
    variance = variance
  )
  interaction_names <- grep(":", names(beta), value = TRUE)
  tests <- tests_prototype()
  if (length(interaction_names)) {
    interaction_beta <- beta[interaction_names]
    interaction_vcov <- covariance[interaction_names, interaction_names, drop = FALSE]
    statistic_joint <- tryCatch(drop(t(interaction_beta) %*%
      solve(interaction_vcov, interaction_beta)), error = function(e) NA_real_)
    tests <- tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], contrast = NA_character_,
      numerator = NA_character_, denominator = NA_character_,
      test = "group_by_time", statistic = statistic_joint,
      df = as.numeric(length(interaction_names)),
      p_value = if (is.na(statistic_joint)) NA_real_ else stats::pchisq(
        statistic_joint, length(interaction_names), lower.tail = FALSE
      ),
      p_adjusted = NA_real_, adjust_method = "none", method = method$id
    )
  }
  diagnostics <- tibble::tibble(
    analysis_id = spec$analysis_id[[1]], metric = c("n_subjects", "n_observations"),
    value = c(length(unique(frame$..bq_id)), nrow(frame)), status = "observed",
    message = NA_character_
  )
  if (inherits(fit, "glmmTMB")) {
    converged <- isTRUE(fit$sdr$pdHess) && isTRUE(fit$fit$convergence == 0L)
    diagnostics <- vctrs::vec_rbind(diagnostics, tibble::tibble(
      analysis_id = spec$analysis_id[[1]], metric = "converged",
      value = as.numeric(converged), status = if (converged) "ok" else "warning",
      message = if (converged) NA_character_ else "The glmmTMB fit did not converge."
    ))
  } else if (inherits(fit, "merMod")) {
    singular <- singular_fit
    diagnostics <- vctrs::vec_rbind(diagnostics, tibble::tibble(
      analysis_id = spec$analysis_id[[1]], metric = "singular",
      value = as.numeric(singular),
      status = if (singular) "warning" else "ok",
      message = if (singular) "The mixed model fit is singular." else NA_character_
    ))
  } else {
    convergence_error <- fit$geese$error
    diagnostics <- vctrs::vec_rbind(diagnostics, tibble::tibble(
      analysis_id = spec$analysis_id[[1]], metric = "convergence_code",
      value = as.numeric(convergence_error),
      status = if (convergence_error == 0) "ok" else "warning",
      message = if (convergence_error == 0) NA_character_ else
        "The GEE backend reported a non-zero convergence code."
    ))
  }
  contrasts <- if (isTRUE(spec$longitudinal_comparisons[[1]])) {
    compute_longitudinal_contrasts(fit, covariance, spec, frame)
  } else contrasts_prototype()
  list(
    model = fit, estimates = estimates, tests = tests,
    contrasts = contrasts, diagnostics = diagnostics
  )
}

compute_longitudinal_contrasts <- function(fit, covariance, spec, frame) {
  time <- frame$..bq_time
  time_levels <- if (is.factor(time)) levels(time) else sort(unique(time))
  baseline <- spec$reshape_spec[[1]]$baseline
  if (is.null(baseline)) baseline <- time_levels[[1]]
  followup <- setdiff(time_levels, as.character(baseline))
  if (!length(followup)) return(contrasts_prototype())
  grouped <- "..bq_group" %in% names(frame)
  groups <- if (grouped) levels(factor(frame$..bq_group)) else ".all"
  terms_object <- if (inherits(fit, "glmmTMB")) {
    stats::delete.response(stats::terms(lme4::nobars(stats::formula(fit))))
  } else if (inherits(fit, "merMod")) {
    stats::delete.response(stats::terms(stats::formula(fit, fixed.only = TRUE)))
  } else stats::delete.response(stats::terms(fit))
  beta <- if (inherits(fit, "glmmTMB")) {
    glmmTMB::fixef(fit)$cond
  } else if (inherits(fit, "merMod")) lme4::fixef(fit) else stats::coef(fit)
  cell_matrix <- function(group, time_value) {
    row <- frame[1, , drop = FALSE]
    row$..bq_id <- frame$..bq_id[[1]]
    row$..bq_time <- if (is.factor(time)) {
      factor(time_value, levels = levels(time))
    } else as.numeric(time_value)
    if (grouped) row$..bq_group <- factor(group, levels = groups)
    matrix <- stats::model.matrix(terms_object, row)
    matrix[, names(beta), drop = FALSE]
  }
  contrast_row <- function(vector, estimand, group, time_value, numerator, denominator) {
    estimate <- drop(vector %*% beta)
    std_error <- sqrt(drop(vector %*% covariance %*% t(vector)))
    statistic <- estimate / std_error
    critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
    conf_low <- estimate - critical * std_error
    conf_high <- estimate + critical * std_error
    exponentiate <- spec$scale[[1]] == "ratio"
    effect_measure <- if (estimand == "difference_in_changes") {
      if (exponentiate) "ratio_of_odds_ratios" else "difference_in_changes"
    } else if (exponentiate) "odds_ratio_change" else "change_from_baseline"
    if (exponentiate) {
      estimate <- exp(estimate); conf_low <- exp(conf_low); conf_high <- exp(conf_high)
    }
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]],
      contrast_id = bq_id(
        "contrast", spec$analysis_id[[1]], estimand, numerator, denominator,
        time_value
      ),
      contrast = paste0(numerator, " - ", denominator),
      numerator = numerator, denominator = denominator,
      modifier = if (grouped) "group_by_time" else "time",
      modifier_level = as.character(time_value),
      inner_contrast = paste0(time_value, " - ", baseline),
      outer_contrast = if (estimand == "difference_in_changes") {
        paste0(group, " - ", groups[[1]])
      } else NA_character_,
      estimand = estimand, exponentiated = exponentiate,
      estimate = as.numeric(estimate), std_error = as.numeric(std_error),
      std_error_scale = spec$model_scale[[1]], conf_low = conf_low,
      conf_high = conf_high,
      p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      p_adjusted = NA_real_, adjust_method = spec$adjust_method[[1]],
      effect_measure = effect_measure,
      scale = if (exponentiate) "ratio" else spec$scale[[1]]
    )
  }
  rows <- list()
  for (group in groups) for (time_value in followup) {
    change <- cell_matrix(group, time_value) - cell_matrix(group, baseline)
    rows[[length(rows) + 1L]] <- contrast_row(
      change, "change_from_baseline", group, time_value,
      paste0(group, "@", time_value), paste0(group, "@", baseline)
    )
    if (grouped && group != groups[[1]]) {
      reference_change <- cell_matrix(groups[[1]], time_value) -
        cell_matrix(groups[[1]], baseline)
      rows[[length(rows) + 1L]] <- contrast_row(
        change - reference_change, "difference_in_changes", group, time_value,
        paste0(group, " change"), paste0(groups[[1]], " change")
      )
    }
  }
  output <- vctrs::vec_rbind(!!!rows)
  output$p_adjusted <- stats::p.adjust(
    output$p_value, method = spec$adjust_method[[1]]
  )
  output
}
