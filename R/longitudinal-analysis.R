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
#' @return An `analysis_plan` with one row per outcome.
#' @export
plan_longitudinal <- function(
  .data, outcomes = tidyselect::everything(), method = lmm_model(),
  confidence_level = 0.95
) {
  check_bq_data(.data); check_confidence_level(confidence_level)
  if (!inherits(method, "longitudinal_method_spec")) {
    stop_invalid_longitudinal_design("`method` must be a longitudinal method specification.")
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
    if (binary_engine && variable_type != "binary") issues <- c(
      issues, "Binary longitudinal engines require a binary repeated outcome."
    )
    if (!binary_engine && variable_type != "continuous") issues <- c(
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
    plan$formula[[i]] <- if (plan$engine[[i]] %in% c("lme4_lmer", "lme4_glmer")) {
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
    std_error_scale = if (exponentiate) "log_odds" else "outcome_units",
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
  if (inherits(fit, "merMod")) {
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
  list(model = fit, estimates = estimates, tests = tests, diagnostics = diagnostics)
}
