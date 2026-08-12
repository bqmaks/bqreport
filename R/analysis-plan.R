#' Compile a univariable analysis plan
#'
#' `plan_analysis()` creates one inspectable task for each selected
#' outcome-predictor pair. The initial vertical slice supports continuous
#' outcomes with linear models and binary outcomes with logistic models.
#'
#' @param .data A `bq_data` object.
#' @param outcomes Outcome columns selected with tidyselect.
#' @param predictors Predictor columns selected with tidyselect.
#' @param covariates Optional adjustment covariates selected with tidyselect.
#' @param weights Optional single configured weight column.
#' @param cluster Optional matched-set cluster column.
#' @param variance Variance estimator. Defaults to `robust` for IPW and
#'   `model_based` otherwise.
#' @param confidence_level Confidence level in the open interval `(0, 1)`.
#'
#' @return An `analysis_plan` tibble.
#' @export
plan_analysis <- function(
  .data,
  outcomes = all_outcomes(),
  predictors = all_predictors(),
  covariates = tidyselect::any_of(character()),
  weights = tidyselect::any_of(character()),
  cluster = tidyselect::any_of(character()),
  variance = NULL,
  confidence_level = 0.95
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  outcome_selection <- tidyselect::eval_select(rlang::enquo(outcomes), .data)
  predictor_selection <- tidyselect::eval_select(rlang::enquo(predictors), .data)
  covariate_selection <- tidyselect::eval_select(rlang::enquo(covariates), .data)
  weight_selection <- tidyselect::eval_select(rlang::enquo(weights), .data)
  cluster_selection <- tidyselect::eval_select(rlang::enquo(cluster), .data)
  if (length(weight_selection) > 1L) stop_plan("Select at most one weight.", "bq_error_invalid_weight")
  if (length(cluster_selection) > 1L) stop_plan("Select at most one cluster.", "bq_error_invalid_cluster")
  if (length(cluster_selection) && !is.null(variance) && variance != "cluster_robust") {
    stop_plan("A cluster requires `variance = \"cluster_robust\"`.", "bq_error_invalid_cluster")
  }
  registry <- variables(.data)
  pairs <- expand.grid(
    outcome = names(outcome_selection),
    predictor = names(predictor_selection),
    stringsAsFactors = FALSE
  )

  if (nrow(pairs) == 0L) {
    return(empty_analysis_plan())
  }

  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    outcome_name <- pairs$outcome[[i]]
    predictor_name <- pairs$predictor[[i]]
    outcome_spec <- registry[match(outcome_name, registry$name), , drop = FALSE]
    predictor_spec <- registry[match(predictor_name, registry$name), , drop = FALSE]
    method <- default_method_spec(outcome_spec$type[[1]])
    same_variable <- identical(outcome_spec$var_id[[1]], predictor_spec$var_id[[1]])
    unsupported_predictor <- predictor_spec$type[[1]] %in% c(
      "unknown", "identifier", "date", "datetime"
    )
    needs_review <- outcome_spec$status[[1]] != "valid" ||
      predictor_spec$status[[1]] != "valid"

    status <- if (is.null(method) || unsupported_predictor || same_variable) {
      "invalid"
    } else if (needs_review) {
      "review"
    } else {
      "ready"
    }
    reason <- if (is.null(method)) {
      paste0("Unsupported outcome type `", outcome_spec$type[[1]], "`.")
    } else if (unsupported_predictor) {
      paste0("Unsupported predictor type `", predictor_spec$type[[1]], "`.")
    } else if (same_variable) {
      "Outcome and predictor refer to the same variable."
    } else if (needs_review) {
      "Outcome or predictor metadata require review."
    } else {
      NA_character_
    }

    analysis_plan_row(
      outcome_spec,
      predictor_spec,
      method,
      status,
      reason,
      confidence_level,
      contrast_ids_for(.data, predictor_spec$var_id[[1]]),
      registry[match(names(covariate_selection), registry$name), , drop = FALSE],
      if (length(weight_selection)) registry[match(names(weight_selection), registry$name), , drop = FALSE] else NULL,
      variance,
      if (length(cluster_selection)) registry[match(names(cluster_selection), registry$name), , drop = FALSE] else NULL
    )
  })

  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

analysis_plan_row <- function(
  outcome_spec,
  predictor_spec,
  method,
  status,
  reason,
  confidence_level,
  contrast_ids = character(),
  covariate_specs = NULL,
  weight_spec = NULL,
  variance = NULL,
  cluster_spec = NULL
) {
  outcome <- outcome_spec$name[[1]]
  predictor <- predictor_spec$name[[1]]
  covariate_names <- if (is.null(covariate_specs)) character() else covariate_specs$name
  weight_id <- if (is.null(weight_spec)) NA_character_ else weight_spec$var_id[[1]]
  weight_name <- if (is.null(weight_spec)) NA_character_ else weight_spec$name[[1]]
  weight_type <- if (is.null(weight_spec)) NA_character_ else weight_spec$weight_type[[1]]
  cluster_id <- if (is.null(cluster_spec)) NA_character_ else cluster_spec$var_id[[1]]
  cluster_name <- if (is.null(cluster_spec)) NA_character_ else cluster_spec$name[[1]]
  cluster_type <- if (is.null(cluster_spec)) NA_character_ else cluster_spec$cluster_type[[1]]
  if (is.null(variance)) variance <- if (!is.null(cluster_spec)) "cluster_robust" else if (identical(weight_type, "ipw")) "robust" else "model_based"
  if (is.null(method)) {
    candidate_method_ids <- character()
    method_id <- method_engine <- method_estimator <- method_ci <- NA_character_
    method_family <- method_link <- method_effect <- NA_character_
    method_reason <- reason
  } else {
    candidate_method_ids <- method$method
    method_id <- method$method
    method_engine <- method$engine
    method_estimator <- method$estimator
    method_ci <- method$ci_method
    method_family <- method$family
    method_link <- method$link
    method_effect <- method$effect_measure
    method_reason <- method$selection_reason
  }
  tibble::tibble(
    analysis_id = paste0("analysis_", uuid::UUIDgenerate()),
    analysis_type = "univariable_regression",
    outcome_id = outcome_spec$var_id[[1]],
    predictor_id = predictor_spec$var_id[[1]],
    covariate_ids = list(if (is.null(covariate_specs)) character() else covariate_specs$var_id),
    weight_id = weight_id,
    cluster_id = cluster_id,
    outcome = outcome,
    predictor = predictor,
    covariates = list(covariate_names),
    weight = weight_name,
    cluster = cluster_name,
    design = NA_character_,
    data_layout = "cross_sectional",
    reshape_spec = list(NULL),
    method_policy = "system_default",
    selector_id = NA_character_,
    candidate_methods = list(candidate_method_ids),
    method = method_id,
    engine = method_engine,
    estimator = method_estimator,
    ci_method = method_ci,
    formula = list(new_analysis_formula(outcome, c(predictor, covariate_names))),
    family = method_family,
    link = method_link,
    effect_measure = method_effect,
    selection_reason = method_reason,
    selection_diagnostics = list(tibble::tibble()),
    function_id = NA_character_,
    function_hash = NA_character_,
    required_packages = list("stats"),
    contrast_ids = list(contrast_ids),
    adjust_method = "none",
    missing_policy = "complete_case",
    confidence_level = confidence_level,
    weight_type = weight_type,
    variance = variance,
    weight_diagnostics = list(tibble::tibble()),
    cluster_type = cluster_type,
    cluster_diagnostics = list(tibble::tibble()),
    n_total = NA_integer_,
    n_eligible = NA_integer_,
    n_analyzed = NA_integer_,
    n_missing_outcome = NA_integer_,
    n_missing_predictor = NA_integer_,
    validated = FALSE,
    approved = FALSE,
    status = status,
    reason = reason
  )
}

default_method_spec <- function(outcome_type) {
  if (identical(outcome_type, "continuous")) {
    return(list(
      method = "linear_model",
      engine = "lm",
      estimator = "ordinary_least_squares",
      ci_method = "t",
      family = "gaussian",
      link = "identity",
      effect_measure = "mean_difference",
      selection_reason = "System default for a continuous outcome."
    ))
  }
  if (identical(outcome_type, "binary")) {
    return(list(
      method = "logistic_model",
      engine = "glm",
      estimator = "maximum_likelihood",
      ci_method = "wald",
      family = "binomial",
      link = "logit",
      effect_measure = "odds_ratio",
      selection_reason = "System default for a binary outcome."
    ))
  }
  NULL
}

new_analysis_plan <- function(x) {
  class(x) <- c("analysis_plan", setdiff(class(x), "analysis_plan"))
  x
}

empty_analysis_plan <- function() {
  prototype <- analysis_plan_row(
    tibble::tibble(var_id = "", name = ""),
    tibble::tibble(var_id = "", name = ""),
    NULL,
    "invalid",
    NA_character_,
    0.95,
    character(), NULL, NULL, NULL, NULL
  )
  new_analysis_plan(prototype[0, , drop = FALSE])
}

contrast_ids_for <- function(data, predictor_id) {
  registry <- attr(data, "contrast_registry", exact = TRUE)
  if (nrow(registry) == 0L) return(character())
  registry$contrast_id[registry$predictor_id == predictor_id]
}

#' Validate an analysis plan against data
#'
#' Preflight resolves variables by stable identifiers, refreshes formulas and
#' counts complete observations while treating labelled special missing values
#' as missing only in the internal analysis view.
#'
#' @param plan An `analysis_plan`.
#' @param data A `bq_data` object.
#'
#' @return A validated `analysis_plan`.
#' @export
validate_plan <- function(plan, data) {
  if (!inherits(plan, "analysis_plan")) {
    stop_plan("`plan` must be an analysis_plan.", "bq_error_invalid_plan")
  }
  check_bq_data(data)
  registry <- variables(data)
  out <- tibble::as_tibble(plan)

  for (i in seq_len(nrow(out))) {
    out$validated[[i]] <- TRUE
    outcome_row <- match(out$outcome_id[[i]], registry$var_id)
    predictor_row <- match(out$predictor_id[[i]], registry$var_id)
    issues <- character()

    if (is.na(outcome_row) || is.na(predictor_row)) {
      issues <- c(issues, "A variable referenced by stable id is absent from the data.")
      out$status[[i]] <- "invalid"
      out$reason[[i]] <- append_reasons(out$reason[[i]], issues)
      next
    }

    outcome_spec <- registry[outcome_row, , drop = FALSE]
    predictor_spec <- registry[predictor_row, , drop = FALSE]
    covariate_rows <- match(out$covariate_ids[[i]], registry$var_id)
    if (anyNA(covariate_rows)) issues <- c(issues, "A covariate is absent from the data.")
    covariate_names <- registry$name[covariate_rows[!is.na(covariate_rows)]]
    outcome_name <- outcome_spec$name[[1]]
    predictor_name <- predictor_spec$name[[1]]
    out$outcome[[i]] <- outcome_name
    out$predictor[[i]] <- predictor_name
    out$covariates[[i]] <- covariate_names
    out$formula[[i]] <- new_analysis_formula(outcome_name, c(predictor_name, covariate_names))

    outcome <- data[[outcome_name]]
    predictor <- data[[predictor_name]]
    missing_outcome <- special_missing_mask(outcome)
    missing_predictor <- special_missing_mask(predictor)
    analyzed <- !(missing_outcome | missing_predictor)
    for (name in covariate_names) analyzed <- analyzed & !special_missing_mask(data[[name]])
    if (!is.na(out$weight_id[[i]])) {
      weight_row <- match(out$weight_id[[i]], registry$var_id)
      if (is.na(weight_row)) {
        issues <- c(issues, "Configured weight is absent from the data.")
      } else {
        weight_name <- registry$name[[weight_row]]
        out$weight[[i]] <- weight_name
        weights <- data[[weight_name]]
        missing_weights <- is.na(weights)
        negative_weights <- !missing_weights & weights < 0
        if (any(missing_weights)) issues <- c(issues, "Weight contains missing values.")
        if (any(negative_weights)) issues <- c(issues, "Weight contains negative values.")
        positive <- weights[!missing_weights & weights > 0]
        out$weight_diagnostics[[i]] <- tibble::tibble(
          min = if (length(positive)) min(positive) else NA_real_,
          max = if (length(positive)) max(positive) else NA_real_,
          sum = sum(positive), effective_n = if (length(positive)) sum(positive)^2 / sum(positive^2) else 0,
          n_zero = as.integer(sum(weights == 0, na.rm = TRUE)), n_missing = as.integer(sum(missing_weights))
        )
        analyzed <- analyzed & !missing_weights & !negative_weights & weights > 0
        if (out$weight_type[[i]] == "ipw" && !out$variance[[i]] %in% c("robust", "cluster_robust")) {
          issues <- c(issues, "IPW requires robust variance.")
        }
        if (out$variance[[i]] == "robust" && !requireNamespace("sandwich", quietly = TRUE)) {
          issues <- c(issues, "Robust variance requires package `sandwich`.")
        }
      }
    }
    if (!is.na(out$cluster_id[[i]])) {
      cluster_row <- match(out$cluster_id[[i]], registry$var_id)
      if (is.na(cluster_row)) {
        issues <- c(issues, "Configured cluster is absent from the data.")
      } else {
        cluster_name <- registry$name[[cluster_row]]
        out$cluster[[i]] <- cluster_name
        cluster_values <- data[[cluster_name]]
        missing_cluster <- is.na(cluster_values)
        analyzed <- analyzed & !missing_cluster
        sizes <- table(cluster_values[!missing_cluster])
        out$cluster_diagnostics[[i]] <- tibble::tibble(
          n_clusters = as.integer(length(sizes)),
          min_size = if (length(sizes)) as.integer(min(sizes)) else NA_integer_,
          median_size = if (length(sizes)) as.numeric(stats::median(sizes)) else NA_real_,
          max_size = if (length(sizes)) as.integer(max(sizes)) else NA_integer_,
          n_singleton = as.integer(sum(sizes == 1L)),
          n_missing = as.integer(sum(missing_cluster))
        )
        if (length(sizes) < 2L) issues <- c(issues, "Cluster-robust variance requires at least two clusters.")
        if (!requireNamespace("sandwich", quietly = TRUE)) issues <- c(issues, "Cluster-robust variance requires package `sandwich`.")
      }
    }
    n_total <- nrow(data)
    out$n_total[[i]] <- n_total
    out$n_eligible[[i]] <- n_total
    out$n_analyzed[[i]] <- sum(analyzed)
    out$n_missing_outcome[[i]] <- sum(missing_outcome)
    out$n_missing_predictor[[i]] <- sum(missing_predictor)

    outcome_values <- unclass_for_validation(outcome)[analyzed]
    predictor_values <- unclass_for_validation(predictor)[analyzed]
    if (length(outcome_values) == 0L) {
      issues <- c(issues, "No complete observations are available for analysis.")
    } else {
      if (n_distinct_values(outcome_values) < 2L) {
        issues <- c(issues, "Outcome has no variation in the analyzed data.")
      }
      if (n_distinct_values(predictor_values) < 2L) {
        issues <- c(issues, "Predictor has no variation in the analyzed data.")
      }
    }

    if (
      outcome_spec$type[[1]] == "binary" &&
        n_distinct_values(outcome_values) != 2L
    ) {
      issues <- c(issues, "Binary outcome must have exactly two observed values.")
    }
    if (
      predictor_spec$type[[1]] == "binary" &&
        n_distinct_values(predictor_values) != 2L
    ) {
      issues <- c(issues, "Binary predictor must have exactly two observed values.")
    }
    if (
      outcome_spec$type[[1]] == "continuous" &&
        !is.numeric(outcome_values)
    ) {
      issues <- c(issues, "Continuous outcome must use numeric data.")
    }
    if (
      predictor_spec$type[[1]] %in% c("continuous", "count") &&
        !is.numeric(predictor_values)
    ) {
      issues <- c(issues, "Continuous or count predictor must use numeric data.")
    }

    if (outcome_spec$type[[1]] == "binary") {
      event <- outcome_spec$event_value[[1]]
      if (is.null(event)) {
        issues <- c(issues, "Binary outcome has no configured event value.")
      } else if (!event %in% outcome_values) {
        issues <- c(issues, "Configured event value is absent from the analyzed outcome.")
      }
    }
    if (predictor_spec$type[[1]] %in% c("binary", "ordinal", "nominal")) {
      reference <- predictor_spec$reference[[1]]
      if (is.null(reference)) {
        issues <- c(issues, "Categorical predictor has no configured reference value.")
      } else if (!reference %in% predictor_values) {
        issues <- c(issues, "Configured reference value is absent from the analyzed predictor.")
      }
    }

    if (length(issues) > 0L) {
      out$status[[i]] <- "invalid"
      out$reason[[i]] <- append_reasons(out$reason[[i]], issues)
    }
  }

  new_analysis_plan(out)
}

#' Approve reviewed analysis tasks
#'
#' Approval is explicit and can only promote tasks that have completed
#' preflight validation. Invalid tasks cannot be approved.
#'
#' @param plan A validated `analysis_plan`.
#' @param analysis_id Optional character vector of analysis identifiers. If
#'   omitted, all tasks currently in `review` are selected.
#'
#' @return An updated `analysis_plan`.
#' @export
approve_plan <- function(plan, analysis_id = NULL) {
  if (!inherits(plan, "analysis_plan")) {
    stop_plan("`plan` must be an analysis_plan.", "bq_error_invalid_plan")
  }
  out <- tibble::as_tibble(plan)

  if (is.null(analysis_id)) {
    rows <- which(out$status == "review")
  } else {
    valid_ids <- is.character(analysis_id) && length(analysis_id) > 0L &&
      !anyNA(analysis_id)
    if (!valid_ids) {
      stop_plan(
        "`analysis_id` must be a non-missing character vector.",
        "bq_error_unknown_analysis_id"
      )
    }
    unknown <- setdiff(analysis_id, out$analysis_id)
    if (length(unknown) > 0L) {
      stop_plan(
        paste0("Unknown analysis ids: ", paste(unknown, collapse = ", "), "."),
        "bq_error_unknown_analysis_id"
      )
    }
    rows <- match(unique(analysis_id), out$analysis_id)
  }

  if (length(rows) == 0L) {
    return(new_analysis_plan(out))
  }
  if (any(!out$validated[rows])) {
    stop_plan(
      "Reviewed tasks must pass `validate_plan()` before approval.",
      "bq_error_unvalidated_plan"
    )
  }
  if (any(out$status[rows] == "invalid")) {
    invalid_ids <- out$analysis_id[rows][out$status[rows] == "invalid"]
    stop_plan(
      paste0(
        "Invalid tasks cannot be approved: ",
        paste(invalid_ids, collapse = ", "),
        "."
      ),
      "bq_error_invalid_plan_approval"
    )
  }

  review_rows <- rows[out$status[rows] == "review"]
  out$status[review_rows] <- "ready"
  out$approved[review_rows] <- TRUE
  out$reason[review_rows] <- NA_character_
  new_analysis_plan(out)
}

special_missing_mask <- function(x) {
  raw <- unclass_for_validation(x)
  missing <- is.na(raw)
  na_values <- labelled::na_values(x)
  na_range <- labelled::na_range(x)
  if (!is.null(na_values)) {
    missing <- missing | raw %in% na_values
  }
  if (!is.null(na_range)) {
    missing <- missing | (!is.na(raw) & raw >= na_range[[1]] & raw <= na_range[[2]])
  }
  missing
}

unclass_for_validation <- function(x) {
  if (inherits(x, "vctrs_vctr")) vctrs::vec_data(x) else x
}

n_distinct_values <- function(x) length(unique(x))

new_analysis_formula <- function(outcome, predictors) {
  rlang::new_formula(
    lhs = rlang::sym(outcome),
    rhs = Reduce(function(x, y) rlang::call2("+", x, y), lapply(predictors, rlang::sym)),
    env = baseenv()
  )
}

append_reasons <- function(existing, additions) {
  existing <- if (is.na(existing)) character() else existing
  paste(unique(c(existing, additions)), collapse = " ")
}

check_confidence_level <- function(x) {
  valid <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x > 0 && x < 1
  if (!valid) {
    stop_plan(
      "`confidence_level` must be a single number strictly between 0 and 1.",
      "bq_error_invalid_plan"
    )
  }
  invisible(x)
}

stop_plan <- function(message, class) {
  condition <- structure(
    list(message = message, call = sys.call(-1L)),
    class = c(class, "error", "condition")
  )
  stop(condition)
}
