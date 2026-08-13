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
#' @param strata Optional columns defining independent analysis strata. Only
#'   observed complete combinations are compiled.
#' @param effect_modifiers Optional variables interacting with the predictor.
#' @param variance Variance estimator. Defaults to `robust` for IPW and
#'   `model_based` otherwise.
#' @param rules Optional concrete `analysis_rules`.
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
  strata = tidyselect::any_of(character()),
  effect_modifiers = tidyselect::any_of(character()),
  variance = NULL,
  rules = NULL,
  confidence_level = 0.95
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  if (!is.null(rules) && !inherits(rules, "analysis_rules")) {
    stop_method_contract("`rules` must be an analysis_rules object.")
  }
  outcome_selection <- tidyselect::eval_select(rlang::enquo(outcomes), .data)
  predictor_selection <- tidyselect::eval_select(rlang::enquo(predictors), .data)
  covariate_selection <- tidyselect::eval_select(rlang::enquo(covariates), .data)
  weight_selection <- tidyselect::eval_select(rlang::enquo(weights), .data)
  cluster_selection <- tidyselect::eval_select(rlang::enquo(cluster), .data)
  strata_selection <- tidyselect::eval_select(rlang::enquo(strata), .data)
  modifier_selection <- tidyselect::eval_select(rlang::enquo(effect_modifiers), .data)
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
  stratum_specs <- compile_stratum_specs(.data, names(strata_selection), registry)
  task_index <- expand.grid(
    pair = seq_len(nrow(pairs)),
    stratum = seq_along(stratum_specs),
    KEEP.OUT.ATTRS = FALSE
  )

  if (nrow(pairs) == 0L || length(stratum_specs) == 0L) {
    return(empty_analysis_plan())
  }

  rows <- lapply(seq_len(nrow(task_index)), function(i) {
    pair_i <- task_index$pair[[i]]
    stratum_spec <- stratum_specs[[task_index$stratum[[i]]]]
    outcome_name <- pairs$outcome[[pair_i]]
    predictor_name <- pairs$predictor[[pair_i]]
    outcome_spec <- registry[match(outcome_name, registry$name), , drop = FALSE]
    predictor_spec <- registry[match(predictor_name, registry$name), , drop = FALSE]
    matched_rule <- resolve_method_rule(rules, .data, outcome_name)
    rule_value <- if (is.null(matched_rule)) NULL else matched_rule$method
    selector <- if (inherits(rule_value, "method_selector")) rule_value else NULL
    method <- if (is.null(matched_rule)) default_method_spec(outcome_spec$type[[1]]) else if (is.null(selector)) rule_value else NULL
    method_policy <- if (is.null(matched_rule)) "system_default" else if (is.null(selector)) "user_rule" else "user_selector"
    same_variable <- identical(outcome_spec$var_id[[1]], predictor_spec$var_id[[1]])
    unsupported_predictor <- predictor_spec$type[[1]] %in% c(
      "unknown", "identifier", "date", "datetime"
    )
    needs_review <- outcome_spec$status[[1]] != "valid" ||
      predictor_spec$status[[1]] != "valid"

    status <- if ((is.null(method) && is.null(selector)) || unsupported_predictor || same_variable) {
      "invalid"
    } else if (needs_review) {
      "review"
    } else {
      "ready"
    }
    reason <- if (is.null(method) && is.null(selector)) {
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
      if (length(cluster_selection)) registry[match(names(cluster_selection), registry$name), , drop = FALSE] else NULL,
      method_policy, stratum_spec,
      registry[match(names(modifier_selection), registry$name), , drop = FALSE],
      selector
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
  cluster_spec = NULL,
  method_policy = "system_default",
  stratum_spec = empty_stratum_spec(),
  modifier_specs = NULL,
  selector = NULL
) {
  outcome <- outcome_spec$name[[1]]
  predictor <- predictor_spec$name[[1]]
  covariate_names <- if (is.null(covariate_specs)) character() else covariate_specs$name
  transformed_specs <- c(list(predictor_spec), if (is.null(covariate_specs)) list() else split(covariate_specs, seq_len(nrow(covariate_specs))))
  transformation_specs <- lapply(transformed_specs, function(x) x$transformation[[1]])
  names(transformation_specs) <- vapply(transformed_specs, function(x) x$var_id[[1]], character(1))
  modifier_names <- if (is.null(modifier_specs)) character() else modifier_specs$name
  weight_id <- if (is.null(weight_spec)) NA_character_ else weight_spec$var_id[[1]]
  weight_name <- if (is.null(weight_spec)) NA_character_ else weight_spec$name[[1]]
  weight_type <- if (is.null(weight_spec)) NA_character_ else weight_spec$weight_type[[1]]
  cluster_id <- if (is.null(cluster_spec)) NA_character_ else cluster_spec$var_id[[1]]
  cluster_name <- if (is.null(cluster_spec)) NA_character_ else cluster_spec$name[[1]]
  cluster_type <- if (is.null(cluster_spec)) NA_character_ else cluster_spec$cluster_type[[1]]
  if (is.null(variance)) variance <- if (!is.null(cluster_spec)) "cluster_robust" else if (identical(weight_type, "ipw")) "robust" else "model_based"
  if (is.null(method)) {
    candidate_method_ids <- if (is.null(selector)) character() else names(selector$candidates)
    method_id <- method_engine <- method_estimator <- method_ci <- NA_character_
    method_family <- method_link <- method_effect <- NA_character_
    method_reason <- reason
    method_function_id <- method_function_hash <- NA_character_
    method_packages <- if (is.null(selector)) "stats" else selector$required_packages
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
    method_function_id <- method$function_id
    method_function_hash <- method$function_hash
    method_packages <- method$required_packages
  }
  method_exponentiate <- if (is.null(method)) FALSE else method$exponentiate
  method_model_scale <- if (is.null(method)) NA_character_ else method$model_scale
  method_output_scale <- if (is.null(method)) NA_character_ else method$scale
  method_spec_object <- method
  tibble::tibble(
    analysis_id = paste0("analysis_", uuid::UUIDgenerate()),
    analysis_type = "univariable_regression",
    outcome_id = outcome_spec$var_id[[1]],
    predictor_id = predictor_spec$var_id[[1]],
    covariate_ids = list(if (is.null(covariate_specs)) character() else covariate_specs$var_id),
    effect_modifier_ids = list(if (is.null(modifier_specs)) character() else modifier_specs$var_id),
    weight_id = weight_id,
    cluster_id = cluster_id,
    stratum_ids = list(stratum_spec$var_ids),
    outcome = outcome,
    predictor = predictor,
    covariates = list(covariate_names),
    effect_modifiers = list(modifier_names),
    transformation_specs = list(transformation_specs),
    weight = weight_name,
    cluster = cluster_name,
    strata = list(stratum_spec$names),
    stratum_values = list(stratum_spec$values),
    stratum_label = stratum_spec$label,
    n_excluded_strata = stratum_spec$n_excluded,
    design = NA_character_,
    data_layout = "cross_sectional",
    reshape_spec = list(NULL),
    method_policy = method_policy,
    selector_id = if (is.null(selector)) NA_character_ else selector$id,
    selector_hash = if (is.null(selector)) NA_character_ else selector$function_hash,
    candidate_methods = list(candidate_method_ids),
    method = method_id,
    engine = method_engine,
    estimator = method_estimator,
    ci_method = method_ci,
    formula = list(new_analysis_formula(outcome, predictor, covariate_names, modifier_names)),
    family = method_family,
    link = method_link,
    effect_measure = method_effect,
    model_scale = method_model_scale,
    scale = method_output_scale,
    exponentiate = method_exponentiate,
    selection_reason = method_reason,
    selection_diagnostics = list(tibble::tibble()),
    function_id = method_function_id,
    function_hash = method_function_hash,
    required_packages = list(method_packages),
    method_object = list(method_spec_object),
    selector_object = list(selector),
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
    method <- linear_model()
    method$selection_reason <- "System default for a continuous outcome."
    return(method)
  }
  if (identical(outcome_type, "binary")) {
    method <- logistic_model()
    method$selection_reason <- "System default for a binary outcome."
    return(method)
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
    character(), NULL, NULL, NULL, NULL, "system_default", empty_stratum_spec(), NULL, NULL
  )
  new_analysis_plan(prototype[0, , drop = FALSE])
}

empty_stratum_spec <- function() {
  list(
    var_ids = character(), names = character(), values = list(),
    label = NA_character_, n_excluded = 0L
  )
}

compile_stratum_specs <- function(data, names, registry) {
  if (length(names) == 0L) return(list(empty_stratum_spec()))
  stratum_data <- tibble::as_tibble(data)[names]
  complete <- rep(TRUE, nrow(stratum_data))
  for (name in names) {
    complete <- complete & !special_missing_mask(data[[name]])
  }
  observed <- unique(stratum_data[complete, , drop = FALSE])
  if (nrow(observed) == 0L) return(list())
  lapply(seq_len(nrow(observed)), function(i) {
    values <- as.list(observed[i, , drop = FALSE])
    list(
      var_ids = registry$var_id[match(names, registry$name)],
      names = names,
      values = values,
      label = paste0(names, "=", vapply(values, as.character, character(1)),
        collapse = "; "),
      n_excluded = as.integer(sum(!complete))
    )
  })
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
    if (identical(out$analysis_type[[i]], "descriptive")) {
      out <- validate_descriptive_plan_task(out, i, data, registry)
      next
    }
    out$validated[[i]] <- TRUE
    outcome_row <- match(out$outcome_id[[i]], registry$var_id)
    predictor_row <- match(out$predictor_id[[i]], registry$var_id)
    issues <- character()
    missing_packages <- out$required_packages[[i]][
      !vapply(out$required_packages[[i]], requireNamespace, logical(1), quietly = TRUE)
    ]
    if (length(missing_packages)) {
      issues <- c(issues, paste0(
        "Missing required packages: ", paste(missing_packages, collapse = ", "), "."
      ))
    }
    comparison_registry <- contrasts(data)
    if (nrow(comparison_registry) > 0L) {
      comparison_rows <- comparison_registry$contrast_id %in% out$contrast_ids[[i]]
      conditional_rows <- comparison_rows &
        comparison_registry$comparison_type == "within_levels"
      missing_modifiers <- setdiff(
        comparison_registry$modifier_id[conditional_rows],
        out$effect_modifier_ids[[i]]
      )
      if (length(missing_modifiers)) {
        issues <- c(issues,
          "A `within_levels()` comparison references a variable that is not an effect modifier in this plan.")
      }
      comparison_packages <- unique(unlist(
        comparison_registry$required_packages[comparison_rows],
        use.names = FALSE
      ))
      missing_comparison_packages <- comparison_packages[
        !vapply(comparison_packages, requireNamespace, logical(1), quietly = TRUE)
      ]
      if (length(missing_comparison_packages)) {
        issues <- c(issues, paste0(
          "Missing packages required by comparisons: ",
          paste(missing_comparison_packages, collapse = ", "), "."
        ))
      }
    }

    if (is.na(outcome_row) || is.na(predictor_row)) {
      issues <- c(issues, "A variable referenced by stable id is absent from the data.")
      out$status[[i]] <- "invalid"
      out$reason[[i]] <- append_reasons(out$reason[[i]], issues)
      next
    }

    outcome_spec <- registry[outcome_row, , drop = FALSE]
    predictor_spec <- registry[predictor_row, , drop = FALSE]
    stratum_rows <- match(out$stratum_ids[[i]], registry$var_id)
    if (anyNA(stratum_rows)) {
      issues <- c(issues, "A stratification variable is absent from the data.")
      analysis_data <- data[0, , drop = FALSE]
    } else {
      current_strata_names <- registry$name[stratum_rows]
      out$strata[[i]] <- current_strata_names
      names(out$stratum_values[[i]]) <- current_strata_names
      analysis_data <- data[stratum_mask(data, current_strata_names,
        out$stratum_values[[i]]), , drop = FALSE]
    }
    covariate_rows <- match(out$covariate_ids[[i]], registry$var_id)
    if (anyNA(covariate_rows)) issues <- c(issues, "A covariate is absent from the data.")
    covariate_names <- registry$name[covariate_rows[!is.na(covariate_rows)]]
    modifier_rows <- match(out$effect_modifier_ids[[i]], registry$var_id)
    if (anyNA(modifier_rows)) issues <- c(issues, "An effect modifier is absent from the data.")
    modifier_names <- registry$name[modifier_rows[!is.na(modifier_rows)]]
    outcome_name <- outcome_spec$name[[1]]
    predictor_name <- predictor_spec$name[[1]]
    out$outcome[[i]] <- outcome_name
    out$predictor[[i]] <- predictor_name
    out$covariates[[i]] <- covariate_names
    out$effect_modifiers[[i]] <- modifier_names
    out$formula[[i]] <- new_analysis_formula(
      outcome_name, predictor_name, covariate_names, modifier_names
    )

    outcome <- analysis_data[[outcome_name]]
    predictor <- analysis_data[[predictor_name]]
    missing_outcome <- special_missing_mask(outcome)
    missing_predictor <- special_missing_mask(predictor)
    transformed_names <- c(predictor_name, covariate_names)
    transformed_ids <- c(out$predictor_id[[i]], out$covariate_ids[[i]])
    for (j in seq_along(transformed_names)) {
      transformation <- out$transformation_specs[[i]][[transformed_ids[[j]]]]
      if (!is.null(transformation)) {
        transformed <- tryCatch(
          apply_transformation_spec(
            analysis_vector(analysis_data[[transformed_names[[j]]]]),
            transformation, transformed_names[[j]], out$analysis_id[[i]]
          ),
          error = function(condition) condition
        )
        if (inherits(transformed, "error")) {
          issues <- c(issues, conditionMessage(transformed))
        } else if (n_distinct_values(transformed[!is.na(transformed)]) < 2L) {
          issues <- c(issues, paste0(
            "Variable `", transformed_names[[j]],
            "` has no variation after transformation."
          ))
        }
      }
    }
    analyzed <- !(missing_outcome | missing_predictor)
    for (name in covariate_names) analyzed <- analyzed & !special_missing_mask(analysis_data[[name]])
    for (name in modifier_names) analyzed <- analyzed & !special_missing_mask(analysis_data[[name]])
    if (!is.na(out$weight_id[[i]])) {
      weight_row <- match(out$weight_id[[i]], registry$var_id)
      if (is.na(weight_row)) {
        issues <- c(issues, "Configured weight is absent from the data.")
      } else {
        weight_name <- registry$name[[weight_row]]
        out$weight[[i]] <- weight_name
        weights <- analysis_data[[weight_name]]
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
        cluster_values <- analysis_data[[cluster_name]]
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
    n_total <- nrow(analysis_data)
    out$n_total[[i]] <- n_total
    out$n_eligible[[i]] <- n_total
    out$n_analyzed[[i]] <- sum(analyzed)
    out$n_missing_outcome[[i]] <- sum(missing_outcome)
    out$n_missing_predictor[[i]] <- sum(missing_predictor)

    outcome_values <- unclass_for_validation(outcome)[analyzed]
    predictor_values <- unclass_for_validation(predictor)[analyzed]
    for (modifier_name in modifier_names) {
      modifier_values <- unclass_for_validation(analysis_data[[modifier_name]])[analyzed]
      if (n_distinct_values(modifier_values) < 2L) {
        issues <- c(issues, paste0("Effect modifier `", modifier_name, "` has no variation."))
      }
      if (is.factor(predictor_values) || is.character(predictor_values) ||
          is.factor(modifier_values) || is.character(modifier_values)) {
        cells <- table(predictor_values, modifier_values)
        if (any(cells == 0L)) issues <- c(issues, paste0(
          "Interaction has empty predictor-by-modifier cells for `",
          modifier_name, "`."
        ))
      }
    }
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

    selector <- out$selector_object[[i]]
    if (length(issues) == 0L && !is.null(selector) && is.na(out$method[[i]])) {
      selection <- tryCatch({
        spec <- new_analysis_plan(out[i, , drop = FALSE])
        frame <- build_analysis_frame(spec, data)
        choice <- selector$select(build_analysis_context(spec, frame, data))
        validate_method_choice(choice, selector)
      }, error = function(condition) condition)
      if (inherits(selection, "error")) {
        issues <- c(issues, paste0(
          "Method selector `", selector$id, "` failed: ",
          conditionMessage(selection)
        ))
      } else {
        out <- apply_method_choice(out, i, selector, selection)
        selected_packages <- out$required_packages[[i]]
        missing_selected_packages <- selected_packages[
          !vapply(selected_packages, requireNamespace, logical(1), quietly = TRUE)
        ]
        if (length(missing_selected_packages)) {
          issues <- c(issues, paste0(
            "Missing packages required by selected method: ",
            paste(missing_selected_packages, collapse = ", "), "."
          ))
        }
      }
    }

    if (length(issues) > 0L) {
      out$status[[i]] <- "invalid"
      out$reason[[i]] <- append_reasons(out$reason[[i]], issues)
    }
  }

  new_analysis_plan(out)
}

validate_method_choice <- function(choice, selector) {
  if (!inherits(choice, "method_choice")) {
    stop_method_choice("Selector must return `method_choice()`.")
  }
  if (!choice$method %in% names(selector$candidates)) {
    stop_method_choice(paste0(
      "Method `", choice$method, "` is not an announced candidate."
    ))
  }
  if (!inherits(choice$diagnostics, "data.frame")) {
    stop_method_choice("Choice diagnostics must be a data frame.")
  }
  choice
}

apply_method_choice <- function(plan, i, selector, choice) {
  method <- selector$candidates[[choice$method]]
  plan$method[[i]] <- method$method
  plan$engine[[i]] <- method$engine
  plan$estimator[[i]] <- method$estimator
  plan$ci_method[[i]] <- method$ci_method
  plan$family[[i]] <- method$family
  plan$link[[i]] <- method$link
  plan$effect_measure[[i]] <- method$effect_measure
  plan$model_scale[[i]] <- method$model_scale
  plan$scale[[i]] <- method$scale
  plan$exponentiate[[i]] <- method$exponentiate
  plan$selection_reason[[i]] <- choice$reason
  plan$selection_diagnostics[[i]] <- tibble::as_tibble(choice$diagnostics)
  plan$function_id[[i]] <- method$function_id
  plan$function_hash[[i]] <- method$function_hash
  plan$required_packages[[i]] <- unique(c(
    selector$required_packages, method$required_packages
  ))
  plan$method_object[[i]] <- method
  plan
}

stratum_mask <- function(data, names, values) {
  if (length(names) == 0L) return(rep(TRUE, nrow(data)))
  mask <- rep(TRUE, nrow(data))
  for (i in seq_along(names)) {
    column <- data[[names[[i]]]]
    value <- values[[i]]
    mask <- mask & !is.na(column) & column == value
  }
  mask
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

new_analysis_formula <- function(outcome, predictor, covariates = character(),
                                 modifiers = character()) {
  rhs <- if (length(modifiers)) {
    rlang::call2("*", rlang::sym(predictor), rlang::sym(modifiers[[1]]))
  } else {
    rlang::sym(predictor)
  }
  if (length(modifiers) > 1L) {
    for (modifier in modifiers[-1L]) {
      rhs <- rlang::call2("+", rhs,
        rlang::call2("*", rlang::sym(predictor), rlang::sym(modifier)))
    }
  }
  for (covariate in covariates) rhs <- rlang::call2("+", rhs, rlang::sym(covariate))
  rlang::new_formula(
    lhs = rlang::sym(outcome),
    rhs = rhs,
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
