# Result component prototypes, provenance, issues, and engine conditions.

survival_estimates_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), group = character(),
    group_level = character(), cause = character(), time = double(), n_risk = integer(),
    n_event = integer(), n_censor = integer(), estimate = double(),
    std_error = double(), conf_low = double(), conf_high = double(),
    quantile_probability = double(), restriction_time = double(),
    estimate_type = character(), scale = character(), time_unit = character(),
    method = character()
  )
}


provenance_row <- function(spec, declared_spec = spec, fallback_used = FALSE) {
  transformations <- spec$transformation_specs[[1]]
  transformations <- transformations[!vapply(transformations, is.null, logical(1))]
  model_terms <- if ("model_term_specs" %in% names(spec)) {
    spec$model_term_specs[[1]]
  } else {
    list()
  }
  model_terms <- model_terms[!vapply(model_terms, is.null, logical(1))]
  descriptive_functions <- if (
    "descriptive_functions" %in% names(spec)
  ) spec$descriptive_functions[[1]] else list()
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    method = declared_spec$method[[1]],
    engine = declared_spec$engine[[1]],
    selector_id = spec$selector_id[[1]],
    selector_hash = spec$selector_hash[[1]],
    candidate_methods = spec$candidate_methods,
    method_chain = if ("method_chain" %in% names(declared_spec)) {
      declared_spec$method_chain
    } else list(character()),
    fallback_conditions = if ("fallback_conditions" %in% names(declared_spec)) {
      declared_spec$fallback_conditions
    } else list(character()),
    executed_method = spec$method[[1]],
    fallback_used = fallback_used,
    selection_reason = declared_spec$selection_reason[[1]],
    selection_diagnostics = spec$selection_diagnostics,
    function_id = declared_spec$function_id[[1]],
    function_hash = declared_spec$function_hash[[1]],
    r_version = as.character(getRversion()),
    required_packages = declared_spec$required_packages,
    package_versions = list(c(stats = as.character(utils::packageVersion("stats")))),
    transformation_ids = list(vapply(transformations, `[[`, character(1), "id")),
    transformation_hashes = list(vapply(transformations, `[[`, character(1), "function_hash")),
    transformation_parameters = list(lapply(transformations, `[[`, "parameters")),
    model_term_ids = list(vapply(model_terms, `[[`, character(1), "id")),
    model_term_parameters = list(lapply(model_terms, `[[`, "resolved_parameters")),
    model_term_output_names = list(lapply(model_terms, `[[`, "output_names")),
    descriptive_function_ids = list(vapply(
      descriptive_functions, `[[`, character(1), "id"
    )),
    descriptive_function_hashes = list(vapply(
      descriptive_functions, `[[`, character(1), "function_hash"
    )),
    comparison_method = if (
      "comparison_method" %in% names(spec)
    ) spec$comparison_method[[1]] else NA_character_,
    comparison_estimand = if (
      "comparison_estimand" %in% names(spec)
    ) spec$comparison_estimand[[1]] else NA_character_,
    comparison_scale = if (
      "comparison_scale" %in% names(spec)
    ) spec$comparison_scale[[1]] else NA_character_,
    comparison_ci_method = if (
      "comparison_ci_method" %in% names(spec)
    ) spec$comparison_ci_method[[1]] else NA_character_,
    comparison_function_hash = if (
      "comparison_function_hash" %in% names(spec)
    ) spec$comparison_function_hash[[1]] else NA_character_,
    correlation_estimand = if (
      "estimand" %in% names(spec)
    ) spec$estimand[[1]] else NA_character_,
    correlation_adjustment_ids = if (
      "adjustment_ids" %in% names(spec)
    ) spec$adjustment_ids else list(character()),
    correlation_missing_policy = if (
      "missing_policy" %in% names(spec) &&
        identical(spec$analysis_type[[1]], "correlation")
    ) spec$missing_policy[[1]] else NA_character_,
    correlation_comparator_id = if (
      "correlation_comparator_id" %in% names(spec)
    ) spec$correlation_comparator_id[[1]] else NA_character_,
    correlation_comparator_hash = if (
      "correlation_comparator_hash" %in% names(spec)
    ) spec$correlation_comparator_hash[[1]] else NA_character_,
    bootstrap_replicates = if (
      "bootstrap_replicates" %in% names(spec)
    ) spec$bootstrap_replicates[[1]] else 0L,
    permutation_replicates = if (
      "permutation_replicates" %in% names(spec)
    ) spec$permutation_replicates[[1]] else 0L,
    resampling_seed = if (
      "resampling_seed" %in% names(spec)
    ) spec$resampling_seed[[1]] else NA_integer_,
    correlation_weight_id = if (
      "weight_id" %in% names(spec) && identical(spec$analysis_type[[1]], "correlation")
    ) spec$weight_id[[1]] else NA_character_,
    correlation_subject_id = if (
      "correlation_subject_id" %in% names(spec)
    ) spec$correlation_subject_id[[1]] else NA_character_
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
    stratum_label = character(),
    transformation_id = character(), transformation_label = character(),
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
    modifier = character(), modifier_level = character(),
    inner_contrast = character(), outer_contrast = character(),
    estimand = character(), exponentiated = logical(),
    estimate = double(), std_error = double(), std_error_scale = character(),
    conf_low = double(), conf_high = double(),
    p_value = double(), p_adjusted = double(), adjust_method = character(),
    effect_measure = character(), scale = character()
  )
}

tests_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
    contrast = character(), numerator = character(), denominator = character(),
    test = character(), statistic = double(), df = double(), p_value = double(),
    p_adjusted = double(), adjust_method = character(),
    method = character()
  )
}

diagnostics_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), metric = character(), value = double(),
    status = character(), message = character()
  )
}

omnibus_effects_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), outcome = character(), predictor = character(),
    estimate = double(), std_error = double(), std_error_scale = character(),
    conf_low = double(), conf_high = double(), confidence_level = double(),
    effect_measure = character(), scale = character(), n = integer(),
    n_groups = integer(), method = character(), ci_method = character()
  )
}

issues_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), stage = character(), severity = character(),
    condition_class = character(), message = character()
  )
}

attempts_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), chain_id = character(), attempt = integer(),
    member = character(), method = character(), status = character(),
    condition_class = character(), message = character()
  )
}

provenance_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), method = character(), engine = character(),
    selector_id = character(), selector_hash = character(),
    candidate_methods = list(), selection_reason = character(),
    method_chain = list(), fallback_conditions = list(),
    executed_method = character(), fallback_used = logical(),
    selection_diagnostics = list(),
    function_id = character(), function_hash = character(), r_version = character(),
    required_packages = list(), package_versions = list()
    , transformation_ids = list(), transformation_hashes = list(),
    transformation_parameters = list(), model_term_ids = list(),
    model_term_parameters = list(), model_term_output_names = list(),
    descriptive_function_ids = list(),
    descriptive_function_hashes = list(), comparison_method = character(),
    comparison_estimand = character(), comparison_scale = character(),
    comparison_ci_method = character(), comparison_function_hash = character()
    , correlation_estimand = character(), correlation_adjustment_ids = list(),
    correlation_missing_policy = character(), correlation_comparator_id = character(),
    correlation_comparator_hash = character(), bootstrap_replicates = integer(),
    permutation_replicates = integer(), resampling_seed = integer(),
    correlation_weight_id = character(), correlation_subject_id = character()
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
