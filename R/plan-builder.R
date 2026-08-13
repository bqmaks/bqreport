#' Build an analysis plan incrementally
#'
#' `analysis_plan()` starts an accumulator tied to one `bq_data` object.
#' Analysis blocks can then be appended with [add_analysis()],
#' [add_descriptives()], and [add_correlations()]. [build_plan()] returns the
#' accumulated plan without preflight, while calling [validate_plan()] on the
#' accumulator returns the ordinary validated `analysis_plan` tibble.
#'
#' The builder retains the source data while tasks are being compiled. The
#' ordinary plan returned by [validate_plan()] remains separate from the data
#' and refers to variables through stable identifiers.
#'
#' @param .data A `bq_data` object.
#'
#' @return An `analysis_plan_builder`.
#' @examples
#' data <- as_bq_data(tibble::tibble(
#'   y = c(1, 2, 3, 4),
#'   response = c(0, 1, 0, 1),
#'   treatment = factor(c("A", "A", "B", "B"))
#' )) |>
#'   set_outcome(y, type = "continuous") |>
#'   set_outcome(response, type = "binary", event = 1) |>
#'   set_predictor(treatment, type = "binary", reference = "A")
#' plan <- data |>
#'   analysis_plan() |>
#'   add_analysis(y, treatment) |>
#'   add_analysis(response, treatment) |>
#'   validate_plan()
#' @export
analysis_plan <- function(.data) {
  check_bq_data(.data)
  structure(
    list(
      data = .data,
      plan = NULL,
      data_signature = analysis_schema_signature(.data)
    ),
    class = "analysis_plan_builder"
  )
}

#' Add regression tasks to an incremental plan
#'
#' Arguments have the same meaning as in [plan_analysis()].
#'
#' @param .builder An `analysis_plan_builder`.
#' @inheritParams plan_analysis
#'
#' @return The updated `analysis_plan_builder`.
#' @export
add_analysis <- function(
  .builder,
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
  check_analysis_plan_builder(.builder)
  outcomes <- rlang::enquo(outcomes)
  predictors <- rlang::enquo(predictors)
  covariates <- rlang::enquo(covariates)
  weights <- rlang::enquo(weights)
  cluster <- rlang::enquo(cluster)
  strata <- rlang::enquo(strata)
  effect_modifiers <- rlang::enquo(effect_modifiers)
  block <- rlang::inject(plan_analysis(
    .builder$data,
    outcomes = !!outcomes,
    predictors = !!predictors,
    covariates = !!covariates,
    weights = !!weights,
    cluster = !!cluster,
    strata = !!strata,
    effect_modifiers = !!effect_modifiers,
    variance = variance,
    rules = rules,
    confidence_level = confidence_level
  ))
  append_analysis_plan_block(.builder, block)
}

#' Add descriptive tasks to an incremental plan
#'
#' Arguments have the same meaning as in [plan_descriptives()].
#'
#' @param .builder An `analysis_plan_builder`.
#' @inheritParams plan_descriptives
#'
#' @return The updated `analysis_plan_builder`.
#' @export
add_descriptives <- function(
  .builder,
  variables = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  overall = TRUE,
  confidence_level = 0.95,
  functions = list(),
  comparisons = FALSE,
  contrasts = NULL,
  adjust = "none"
) {
  check_analysis_plan_builder(.builder)
  variables <- rlang::enquo(variables)
  groups <- rlang::enquo(groups)
  block <- rlang::inject(plan_descriptives(
    .builder$data,
    variables = !!variables,
    groups = !!groups,
    overall = overall,
    confidence_level = confidence_level,
    functions = functions,
    comparisons = comparisons,
    contrasts = contrasts,
    adjust = adjust
  ))
  append_analysis_plan_block(.builder, block)
}

#' Add correlation tasks to an incremental plan
#'
#' Arguments have the same meaning as in [plan_correlations()].
#'
#' @param .builder An `analysis_plan_builder`.
#' @inheritParams plan_correlations
#'
#' @return The updated `analysis_plan_builder`.
#' @export
add_correlations <- function(
  .builder,
  variables = where_continuous(),
  with = NULL,
  adjust_for = tidyselect::any_of(character()),
  strata = tidyselect::any_of(character()),
  weights = tidyselect::any_of(character()),
  id = tidyselect::any_of(character()),
  interaction_test = FALSE,
  comparator = NULL,
  method = pearson_correlation(),
  missing = c("pairwise", "complete"),
  confidence_level = 0.95,
  adjust = "none"
) {
  check_analysis_plan_builder(.builder)
  variables <- rlang::enquo(variables)
  with <- rlang::enquo(with)
  adjust_for <- rlang::enquo(adjust_for)
  strata <- rlang::enquo(strata)
  weights <- rlang::enquo(weights)
  id <- rlang::enquo(id)
  block <- rlang::inject(plan_correlations(
    .builder$data,
    variables = !!variables,
    with = !!with,
    adjust_for = !!adjust_for,
    strata = !!strata,
    weights = !!weights,
    id = !!id,
    interaction_test = interaction_test,
    comparator = comparator,
    method = method,
    missing = missing,
    confidence_level = confidence_level,
    adjust = adjust
  ))
  append_analysis_plan_block(.builder, block)
}

#' Add Cox survival-regression tasks to an incremental plan
#'
#' Arguments have the same meaning as in [plan_survival()].
#'
#' @param .builder An `analysis_plan_builder`.
#' @inheritParams plan_survival
#'
#' @return The updated `analysis_plan_builder`.
#' @export
add_survival <- function(
  .builder,
  outcomes = tidyselect::everything(),
  predictors = all_predictors(),
  covariates = tidyselect::any_of(character()),
  effect_modifiers = tidyselect::any_of(character()),
  confidence_level = 0.95,
  method = cox_model()
) {
  check_analysis_plan_builder(.builder)
  outcomes <- rlang::enquo(outcomes)
  predictors <- rlang::enquo(predictors)
  covariates <- rlang::enquo(covariates)
  effect_modifiers <- rlang::enquo(effect_modifiers)
  block <- rlang::inject(plan_survival(
    .builder$data,
    outcomes = !!outcomes,
    predictors = !!predictors,
    covariates = !!covariates,
    effect_modifiers = !!effect_modifiers,
    confidence_level = confidence_level,
    method = method
  ))
  append_analysis_plan_block(.builder, block)
}

#' Add Kaplan--Meier tasks to an incremental plan
#'
#' Arguments have the same meaning as in [plan_kaplan_meier()].
#'
#' @param .builder An `analysis_plan_builder`.
#' @inheritParams plan_kaplan_meier
#'
#' @return The updated `analysis_plan_builder`.
#' @export
add_kaplan_meier <- function(
  .builder,
  outcomes = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  times = NULL,
  confidence_level = 0.95,
  quantiles = NULL,
  rmst_tau = NULL,
  estimates = "survival",
  comparisons = NULL,
  adjust = "none"
) {
  check_analysis_plan_builder(.builder)
  outcomes <- rlang::enquo(outcomes)
  groups <- rlang::enquo(groups)
  block <- rlang::inject(plan_kaplan_meier(
    .builder$data,
    outcomes = !!outcomes,
    groups = !!groups,
    times = times,
    confidence_level = confidence_level,
    quantiles = quantiles,
    rmst_tau = rmst_tau,
    estimates = estimates,
    comparisons = comparisons,
    adjust = adjust
  ))
  append_analysis_plan_block(.builder, block)
}

#' Add cumulative-incidence tasks to an incremental plan
#'
#' Arguments have the same meaning as in [plan_cumulative_incidence()].
#'
#' @param .builder An `analysis_plan_builder`.
#' @inheritParams plan_cumulative_incidence
#'
#' @return The updated `analysis_plan_builder`.
#' @export
add_cumulative_incidence <- function(
  .builder,
  outcomes = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  times = NULL,
  confidence_level = 0.95
) {
  check_analysis_plan_builder(.builder)
  outcomes <- rlang::enquo(outcomes)
  groups <- rlang::enquo(groups)
  block <- rlang::inject(plan_cumulative_incidence(
    .builder$data,
    outcomes = !!outcomes,
    groups = !!groups,
    times = times,
    confidence_level = confidence_level
  ))
  append_analysis_plan_block(.builder, block)
}

#' Finish compiling an incremental analysis plan
#'
#' `build_plan()` exposes the accumulated, unvalidated plan for inspection or
#' editing. Use [validate_plan()] afterwards to run preflight checks. For a
#' shorter pipeline, an `analysis_plan_builder` can be passed directly to
#' [validate_plan()].
#'
#' @param .builder An `analysis_plan_builder`.
#'
#' @return An unvalidated `analysis_plan` tibble.
#' @export
build_plan <- function(.builder) {
  check_analysis_plan_builder(.builder)
  if (is.null(.builder$plan)) {
    stop_plan(
      "The analysis_plan_builder contains no analysis tasks.",
      "bq_error_empty_plan_builder"
    )
  }
  .builder$plan
}

#' Combine compatible analysis plans
#'
#' Plans must originate from the same analytical schema, have the same
#' validation state, and contain distinct deterministic `analysis_id` values.
#'
#' @param ... Two or more `analysis_plan` objects produced by [build_plan()].
#'
#' @return A combined `analysis_plan` tibble.
#' @export
combine_plans <- function(...) {
  plans <- list(...)
  if (length(plans) < 2L || any(!vapply(plans, inherits, logical(1), "analysis_plan"))) {
    stop_plan(
      "`combine_plans()` requires at least two analysis_plan objects.",
      "bq_error_invalid_plan"
    )
  }
  signatures <- lapply(plans, function(plan) unique(plan$data_signature))
  if (any(lengths(signatures) != 1L) || anyNA(unlist(signatures)) ||
      length(unique(unlist(signatures))) != 1L) {
    stop_plan(
      "Analysis plans were compiled from incompatible analytical schemas.",
      "bq_error_incompatible_plans"
    )
  }
  validation_states <- lapply(plans, function(plan) unique(plan$validated))
  if (any(lengths(validation_states) != 1L) ||
      length(unique(unlist(validation_states))) != 1L) {
    stop_plan(
      "Cannot combine validated and unvalidated analysis plans.",
      "bq_error_incompatible_plan_state"
    )
  }
  ids <- unlist(lapply(plans, `[[`, "analysis_id"), use.names = FALSE)
  duplicates <- unique(ids[duplicated(ids)])
  if (length(duplicates)) {
    stop_plan(
      paste0("Cannot combine duplicate analysis tasks: ",
        paste(duplicates, collapse = ", "), "."),
      "bq_error_duplicate_analysis"
    )
  }
  new_analysis_plan(vctrs::vec_rbind(!!!plans))
}

append_analysis_plan_block <- function(builder, block) {
  block$data_signature <- builder$data_signature
  if (is.null(builder$plan)) {
    builder$plan <- block
    return(builder)
  }
  duplicate_ids <- intersect(builder$plan$analysis_id, block$analysis_id)
  if (length(duplicate_ids)) {
    stop_plan(
      paste0(
        "Cannot add duplicate analysis task", if (length(duplicate_ids) > 1L) "s" else "",
        ": ", paste(duplicate_ids, collapse = ", "), "."
      ),
      "bq_error_duplicate_analysis"
    )
  }
  builder$plan <- new_analysis_plan(vctrs::vec_rbind(builder$plan, block))
  builder
}

analysis_schema_signature <- function(data) {
  registries <- list(
    variables = variables(data),
    outcomes = outcomes(data),
    designs = designs(data),
    contrasts = contrasts(data)
  )
  normalized <- lapply(registries, normalize_signature_registry)
  bq_id("data_schema", normalized)
}

normalize_signature_registry <- function(registry) {
  registry <- tibble::as_tibble(registry)
  display_columns <- intersect(
    names(registry),
    c("name", "label", "unit", "time", "event", "group", "id",
      "variable", "outcome", "predictor")
  )
  registry <- registry[setdiff(names(registry), display_columns)]
  id_columns <- intersect(
    c("var_id", "outcome_id", "design_id", "contrast_id"), names(registry)
  )
  if (nrow(registry) > 1L && length(id_columns)) {
    order_args <- lapply(id_columns, function(name) registry[[name]])
    registry <- registry[do.call(order, order_args), , drop = FALSE]
  }
  unclass(registry)
}

check_analysis_plan_builder <- function(x) {
  if (!inherits(x, "analysis_plan_builder")) {
    stop_plan(
      "The first argument must be an analysis_plan_builder created by `analysis_plan()`.",
      "bq_error_invalid_plan_builder"
    )
  }
  invisible(x)
}
