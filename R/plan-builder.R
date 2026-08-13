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
    list(data = .data, plan = NULL),
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

append_analysis_plan_block <- function(builder, block) {
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

check_analysis_plan_builder <- function(x) {
  if (!inherits(x, "analysis_plan_builder")) {
    stop_plan(
      "The first argument must be an analysis_plan_builder created by `analysis_plan()`.",
      "bq_error_invalid_plan_builder"
    )
  }
  invisible(x)
}
