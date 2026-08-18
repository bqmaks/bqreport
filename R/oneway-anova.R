#' Declare a one-way ANOVA
#'
#' Creates an analytic function with its method, effect size and confidence
#' level fixed before analysis. The returned function is executed later by an
#' analysis plan with data and a compiled analysis context.
#'
#' The analytic function supplies only the omnibus test and, when requested,
#' an effect size. Its internal model is not returned and cannot be used to
#' extract within-group estimates or contrasts.
#'
#' @param var_equal Whether to use the classical equal-variance ANOVA. Welch's
#'   ANOVA is not yet available and `FALSE` is currently rejected.
#' @param effect_size Effect size to report. One of `"none"`,
#'   `"eta_squared"` and `"omega_squared"`.
#' @param conf_level Confidence level for interval estimates. Must be one
#'   finite number strictly between zero and one.
#'
#' @return A `bq_oneway_anova` analytic function.
#' @export
#' @examples
#' analysis <- oneway_anova(
#'   var_equal = TRUE,
#'   effect_size = "omega_squared",
#'   conf_level = 0.95
#' )
#' analysis
oneway_anova <- function(
  var_equal = TRUE,
  effect_size = "omega_squared",
  conf_level = 0.95
) {
  if (
    !is.logical(var_equal) || length(var_equal) != 1L || is.na(var_equal)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`var_equal` must be either TRUE or FALSE."
    )
  }

  if (!var_equal) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "Welch's one-way ANOVA is not yet available; set `var_equal = TRUE`."
      )
    )
  }

  allowed_effect_sizes <- c("none", "eta_squared", "omega_squared")
  if (
    !is.character(effect_size) || length(effect_size) != 1L ||
      is.na(effect_size) || !effect_size %in% allowed_effect_sizes
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`effect_size` must be one of \"none\", \"eta_squared\" and ",
        "\"omega_squared\"."
      )
    )
  }

  if (
    !is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`conf_level` must be one finite number strictly between zero and one."
    )
  }

  specification <- list(
    kind = "oneway_anova",
    var_equal = var_equal,
    effect_size = effect_size,
    conf_level = as.double(conf_level)
  )
  supplied_results <- "omnibus_test"
  suggested_dependencies <- character()
  if (effect_size != "none") {
    supplied_results <- c(supplied_results, "effect_size")
    suggested_dependencies <- "effectsize"
  }
  capabilities <- list(
    outcome_types = "continuous",
    outcomes_per_analysis = 1L,
    requires_group = TRUE,
    group_min_levels = 2L,
    group_max_levels = NA_integer_,
    max_strata = 0L,
    supports_covariates = FALSE,
    supports_weights = FALSE,
    supports_clusters = FALSE,
    supports_matched_sets = FALSE,
    provides_fits = FALSE,
    supplied_results = supplied_results,
    supplied_extractors = character(),
    suggested_dependencies = suggested_dependencies
  )

  analysis_function <- function(data, context) {
    if (
      !tibble::is_tibble(data) ||
        !identical(names(data), c(".row_id", ".outcome", ".group"))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`data` for `oneway_anova()` must be a tibble with columns ",
          "`.row_id`, `.outcome` and `.group`, in that order."
        )
      )
    }

    if (
      anyNA(data$.row_id) || anyDuplicated(data$.row_id) ||
        !is.atomic(data$.row_id) || !is.null(dim(data$.row_id))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`.row_id` must contain unique, non-missing atomic values."
      )
    }

    if (
      !is.numeric(data$.outcome) || is.object(data$.outcome) ||
        !is.null(dim(data$.outcome))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`.outcome` must be one plain numeric vector."
      )
    }

    if (!is.factor(data$.group) || anyNA(data$.group)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`.group` must be a factor without missing values."
      )
    }

    required_context <- c(
      "analysis_id", "test_id", "outcome_var_id", "group_var_id",
      "strata_var_id", "group_levels"
    )
    if (!is.list(context) || !identical(names(context), required_context)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`context` for `oneway_anova()` must contain `analysis_id`, ",
          "`test_id`, `outcome_var_id`, `group_var_id`, ",
          "`strata_var_id` and `group_levels`, in that order."
        )
      )
    }

    identifiers <- context[c(
      "analysis_id", "test_id", "outcome_var_id", "group_var_id"
    )]
    valid_identifiers <- vapply(
      identifiers,
      function(value) {
        is.character(value) && length(value) == 1L && !is.na(value) &&
          nzchar(value)
      },
      logical(1)
    )
    if (!all(valid_identifiers)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "Analysis, test and variable IDs must be non-empty character scalars."
      )
    }

    if (
      !is.character(context$strata_var_id) ||
        length(context$strata_var_id) != 1L ||
        !is.na(context$strata_var_id)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`strata_var_id` must be NA because `oneway_anova()` does not support strata."
      )
    }

    if (
      !tibble::is_tibble(context$group_levels) ||
        !identical(names(context$group_levels), c("var_id", "value", "position")) ||
        !is.character(context$group_levels$var_id) ||
        !is.character(context$group_levels$value) ||
        !is.integer(context$group_levels$position) ||
        anyNA(context$group_levels) ||
        !identical(
          context$group_levels$var_id,
          rep(context$group_var_id, nrow(context$group_levels))
        ) ||
        !identical(
          context$group_levels$position,
          seq_len(nrow(context$group_levels))
        ) ||
        !identical(context$group_levels$value, levels(data$.group))
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`group_levels` must describe every `.group` level once, in factor ",
          "order, for `group_var_id`."
        )
      )
    }

    if (nlevels(data$.group) < 2L) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`oneway_anova()` requires at least two declared group levels."
      )
    }

    if (specification$effect_size != "none") {
      bq_abort(
        "bq_error_analysis_not_implemented",
        "Effect sizes for `oneway_anova()` are not implemented yet."
      )
    }

    group_values <- levels(data$.group)
    missing_outcome <- is.na(data$.outcome)
    n_total <- vapply(
      group_values,
      function(value) sum(data$.group == value),
      integer(1)
    )
    n_missing <- vapply(
      group_values,
      function(value) sum(data$.group == value & missing_outcome),
      integer(1)
    )
    n_used <- n_total - n_missing

    if (any(n_used == 0L)) {
      group_value <- group_values[which(n_used == 0L)[1L]]
      bq_abort(
        "bq_error_invalid_analysis_input",
        sprintf(
          "Group level `%s` has no observed outcome values; provide data for every declared level.",
          group_value
        )
      )
    }

    if (sum(n_used) <= length(group_values)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "`oneway_anova()` requires positive residual degrees of freedom; ",
          "provide more observed outcome values."
        )
      )
    }

    model_data <- data.frame(
      .outcome = data$.outcome[!missing_outcome],
      .group = data$.group[!missing_outcome]
    )
    fit <- tryCatch(
      stats::lm(.outcome ~ .group, data = model_data),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`oneway_anova()` failed to fit its model: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )
    anova_table <- tryCatch(
      stats::anova(fit),
      error = function(error) {
        bq_abort(
          "bq_error_analysis_runtime",
          paste0("`oneway_anova()` failed to compute its F test: ", conditionMessage(error)),
          analysis_id = context$analysis_id
        )
      }
    )

    tests <- tibble::tibble(
      test_id = context$test_id,
      analysis_id = context$analysis_id,
      outcome_var_id = context$outcome_var_id,
      test = "oneway_anova",
      statistic = unname(as.double(anova_table[["F value"]][1L])),
      df1 = unname(as.double(anova_table[["Df"]][1L])),
      df2 = unname(as.double(anova_table[["Df"]][2L])),
      p_value = unname(as.double(anova_table[["Pr(>F)"]][1L]))
    )
    estimates <- tibble::tibble(
      estimate_id = character(),
      analysis_id = character(),
      outcome_var_id = character(),
      estimand = character(),
      estimate = double(),
      std_error = double(),
      conf_low = double(),
      conf_high = double()
    )
    sample_flow <- tibble::tibble(
      analysis_id = rep(context$analysis_id, length(group_values)),
      outcome_var_id = rep(context$outcome_var_id, length(group_values)),
      group_value = group_values,
      n_total = unname(n_total),
      n_missing = unname(n_missing),
      n_used = unname(n_used)
    )
    list(
      tests = tests,
      estimates = estimates,
      sample_flow = sample_flow
    )
  }

  structure(
    analysis_function,
    specification = specification,
    capabilities = capabilities,
    class = c("bq_oneway_anova", "bq_analysis_function", "function")
  )
}
