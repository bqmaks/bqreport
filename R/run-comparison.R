#' Run a comparison directly from vectors or a data frame
#'
#' Executes one terminal comparison specification without creating `bq_data`
#' or an analysis plan. The same analytic function and result schema are used
#' by the metadata-aware workflow; this function only prepares its data and
#' context inputs.
#'
#' When `data` is `NULL`, `outcome` and `group` are vectors. When `data` is a
#' data frame, they are single column names supplied as character strings.
#' Existing factor level order is preserved; otherwise group levels follow
#' their first appearance.
#'
#' @param analysis A comparison specification created by [t_test()],
#'   [mann_whitney_test()], [brunner_munzel_test()],
#'   [kruskal_wallis_test()] or [oneway_anova()].
#' @param outcome A numeric outcome vector, or its column name when `data` is
#'   supplied. Ordered factors are represented by their declared positions;
#'   other factor or character values must contain numeric representations.
#' @param group A group vector, or its column name when `data` is supplied.
#' @param data `NULL`, or an ordinary data frame or tibble.
#' @param reference Reference group value for two-group comparisons. It must
#'   be `NULL` for omnibus comparisons.
#'
#' @return A comparison result with `tests`, `estimates` and `sample_flow`
#'   tables, in the same schema returned by the analytic function.
#' @export
#' @examples
#' run_comparison(
#'   t_test(),
#'   outcome = c(8, 10, 12, 1, 2, 6),
#'   group = c("new", "new", "new", "control", "control", "control"),
#'   reference = "control"
#' )
#'
#' trial <- data.frame(
#'   response = c(8, 10, 12, 1, 2, 6),
#'   arm = rep(c("new", "control"), each = 3)
#' )
#' run_comparison(
#'   mann_whitney_test(), trial$response, trial$arm,
#'   reference = "control"
#' )
run_comparison <- function(
  analysis,
  outcome,
  group,
  data = NULL,
  reference = NULL
) {
  supported_kinds <- c(
    "t_test", "mann_whitney_test", "brunner_munzel_test",
    "kruskal_wallis_test", "oneway_anova"
  )
  specification <- attr(analysis, "specification")
  capabilities <- attr(analysis, "capabilities")
  if (
    !inherits(analysis, "bq_analysis_function") ||
      !is.list(specification) || !is.character(specification$kind) ||
      length(specification$kind) != 1L || is.na(specification$kind) ||
      !specification$kind %in% supported_kinds || !is.list(capabilities)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`analysis` must be a terminal comparison created by `t_test()`, ",
        "`mann_whitney_test()`, `brunner_munzel_test()`, ",
        "`kruskal_wallis_test()` or `oneway_anova()`."
      )
    )
  }
  if (missing(outcome) || missing(group)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`outcome` and `group` must both be supplied."
    )
  }

  outcome_id <- ".outcome"
  group_id <- ".group"
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`data` must be NULL or a data frame."
      )
    }
    if (anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`data` must have unique, non-empty column names."
      )
    }
    valid_column <- function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) &&
        nzchar(value) && value %in% names(data)
    }
    if (!valid_column(outcome) || !valid_column(group) || outcome == group) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        paste0(
          "With `data`, `outcome` and `group` must name two different ",
          "existing columns."
        )
      )
    }
    outcome_id <- outcome
    group_id <- group
    outcome <- data[[outcome_id]]
    group <- data[[group_id]]
  }

  if (length(outcome) == 0L || length(outcome) != length(group)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`outcome` and `group` must be non-empty vectors of equal length."
    )
  }
  numeric_outcome <- if (is.ordered(outcome)) {
    as.double(outcome)
  } else {
    as_continuous_model_vector(outcome)
  }
  if (is.null(numeric_outcome)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      paste0(
        "`outcome` must be numeric, an ordered factor, or values with finite ",
        "numeric representations."
      )
    )
  }
  if (!is.atomic(group) || !is.null(dim(group)) || anyNA(group)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`group` must be one atomic vector without missing values."
    )
  }

  group_values <- as.character(group)
  group_levels <- if (is.factor(group)) {
    as.character(levels(group))
  } else {
    unique(group_values)
  }
  group_factor <- factor(group_values, levels = group_levels)
  level_n <- nlevels(group_factor)
  if (
    level_n < capabilities$group_min_levels ||
      (!is.na(capabilities$group_max_levels) &&
        level_n > capabilities$group_max_levels)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      sprintf(
        "The selected comparison does not support %d declared group levels.",
        level_n
      )
    )
  }

  two_group <- !is.na(capabilities$group_max_levels) &&
    capabilities$group_max_levels == 2L
  if (two_group) {
    if (
      is.null(reference) || length(reference) != 1L || is.na(reference) ||
        !as.character(reference) %in% group_levels
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "`reference` must identify one declared group level."
      )
    }
    reference <- as.character(reference)
  } else if (!is.null(reference)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`reference` must be NULL for an omnibus comparison."
    )
  }

  engine_data <- tibble::tibble(
    .row_id = seq_along(numeric_outcome),
    .outcome = numeric_outcome,
    .group = group_factor
  )
  level_registry <- tibble::tibble(
    var_id = rep(group_id, level_n),
    value = group_levels,
    position = seq_len(level_n)
  )
  estimate_id <- if (
    !is.null(specification$effect_size) && specification$effect_size != "none"
  ) {
    "e001"
  } else {
    NA_character_
  }
  common_context <- list(
    analysis_id = "a001",
    test_id = "t001",
    outcome_var_id = outcome_id,
    group_var_id = group_id,
    strata_var_id = NA_character_
  )
  context <- switch(
    specification$kind,
    t_test = c(
      common_context[1:2],
      list(estimate_id = estimate_id),
      common_context[3:5],
      list(reference_value = reference, group_levels = level_registry)
    ),
    mann_whitney_test = c(
      common_context,
      list(reference_value = reference, group_levels = level_registry)
    ),
    brunner_munzel_test = c(
      common_context,
      list(reference_value = reference, group_levels = level_registry)
    ),
    kruskal_wallis_test = c(
      common_context[1:2],
      list(estimate_id = estimate_id),
      common_context[3:5],
      list(group_levels = level_registry)
    ),
    oneway_anova = c(
      common_context[1:2],
      list(estimate_id = estimate_id),
      common_context[3:5],
      list(group_levels = level_registry)
    )
  )

  analysis(engine_data, context)
}
