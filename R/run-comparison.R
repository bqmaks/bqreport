#' Run a comparison directly from vectors or a data frame
#'
#' Executes one terminal comparison specification without creating `bq_data`
#' or an analysis plan. The same analytic function and result schema are used
#' by the metadata-aware workflow; this function only prepares its data and
#' context inputs.
#'
#' When `data` is `NULL`, `outcome` and `group` are vectors. When `data` is a
#' data frame, they select one column each with tidyselect syntax: a bare
#' column name or a character string. Existing factor level order is
#' preserved; other group vectors are ordered by `sort()`, so character values
#' are lexicographic and numeric values ascending.
#'
#' @param analysis A comparison specification such as [t_test()],
#'   [oneway_anova()], [t_family()] or [tukey_test()]: any function created by
#'   the comparison constructors of this package.
#' @param outcome A numeric outcome vector, or a column selection when `data`
#'   is supplied. Ordered factors are represented by their declared positions;
#'   other factor or character values must contain numeric representations.
#' @param group A group vector, or a column selection when `data` is supplied.
#' @param data `NULL`, or an ordinary data frame or tibble.
#' @param reference Reference group value for two-group terminal tests. It
#'   must be `NULL` when the selected analysis declares its comparison family
#'   internally.
#'
#' @return A `bq_result_comparison` object: a list holding the executed
#'   `specification` and the provider's result tables. Two-group and
#'   omnibus tests supply `tests`, `estimates` and `sample_flow`.
#'   Comparison-family providers are separate analytic entities and supply
#'   only `comparisons` and `sample_flow`.
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
#'   mann_whitney_test(), response, arm, data = trial,
#'   reference = "control"
#' )
run_comparison <- function(
  analysis,
  outcome,
  group,
  data = NULL,
  reference = NULL
) {
  specification <- attr(analysis, "specification")
  capabilities <- attr(analysis, "capabilities")
  if (
    !inherits(analysis, "bq_analysis_function") ||
      !is.list(specification) || !is.character(specification$kind) ||
      length(specification$kind) != 1L || is.na(specification$kind) ||
      !is.list(capabilities) ||
      !all(
        c(
          "outcome_types", "group_min_levels", "group_max_levels",
          "supplied_results"
        ) %in% names(capabilities)
      )
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`analysis` must be a comparison specification created by one of ",
        "the comparison constructors, such as `t_test()` or `t_family()`."
      )
    )
  }
  input_error <- function(...) {
    bq_abort("bq_error_invalid_analysis_input", paste0(...))
  }
  if (missing(outcome) || missing(group)) {
    input_error("`outcome` and `group` must both be supplied.")
  }

  outcome_id <- ".outcome"
  group_id <- ".group"
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      input_error("`data` must be NULL or a data frame.")
    }
    if (
      anyNA(names(data)) || any(!nzchar(names(data))) ||
        anyDuplicated(names(data))
    ) {
      input_error("`data` must have unique, non-empty column names.")
    }
    select_one <- function(selection, argument) {
      selected <- tryCatch(
        tidyselect::eval_select(selection, data),
        error = function(error) {
          input_error(
            "Cannot select `", argument, "` in `data`: ",
            conditionMessage(error)
          )
        }
      )
      if (length(selected) != 1L) {
        input_error(
          "`", argument, "` must select exactly one column of `data`, not ",
          length(selected), "."
        )
      }
      names(data)[selected]
    }
    outcome_id <- select_one(rlang::enquo(outcome), "outcome")
    group_id <- select_one(rlang::enquo(group), "group")
    if (outcome_id == group_id) {
      input_error("`outcome` and `group` must select two different columns.")
    }
    outcome <- data[[outcome_id]]
    group <- data[[group_id]]
  }

  if (length(outcome) == 0L || length(outcome) != length(group)) {
    input_error(
      "`outcome` and `group` must be non-empty vectors of equal length."
    )
  }
  if (is.ordered(outcome) && !"ordinal" %in% capabilities$outcome_types) {
    input_error(
      "`", specification$kind, "()` does not accept an ordinal outcome; ",
      "supply numeric values or choose a rank-based comparison."
    )
  }
  numeric_outcome <- if (is.ordered(outcome)) {
    as.double(outcome)
  } else {
    as_continuous_model_vector(outcome)
  }
  if (is.null(numeric_outcome)) {
    input_error(
      "`outcome` must be numeric, an ordered factor, or values with finite ",
      "numeric representations."
    )
  }
  if (!is.atomic(group) || !is.null(dim(group)) || anyNA(group)) {
    input_error("`group` must be one atomic vector without missing values.")
  }

  # sort() keeps numeric groups in numeric order and character groups in
  # C-locale order, so the declared level order does not depend on the order
  # of appearance or on the session locale.
  group_levels <- if (is.factor(group)) {
    as.character(levels(group))
  } else {
    as.character(sort(unique(group), method = "radix"))
  }
  group_factor <- factor(as.character(group), levels = group_levels)
  level_n <- nlevels(group_factor)
  if (
    level_n < capabilities$group_min_levels ||
      (!is.na(capabilities$group_max_levels) &&
        level_n > capabilities$group_max_levels)
  ) {
    input_error(
      "The selected comparison does not support ", level_n,
      " declared group levels."
    )
  }

  two_group <- !is.na(capabilities$group_max_levels) &&
    capabilities$group_max_levels == 2L
  if (two_group) {
    if (
      is.null(reference) || length(reference) != 1L || is.na(reference) ||
        !as.character(reference) %in% group_levels
    ) {
      input_error("`reference` must identify one declared group level.")
    }
    reference <- as.character(reference)
  } else if (!is.null(reference)) {
    input_error(
      "`reference` must be NULL when the selected analysis declares its ",
      "comparison family internally."
    )
  }

  engine_data <- tibble::tibble(
    .row_id = seq_along(numeric_outcome),
    .outcome = numeric_outcome,
    .group = group_factor
  )
  # A separate estimate identifier exists only when the provider reports an
  # effect size in its own `estimates` table; family providers keep effects
  # inside `comparisons` and never use it.
  estimate_id <- if ("effect_size" %in% capabilities$supplied_results) {
    "e001"
  } else {
    NA_character_
  }
  context <- list(
    analysis_id = "a001",
    test_id = "t001",
    estimate_id = estimate_id,
    outcome_var_id = outcome_id,
    group_var_id = group_id,
    strata_var_id = NA_character_,
    group_levels = tibble::tibble(
      var_id = rep(group_id, level_n),
      value = group_levels,
      position = seq_len(level_n)
    )
  )
  if (two_group) {
    context$reference_value <- reference
  }

  result <- analysis(engine_data, context)
  structure(
    c(list(analysis = "comparison", specification = specification), result),
    class = c("bq_result_comparison", "bq_result")
  )
}

#' Print a comparison result
#'
#' @param x A `bq_result_comparison` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
print.bq_result_comparison <- function(x, ...) {
  cat("<bq result: comparison>\n")
  cat("Specification: ", x$specification$kind, "\n", sep = "")
  for (name in setdiff(names(x), c("analysis", "specification"))) {
    rows <- nrow(x[[name]])
    cat(name, ": ", rows, " row", if (rows == 1L) "" else "s", "\n", sep = "")
  }
  invisible(x)
}
