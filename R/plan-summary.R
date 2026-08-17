#' Plan a summary analysis
#'
#' Fixes the variables and design axes of a summary analysis before any
#' computation. `group` defines levels that may later be compared, while
#' `strata` defines contexts in which the analysis is repeated. `overall`
#' names axes that should also be collapsed into raw, pooled summaries.
#'
#' The plan stores stable variable identifiers and the `bq_data` object they
#' belong to. It does not infer types, choose estimands, validate analytic
#' methods or compute results.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more variables to
#'   summarise.
#' @param group `NULL`, or a tidyselect expression selecting exactly one group
#'   variable.
#' @param strata `NULL`, or a tidyselect expression selecting one or more
#'   stratification variables.
#' @param overall Character vector containing `"group"`, `"strata"`, both, or
#'   neither. Overall summaries always pool the underlying observations and
#'   are never model-based.
#'
#' @return A `bq_plan_summary` object.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(
#'   age = c(40, 55, 61),
#'   treatment = c("A", "B", "A"),
#'   centre = c("X", "X", "Y")
#' ))
#' plan_summary(
#'   data,
#'   variables = age,
#'   group = treatment,
#'   strata = centre,
#'   overall = c("group", "strata")
#' )
plan_summary <- function(
  data,
  variables,
  group = NULL,
  strata = NULL,
  overall = character()
) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  allowed_overall <- c("group", "strata")

  if (
    !is.character(overall) || anyNA(overall) ||
      length(setdiff(overall, allowed_overall)) > 0L || anyDuplicated(overall)
  ) {
    bq_abort(
      "bq_error_invalid_plan",
      "`overall` must contain each of \"group\" and \"strata\" at most once."
    )
  }

  selected_variables <- resolve_variables(
    data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )
  selected_group <- resolve_variables(
    data,
    rlang::enquo(group),
    argument = "group",
    min = 0L,
    max = 1L
  )
  selected_strata <- resolve_variables(
    data,
    rlang::enquo(strata),
    argument = "strata",
    min = 0L
  )
  registry <- attr(data, "variables")

  if ("group" %in% overall && nrow(selected_group) == 0L) {
    bq_abort(
      "bq_error_invalid_plan",
      "`overall = \"group\"` requires a group variable."
    )
  }

  if ("strata" %in% overall && nrow(selected_strata) == 0L) {
    bq_abort(
      "bq_error_invalid_plan",
      "`overall = \"strata\"` requires at least one strata variable."
    )
  }

  group_strata_overlap <- intersect(selected_group$var_id, selected_strata$var_id)

  if (length(group_strata_overlap) > 0L) {
    overlap_name <- registry$name[
      match(group_strata_overlap[1L], registry$var_id)
    ]
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("Variable `%s` cannot be both `group` and `strata`.", overlap_name)
    )
  }

  design_ids <- c(selected_group$var_id, selected_strata$var_id)
  variable_design_overlap <- intersect(selected_variables$var_id, design_ids)

  if (length(variable_design_overlap) > 0L) {
    overlap_name <- registry$name[
      match(variable_design_overlap[1L], registry$var_id)
    ]
    bq_abort(
      "bq_error_invalid_plan",
      sprintf(
        "Variable `%s` cannot be both summarised and used as a design axis.",
        overlap_name
      )
    )
  }

  overall <- allowed_overall[allowed_overall %in% overall]

  structure(
    list(
      analysis = "summary",
      data = data,
      variables = selected_variables$var_id,
      group = selected_group$var_id,
      strata = selected_strata$var_id,
      overall = overall,
      statistics = tibble::tibble(
        statistic_id = character(),
        name = character(),
        kind = character(),
        source = character(),
        missing = character()
      ),
      statistic_components = tibble::tibble(
        statistic_id = character(),
        component = character(),
        type = character(),
        scale = character(),
        rounding = character(),
        digits = integer(),
        position = integer()
      ),
      statistic_assignments = tibble::tibble(
        statistic_id = character(),
        var_id = character()
      ),
      statistic_functions = list(),
      next_statistic_number = 1L,
      display_rules = tibble::tibble(
        rule_id = character(),
        kind = character(),
        max_n = integer(),
        display_statistics = logical()
      ),
      display_rule_assignments = tibble::tibble(
        rule_id = character(),
        var_id = character()
      ),
      next_display_rule_number = 1L
    ),
    class = c("bq_plan_summary", "bq_plan")
  )
}

#' Print a summary analysis plan
#'
#' Shows the fixed variable selections, raw Overall axes and registered
#' statistics without printing the data or executable functions stored inside
#' the plan.
#'
#' @param x A `bq_plan_summary` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
print.bq_plan_summary <- function(x, ...) {
  registry <- attr(x$data, "variables")
  variable_names <- registry$name[match(x$variables, registry$var_id)]
  group_names <- registry$name[match(x$group, registry$var_id)]
  strata_names <- registry$name[match(x$strata, registry$var_id)]

  cat(
    "<bq summary plan>\n",
    "Variables: ", paste(variable_names, collapse = ", "), "\n",
    "Group: ", if (length(group_names) == 0L) "none" else group_names, "\n",
    "Strata: ", if (length(strata_names) == 0L) "none" else paste(strata_names, collapse = ", "), "\n",
    "Overall: ", if (length(x$overall) == 0L) "none" else paste(x$overall, collapse = ", "), "\n",
    sep = ""
  )

  if (nrow(x$statistics) == 0L) {
    cat("Statistics: none\n")
  } else {
    cat("Statistics:\n")

    for (statistic_id in x$statistics$statistic_id) {
      statistic_name <- x$statistics$name[
        match(statistic_id, x$statistics$statistic_id)
      ]
      component_names <- x$statistic_components$component[
        x$statistic_components$statistic_id == statistic_id
      ]
      assigned_ids <- x$statistic_assignments$var_id[
        x$statistic_assignments$statistic_id == statistic_id
      ]
      assigned_names <- registry$name[match(assigned_ids, registry$var_id)]

      cat(
        "  ", statistic_name,
        ": ", paste(component_names, collapse = ", "),
        " -> ", paste(assigned_names, collapse = ", "),
        "\n",
        sep = ""
      )
    }
  }

  invisible(x)
}
