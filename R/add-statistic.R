#' Add a statistic to a summary plan
#'
#' Registers one statistic specification and assigns it to selected summary
#' variables. Declarative metadata, output components and assignments stay in
#' flat tables; an executable custom function is stored separately under the
#' same stable `statistic_id`.
#'
#' @param plan A `bq_plan_summary` object.
#' @param variables A tidyselect expression selecting one or more variables
#'   already included in `plan$variables`.
#' @param statistic A `bq_statistic` specification such as one created by
#'   [continuous_statistic()].
#'
#' @return A copy of `plan` with the statistic registered and assigned.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA), bmi = c(22, 31, 27)))
#' plan <- plan_summary(data, c(age, bmi))
#' robust <- continuous_statistic(
#'   "robust",
#'   function(x) {
#'     data.frame(
#'       median = if (length(x) == 0L) NA_real_ else stats::median(x, na.rm = TRUE)
#'     )
#'   }
#' )
#' add_statistic(plan, c(age, bmi), robust)
add_statistic <- function(plan, variables, statistic) {
  if (!inherits(plan, "bq_plan_summary")) {
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("`plan` must be a bq_plan_summary object, not %s.", class(plan)[1L])
    )
  }

  expected_fields <- c(
    "kind", "name", "components", "component_types", "source", "missing", "fun"
  )
  valid_statistic <- inherits(statistic, "bq_statistic") &&
    identical(names(statistic), expected_fields) &&
    is.character(statistic$kind) && length(statistic$kind) == 1L &&
    is.character(statistic$name) && length(statistic$name) == 1L &&
    is.character(statistic$components) && length(statistic$components) > 0L &&
    is.character(statistic$component_types) &&
    length(statistic$component_types) == length(statistic$components) &&
    is.character(statistic$source) && length(statistic$source) == 1L &&
    is.character(statistic$missing) && length(statistic$missing) == 1L &&
    is.function(statistic$fun)

  if (!valid_statistic) {
    bq_abort(
      "bq_error_invalid_statistic",
      "`statistic` must be a specification created by a statistic constructor."
    )
  }

  selection <- resolve_variables(
    plan$data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )
  outside_plan <- setdiff(selection$var_id, plan$variables)

  if (length(outside_plan) > 0L) {
    variable_name <- selection$name[match(outside_plan[1L], selection$var_id)]
    bq_abort(
      "bq_error_invalid_plan",
      sprintf(
        "Variable `%s` is not a summary variable in `plan`; select only planned variables.",
        variable_name
      )
    )
  }

  if (statistic$name %in% plan$statistics$name) {
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("Statistic name `%s` is already used in `plan`.", statistic$name)
    )
  }

  statistic_id <- sprintf("s%03d", plan$next_statistic_number)

  plan$statistics <- dplyr::bind_rows(
    plan$statistics,
    tibble::tibble(
      statistic_id = statistic_id,
      name = statistic$name,
      kind = statistic$kind,
      source = statistic$source,
      missing = statistic$missing
    )
  )
  plan$statistic_components <- dplyr::bind_rows(
    plan$statistic_components,
    tibble::tibble(
      statistic_id = rep(statistic_id, length(statistic$components)),
      component = statistic$components,
      type = unname(statistic$component_types),
      position = seq_along(statistic$components)
    )
  )
  plan$statistic_assignments <- dplyr::bind_rows(
    plan$statistic_assignments,
    tibble::tibble(
      statistic_id = rep(statistic_id, nrow(selection)),
      var_id = selection$var_id
    )
  )
  plan$statistic_functions[[statistic_id]] <- statistic$fun
  plan$next_statistic_number <- plan$next_statistic_number + 1L

  plan
}
