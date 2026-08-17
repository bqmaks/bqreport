#' Check a statistic specification
#'
#' @param statistic An object to validate.
#'
#' @return `TRUE` when the object follows the statistic specification schema.
#' @noRd
is_valid_statistic_specification <- function(statistic) {
  expected_fields <- c(
    "kind", "name", "components", "component_types", "component_scales",
    "component_rounding", "component_digits", "source", "missing", "fun"
  )

  inherits(statistic, "bq_statistic") &&
    identical(names(statistic), expected_fields) &&
    is.character(statistic$kind) && length(statistic$kind) == 1L &&
    !is.na(statistic$kind) && nzchar(statistic$kind) &&
    is.character(statistic$name) && length(statistic$name) == 1L &&
    !is.na(statistic$name) && nzchar(statistic$name) &&
    is.character(statistic$components) && length(statistic$components) > 0L &&
    !anyNA(statistic$components) && all(nzchar(statistic$components)) &&
    !anyDuplicated(statistic$components) &&
    is.character(statistic$component_types) &&
    length(statistic$component_types) == length(statistic$components) &&
    identical(names(statistic$component_types), statistic$components) &&
    all(statistic$component_types %in% c("double", "integer")) &&
    is.character(statistic$component_scales) &&
    length(statistic$component_scales) == length(statistic$components) &&
    identical(names(statistic$component_scales), statistic$components) &&
    all(
      statistic$component_scales %in%
        c("variable", "count", "dimensionless")
    ) &&
    all(
      statistic$component_scales != "count" |
        statistic$component_types == "integer"
    ) &&
    is.character(statistic$component_rounding) &&
    length(statistic$component_rounding) == length(statistic$components) &&
    identical(names(statistic$component_rounding), statistic$components) &&
    all(
      is.na(statistic$component_rounding) |
        statistic$component_rounding %in% c("decimal", "significant")
    ) &&
    is.integer(statistic$component_digits) &&
    length(statistic$component_digits) == length(statistic$components) &&
    identical(names(statistic$component_digits), statistic$components) &&
    all(
      is.na(statistic$component_rounding) ==
        is.na(statistic$component_digits)
    ) &&
    all(
      is.na(statistic$component_digits) |
        (statistic$component_rounding == "decimal" &
          statistic$component_digits >= 0L) |
        (statistic$component_rounding == "significant" &
          statistic$component_digits >= 1L)
    ) &&
    all(
      statistic$component_scales != "count" |
        is.na(statistic$component_rounding)
    ) &&
    is.character(statistic$source) && length(statistic$source) == 1L &&
    !is.na(statistic$source) && nzchar(statistic$source) &&
    is.character(statistic$missing) && length(statistic$missing) == 1L &&
    !is.na(statistic$missing) && nzchar(statistic$missing) &&
    is.function(statistic$fun)
}

#' Add a statistic to a summary plan
#'
#' Selects one or more summary variables and assigns a calculation
#' specification to them. Variables are added to the plan in first-selection
#' order. Declarative metadata, output components and assignments stay in flat
#' tables; the executable function is stored separately under the same stable
#' `statistic_id`.
#'
#' @param plan A `bq_plan_summary` object.
#' @param variables A tidyselect expression selecting one or more variables.
#'   Design-axis variables cannot also be summary variables.
#' @param statistic A `bq_statistic` specification. The default is
#'   [continuous_descriptives()].
#'
#' @return A copy of `plan` with the statistic registered and assigned.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA), bmi = c(22, 31, 27)))
#' data <- set_type(data, age, continuous())
#' data <- set_type(data, bmi, continuous())
#' plan_summary(data) |>
#'   add_statistic(c(age, bmi))
add_statistic <- function(
  plan,
  variables,
  statistic = continuous_descriptives()
) {
  if (!inherits(plan, "bq_plan_summary")) {
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("`plan` must be a bq_plan_summary object, not %s.", class(plan)[1L])
    )
  }

  valid_statistic <- is_valid_statistic_specification(statistic)

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
  design_ids <- c(plan$group, plan$strata)
  design_overlap <- intersect(selection$var_id, design_ids)

  if (length(design_overlap) > 0L) {
    variable_name <- selection$name[
      match(design_overlap[1L], selection$var_id)
    ]
    bq_abort(
      "bq_error_invalid_plan",
      sprintf(
        "Variable `%s` is a design axis and cannot also be summarised.",
        variable_name
      )
    )
  }

  existing_named_assignments <- plan$statistic_assignments$var_id[
    plan$statistic_assignments$statistic_id %in%
      plan$statistics$statistic_id[plan$statistics$name == statistic$name]
  ]
  duplicate_assignment <- intersect(
    selection$var_id,
    existing_named_assignments
  )

  if (length(duplicate_assignment) > 0L) {
    variable_name <- selection$name[
      match(duplicate_assignment[1L], selection$var_id)
    ]
    bq_abort(
      "bq_error_invalid_plan",
      sprintf(
        "Variable `%s` already has a statistic named `%s`.",
        variable_name,
        statistic$name
      )
    )
  }

  plan$variables <- c(
    plan$variables,
    setdiff(selection$var_id, plan$variables)
  )

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
      scale = unname(statistic$component_scales),
      rounding = unname(statistic$component_rounding),
      digits = unname(statistic$component_digits),
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
