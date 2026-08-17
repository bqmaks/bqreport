#' Check whether an analysis plan is ready to run
#'
#' Performs analysis-specific checks without computing results. Analytic
#' problems are returned together in a flat diagnostics table so they can be
#' inspected and corrected before an engine runs. A damaged internal plan
#' structure raises an error because its references cannot be interpreted
#' safely.
#'
#' @param plan A `bq_plan` object.
#'
#' @return A `bq_preflight` object with readiness diagnostics and compiled cell
#'   registries.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA)))
#' data <- set_type(data, age, continuous())
#' plan <- plan_summary(data, age)
#' statistic <- continuous_statistic(
#'   "average",
#'   function(x) data.frame(mean = mean(x, na.rm = TRUE))
#' )
#' plan <- add_statistic(plan, age, statistic)
#' preflight(plan)
preflight <- function(plan) {
  if (!inherits(plan, "bq_plan")) {
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("`plan` must be a bq_plan object, not %s.", class(plan)[1L])
    )
  }

  UseMethod("preflight")
}

#' Check a summary analysis plan
#'
#' @param plan A `bq_plan_summary` object.
#'
#' @return A `bq_preflight_summary` object.
#' @exportS3Method
#' @noRd
preflight.bq_plan_summary <- function(plan) {
  expected_fields <- c(
    "analysis", "data", "variables", "group", "strata", "overall",
    "statistics", "statistic_components", "statistic_assignments",
    "statistic_functions", "next_statistic_number", "display_rules",
    "display_rule_assignments", "next_display_rule_number"
  )
  statistic_fields <- c(
    "statistic_id", "name", "kind", "source", "missing"
  )
  component_fields <- c(
    "statistic_id", "component", "type", "position"
  )
  assignment_fields <- c("statistic_id", "var_id")
  display_rule_fields <- c(
    "rule_id", "kind", "max_n", "display_statistics"
  )
  display_assignment_fields <- c("rule_id", "var_id")

  valid_structure <- identical(names(plan), expected_fields) &&
    identical(plan$analysis, "summary") &&
    inherits(plan$data, "bq_data") &&
    is.character(plan$variables) && length(plan$variables) > 0L &&
    !anyNA(plan$variables) && !anyDuplicated(plan$variables) &&
    is.character(plan$group) && length(plan$group) <= 1L &&
    !anyNA(plan$group) &&
    is.character(plan$strata) && !anyNA(plan$strata) &&
    !anyDuplicated(plan$strata) &&
    is.character(plan$overall) && !anyNA(plan$overall) &&
    !anyDuplicated(plan$overall) &&
    all(plan$overall %in% c("group", "strata")) &&
    is.data.frame(plan$statistics) &&
    identical(names(plan$statistics), statistic_fields) &&
    is.data.frame(plan$statistic_components) &&
    identical(names(plan$statistic_components), component_fields) &&
    is.data.frame(plan$statistic_assignments) &&
    identical(names(plan$statistic_assignments), assignment_fields) &&
    is.list(plan$statistic_functions) &&
    is.numeric(plan$next_statistic_number) &&
    length(plan$next_statistic_number) == 1L &&
    !is.na(plan$next_statistic_number) &&
    is.finite(plan$next_statistic_number) &&
    plan$next_statistic_number > 0 &&
    plan$next_statistic_number == floor(plan$next_statistic_number) &&
    is.data.frame(plan$display_rules) &&
    identical(names(plan$display_rules), display_rule_fields) &&
    is.data.frame(plan$display_rule_assignments) &&
    identical(
      names(plan$display_rule_assignments),
      display_assignment_fields
    ) &&
    is.numeric(plan$next_display_rule_number) &&
    length(plan$next_display_rule_number) == 1L &&
    !is.na(plan$next_display_rule_number) &&
    is.finite(plan$next_display_rule_number) &&
    plan$next_display_rule_number > 0 &&
    plan$next_display_rule_number == floor(plan$next_display_rule_number)

  if (!valid_structure) {
    bq_abort(
      "bq_error_invalid_plan",
      paste0(
        "`plan` has an invalid summary-plan structure; rebuild it with ",
        "`plan_summary()`, `add_statistic()` and `add_display_rule()`."
      )
    )
  }

  registry <- attr(plan$data, "variables")
  registry_fields <- c(
    "var_id", "name", "label", "role", "type", "event",
    "event_source", "reference", "type_source", "unit", "rounding", "digits"
  )
  function_names <- names(plan$statistic_functions)
  if (is.null(function_names)) {
    function_names <- character()
  }

  plan_ids <- c(plan$variables, plan$group, plan$strata)
  design_overlap <- intersect(plan$group, plan$strata)
  variable_overlap <- intersect(plan$variables, c(plan$group, plan$strata))
  valid_references <- is.data.frame(registry) &&
    identical(names(registry), registry_fields) &&
    nrow(registry) == ncol(plan$data) &&
    identical(registry$name, names(plan$data)) &&
    is.character(registry$var_id) && !anyNA(registry$var_id) &&
    !anyDuplicated(registry$var_id) &&
    is.character(registry$type) &&
    is.character(registry$unit) &&
    all(is.na(registry$unit) | nzchar(registry$unit)) &&
    is.character(registry$rounding) &&
    all(is.na(registry$rounding) | registry$rounding %in% c("decimal", "significant")) &&
    is.integer(registry$digits) &&
    all(is.na(registry$rounding) == is.na(registry$digits)) &&
    all(
      is.na(registry$digits) |
        (registry$rounding == "decimal" & registry$digits >= 0L) |
        (registry$rounding == "significant" & registry$digits >= 1L)
    ) &&
    all(plan_ids %in% registry$var_id) &&
    length(design_overlap) == 0L &&
    length(variable_overlap) == 0L &&
    !("group" %in% plan$overall && length(plan$group) == 0L) &&
    !("strata" %in% plan$overall && length(plan$strata) == 0L) &&
    all(vapply(plan$statistics, is.character, logical(1))) &&
    !anyNA(plan$statistics) &&
    !anyDuplicated(plan$statistics$statistic_id) &&
    !anyDuplicated(plan$statistics$name) &&
    is.character(plan$statistic_components$statistic_id) &&
    is.character(plan$statistic_components$component) &&
    is.character(plan$statistic_components$type) &&
    is.integer(plan$statistic_components$position) &&
    !anyNA(plan$statistic_components) &&
    all(plan$statistic_components$statistic_id %in% plan$statistics$statistic_id) &&
    all(plan$statistic_components$type %in% c("double", "integer")) &&
    is.character(plan$statistic_assignments$statistic_id) &&
    is.character(plan$statistic_assignments$var_id) &&
    !anyNA(plan$statistic_assignments) &&
    all(plan$statistic_assignments$statistic_id %in% plan$statistics$statistic_id) &&
    all(plan$statistic_assignments$var_id %in% plan$variables) &&
    identical(function_names, plan$statistics$statistic_id) &&
    all(vapply(plan$statistic_functions, is.function, logical(1))) &&
    is.character(plan$display_rules$rule_id) &&
    is.character(plan$display_rules$kind) &&
    is.integer(plan$display_rules$max_n) &&
    is.logical(plan$display_rules$display_statistics) &&
    !anyNA(plan$display_rules) &&
    !anyDuplicated(plan$display_rules$rule_id) &&
    all(plan$display_rules$kind == "enumerate_values") &&
    all(plan$display_rules$max_n > 0L) &&
    is.character(plan$display_rule_assignments$rule_id) &&
    is.character(plan$display_rule_assignments$var_id) &&
    !anyNA(plan$display_rule_assignments) &&
    !anyDuplicated(plan$display_rule_assignments$var_id) &&
    all(
      plan$display_rule_assignments$rule_id %in%
        plan$display_rules$rule_id
    ) &&
    all(plan$display_rule_assignments$var_id %in% plan$variables)

  if (nrow(plan$statistics) > 0L) {
    valid_references <- valid_references &&
      all(plan$statistics$statistic_id %in% plan$statistic_components$statistic_id) &&
      all(plan$statistics$statistic_id %in% plan$statistic_assignments$statistic_id)
  }

  if (nrow(plan$display_rules) > 0L) {
    valid_references <- valid_references &&
      all(
        plan$display_rules$rule_id %in%
          plan$display_rule_assignments$rule_id
      )
  }

  if (!valid_references) {
    bq_abort(
      "bq_error_invalid_plan",
      paste0(
        "`plan` contains inconsistent registries or identifiers; rebuild it ",
        "with `plan_summary()`, `add_statistic()` and `add_display_rule()`."
      )
    )
  }

  compiled_cells <- compile_summary_cells(plan)

  diagnostics <- tibble::tibble(
    severity = character(),
    code = character(),
    var_id = character(),
    statistic_id = character(),
    rule_id = character(),
    cell_id = character(),
    message = character()
  )

  for (var_id in plan_ids) {
    variable_row <- match(var_id, registry$var_id)
    variable_name <- registry$name[variable_row]
    variable_type <- registry$type[variable_row]

    if (is.na(variable_type)) {
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "missing_type",
          var_id = var_id,
          statistic_id = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            "Variable `%s` has no analytic type; set or infer it before analysis.",
            variable_name
          )
        )
      )
    } else if (variable_type == "unknown") {
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "unknown_type",
          var_id = var_id,
          statistic_id = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            "Variable `%s` has type `unknown`; set an analytic type before analysis.",
            variable_name
          )
        )
      )
    }

    if (
      !is.na(variable_type) && variable_type != "unknown" &&
        !variable_type %in% c("continuous", "count") &&
        !is.na(registry$unit[variable_row])
    ) {
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "incompatible_unit",
          var_id = var_id,
          statistic_id = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            "Variable `%s` has type `%s`, which cannot carry a measurement unit.",
            variable_name,
            variable_type
          )
        )
      )
    }

    if (
      !is.na(variable_type) && variable_type != "unknown" &&
        !variable_type %in% c("continuous", "count") &&
        !is.na(registry$rounding[variable_row])
    ) {
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "incompatible_rounding",
          var_id = var_id,
          statistic_id = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            paste0(
              "Variable `%s` has type `%s`, which cannot use a quantitative ",
              "rounding policy."
            ),
            variable_name,
            variable_type
          )
        )
      )
    }
  }

  for (var_id in c(plan$group, plan$strata)) {
    variable_row <- match(var_id, registry$var_id)
    variable_name <- registry$name[variable_row]
    missing_n <- sum(is.na(plan$data[[variable_name]]))

    if (missing_n > 0L) {
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "warning",
          code = "missing_design_value",
          var_id = var_id,
          statistic_id = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            paste0(
              "Variable `%s` has %d missing design value%s; these rows are ",
              "retained in explicit cells."
            ),
            variable_name,
            missing_n,
            if (missing_n == 1L) "" else "s"
          )
        )
      )
    }
  }

  empty_leaf_cells <- compiled_cells$cells$cell_id[
    compiled_cells$cells$n == 0L &
      !compiled_cells$cells$overall_group &
      !compiled_cells$cells$overall_strata
  ]
  for (cell_id in empty_leaf_cells) {
    diagnostics <- dplyr::bind_rows(
      diagnostics,
      tibble::tibble(
        severity = "warning",
        code = "empty_cell",
        var_id = NA_character_,
        statistic_id = NA_character_,
        rule_id = NA_character_,
        cell_id = cell_id,
        message = sprintf(
          "Cell `%s` has no rows; inspect `cell_axes` before analysis.",
          cell_id
        )
      )
    )
  }

  assigned_variables <- unique(plan$statistic_assignments$var_id)
  for (var_id in setdiff(plan$variables, assigned_variables)) {
    variable_name <- registry$name[match(var_id, registry$var_id)]
    diagnostics <- dplyr::bind_rows(
      diagnostics,
      tibble::tibble(
        severity = "error",
        code = "missing_statistic",
        var_id = var_id,
        statistic_id = NA_character_,
        rule_id = NA_character_,
        cell_id = NA_character_,
        message = sprintf(
          "Variable `%s` has no assigned statistic; add one before analysis.",
          variable_name
        )
      )
    )
  }

  for (assignment_row in seq_len(nrow(plan$display_rule_assignments))) {
    rule_id <- plan$display_rule_assignments$rule_id[assignment_row]
    var_id <- plan$display_rule_assignments$var_id[assignment_row]
    rule_kind <- plan$display_rules$kind[
      match(rule_id, plan$display_rules$rule_id)
    ]
    variable_row <- match(var_id, registry$var_id)
    variable_type <- registry$type[variable_row]

    if (
      rule_kind == "enumerate_values" &&
        !is.na(variable_type) && variable_type != "unknown" &&
        variable_type != "continuous"
    ) {
      variable_name <- registry$name[variable_row]
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "incompatible_display_rule",
          var_id = var_id,
          statistic_id = NA_character_,
          rule_id = rule_id,
          cell_id = NA_character_,
          message = sprintf(
            paste0(
              "Display rule `%s` requires a continuous variable, but `%s` ",
              "has type `%s`."
            ),
            rule_kind,
            variable_name,
            variable_type
          )
        )
      )
    }
  }

  for (assignment_row in seq_len(nrow(plan$statistic_assignments))) {
    statistic_id <- plan$statistic_assignments$statistic_id[assignment_row]
    var_id <- plan$statistic_assignments$var_id[assignment_row]
    statistic_kind <- plan$statistics$kind[
      match(statistic_id, plan$statistics$statistic_id)
    ]
    variable_row <- match(var_id, registry$var_id)
    variable_type <- registry$type[variable_row]

    if (
      statistic_kind == "continuous_statistic" &&
        !is.na(variable_type) && variable_type != "unknown" &&
        variable_type != "continuous"
    ) {
      statistic_name <- plan$statistics$name[
        match(statistic_id, plan$statistics$statistic_id)
      ]
      variable_name <- registry$name[variable_row]
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "incompatible_statistic",
          var_id = var_id,
          statistic_id = statistic_id,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            paste0(
              "Statistic `%s` requires a continuous variable, but `%s` has ",
              "type `%s`."
            ),
            statistic_name,
            variable_name,
            variable_type
          )
        )
      )
    }
  }

  structure(
    list(
      analysis = "summary",
      ok = !any(diagnostics$severity == "error"),
      diagnostics = diagnostics,
      cells = compiled_cells$cells,
      cell_axes = compiled_cells$cell_axes,
      cell_rows = compiled_cells$cell_rows
    ),
    class = c("bq_preflight_summary", "bq_preflight")
  )
}
