#' Check whether an analysis plan is ready to run
#'
#' Performs analysis-specific checks without computing results. Analytic
#' problems are returned together in a flat diagnostics table so they can be
#' inspected and corrected before an engine runs. A damaged internal plan
#' structure raises an error because its references cannot be interpreted
#' safely.
#'
#' Summary format placeholders are checked against components returned by the
#' statistics assigned to each variable. Unknown or ambiguous component names
#' are blocking diagnostics.
#'
#' @param plan A `bq_plan` object.
#'
#' @return A `bq_preflight` object with readiness diagnostics and compiled cell
#'   registries.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA)))
#' data <- set_type(data, age, type_continuous())
#' statistic <- continuous_statistic(
#'   "average",
#'   function(x) data.frame(mean = mean(x, na.rm = TRUE))
#' )
#' plan <- plan_summary(data) |>
#'   add_statistic(age, statistic)
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
    "statistic_id", "component", "type", "scale", "rounding", "digits",
    "position"
  )
  assignment_fields <- c("statistic_id", "var_id")
  display_rule_fields <- c(
    "rule_id", "kind", "max_n", "display_statistics"
  )
  display_assignment_fields <- c("rule_id", "var_id")

  valid_structure <- identical(names(plan), expected_fields) &&
    identical(plan$analysis, "summary") &&
    inherits(plan$data, "bq_data") &&
    is.character(plan$variables) &&
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
  summary_formats <- attr(plan$data, "summary_formats")
  summary_format_fields <- c(
    "var_id", "format_name", "template", "position"
  )
  placeholder_pattern <- "\\{[A-Za-z][A-Za-z0-9_.]*\\}"
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
    is.data.frame(summary_formats) &&
    identical(names(summary_formats), summary_format_fields) &&
    is.character(summary_formats$var_id) &&
    is.character(summary_formats$format_name) &&
    is.character(summary_formats$template) &&
    is.integer(summary_formats$position) &&
    !anyNA(summary_formats[c("var_id", "template", "position")]) &&
    all(summary_formats$var_id %in% registry$var_id) &&
    all(nzchar(summary_formats$template)) &&
    all(summary_formats$position > 0L) &&
    !anyDuplicated(summary_formats[c("var_id", "position")]) &&
    !any(duplicated(
      summary_formats[
        !is.na(summary_formats$format_name),
        c("var_id", "format_name")
      ]
    )) &&
    all(is.na(summary_formats$format_name) | nzchar(summary_formats$format_name)) &&
    all(vapply(
      split(summary_formats$position, summary_formats$var_id),
      function(position) identical(position, seq_along(position)),
      logical(1)
    )) &&
    all(grepl(placeholder_pattern, summary_formats$template, perl = TRUE)) &&
    !any(grepl(
      "[{}]",
      gsub(
        placeholder_pattern,
        "",
        summary_formats$template,
        perl = TRUE
      )
    )) &&
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
    is.character(plan$statistic_components$statistic_id) &&
    is.character(plan$statistic_components$component) &&
    is.character(plan$statistic_components$type) &&
    is.character(plan$statistic_components$scale) &&
    is.character(plan$statistic_components$rounding) &&
    is.integer(plan$statistic_components$digits) &&
    is.integer(plan$statistic_components$position) &&
    !anyNA(
      plan$statistic_components[
        c("statistic_id", "component", "type", "scale", "position")
      ]
    ) &&
    all(plan$statistic_components$statistic_id %in% plan$statistics$statistic_id) &&
    all(plan$statistic_components$type %in% c("double", "integer")) &&
    all(
      plan$statistic_components$scale %in%
        c("variable", "count", "dimensionless")
    ) &&
    all(
      plan$statistic_components$scale != "count" |
        plan$statistic_components$type == "integer"
    ) &&
    all(
      is.na(plan$statistic_components$rounding) |
        plan$statistic_components$rounding %in% c("decimal", "significant")
    ) &&
    all(
      is.na(plan$statistic_components$rounding) ==
        is.na(plan$statistic_components$digits)
    ) &&
    all(
      is.na(plan$statistic_components$digits) |
        (plan$statistic_components$rounding == "decimal" &
          plan$statistic_components$digits >= 0L) |
        (plan$statistic_components$rounding == "significant" &
          plan$statistic_components$digits >= 1L)
    ) &&
    all(
      plan$statistic_components$scale != "count" |
        is.na(plan$statistic_components$rounding)
    ) &&
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

  diagnostics <- tibble::tibble(
    severity = character(),
    code = character(),
    var_id = character(),
    statistic_id = character(),
    component = character(),
    rule_id = character(),
    cell_id = character(),
    message = character()
  )

  if (length(plan$variables) == 0L) {
    diagnostics <- dplyr::bind_rows(
      diagnostics,
      tibble::tibble(
        severity = "error",
        code = "missing_summary_variable",
        var_id = NA_character_,
        statistic_id = NA_character_,
        component = NA_character_,
        rule_id = NA_character_,
        cell_id = NA_character_,
        message = paste0(
          "The summary plan has no variables; add at least one with ",
          "`add_statistic()`."
        )
      )
    )
  }

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
          component = NA_character_,
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
          component = NA_character_,
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
      !is.na(variable_type) && variable_type == "continuous" &&
        is.null(as_continuous_model_vector(plan$data[[variable_name]]))
    ) {
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "invalid_continuous_storage",
          var_id = var_id,
          statistic_id = NA_character_,
          component = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            paste0(
              "Continuous variable `%s` cannot be converted to finite numeric ",
              "values; correct its storage or analytic type before analysis."
            ),
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
          component = NA_character_,
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
          component = NA_character_,
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

  design_ready <- TRUE
  for (var_id in c(plan$group, plan$strata)) {
    variable_row <- match(var_id, registry$var_id)
    variable_name <- registry$name[variable_row]
    variable_type <- registry$type[variable_row]
    column <- plan$data[[variable_name]]

    if (!is.atomic(column) || !is.null(dim(column))) {
      design_ready <- FALSE
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "invalid_design_storage",
          var_id = var_id,
          statistic_id = NA_character_,
          component = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            paste0(
              "Design variable `%s` must be stored as an atomic vector; ",
              "replace its list or matrix column before analysis."
            ),
            variable_name
          )
        )
      )
      next
    }

    observed <- unique(as.character(column[!is.na(column)]))
    undeclared <- character()
    if (!is.na(variable_type) && variable_type == "binary" && length(observed) > 2L) {
      undeclared <- observed[3L]
    } else if (!is.na(variable_type) && variable_type == "ordinal") {
      declared <- attr(plan$data, "levels")
      declared <- declared$value[declared$var_id == var_id]
      undeclared <- setdiff(observed, declared)
    }

    if (length(undeclared) > 0L) {
      design_ready <- FALSE
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "error",
          code = "invalid_design_domain",
          var_id = var_id,
          statistic_id = NA_character_,
          component = NA_character_,
          rule_id = NA_character_,
          cell_id = NA_character_,
          message = sprintf(
            paste0(
              "Design variable `%s` has value %s outside its declared `%s` ",
              "domain; update the data or declare its type again."
            ),
            variable_name,
            encodeString(undeclared[[1L]], quote = '"'),
            variable_type
          )
        )
      )
    }

    missing_n <- sum(is.na(column))

    if (missing_n > 0L) {
      diagnostics <- dplyr::bind_rows(
        diagnostics,
        tibble::tibble(
          severity = "warning",
          code = "missing_design_value",
          var_id = var_id,
          statistic_id = NA_character_,
          component = NA_character_,
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

  compiled_cells <- if (design_ready) {
    compile_summary_cells(plan)
  } else {
    list(
      cells = tibble::tibble(
        cell_id = character(),
        overall_group = logical(),
        overall_strata = logical(),
        n = integer()
      ),
      cell_axes = tibble::tibble(
        cell_id = character(),
        var_id = character(),
        value = character(),
        is_overall = logical()
      ),
      cell_rows = tibble::tibble(cell_id = character(), row = integer())
    )
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
        component = NA_character_,
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
        component = NA_character_,
        rule_id = NA_character_,
        cell_id = NA_character_,
        message = sprintf(
          "Variable `%s` has no assigned statistic; add one before analysis.",
          variable_name
        )
      )
    )
  }

  for (var_id in plan$variables) {
    variable_formats <- summary_formats[summary_formats$var_id == var_id, ]
    assigned_statistic_ids <- plan$statistic_assignments$statistic_id[
      plan$statistic_assignments$var_id == var_id
    ]
    if (nrow(variable_formats) == 0L || length(assigned_statistic_ids) == 0L) {
      next
    }

    assigned_components <- plan$statistic_components[
      plan$statistic_components$statistic_id %in% assigned_statistic_ids,
      c("statistic_id", "component")
    ]
    variable_name <- registry$name[match(var_id, registry$var_id)]

    for (format_row in seq_len(nrow(variable_formats))) {
      template <- variable_formats$template[format_row]
      placeholder_matches <- regmatches(
        template,
        gregexpr(placeholder_pattern, template, perl = TRUE)
      )[[1L]]
      placeholders <- unique(substring(
        placeholder_matches,
        2L,
        nchar(placeholder_matches) - 1L
      ))
      format_name <- variable_formats$format_name[format_row]
      format_description <- if (is.na(format_name)) {
        sprintf("at position %d", variable_formats$position[format_row])
      } else {
        sprintf("`%s`", format_name)
      }

      for (component in placeholders) {
        matching_statistic_ids <- unique(
          assigned_components$statistic_id[
            assigned_components$component == component
          ]
        )

        if (length(matching_statistic_ids) == 0L) {
          diagnostics <- dplyr::bind_rows(
            diagnostics,
            tibble::tibble(
              severity = "error",
              code = "unknown_summary_component",
              var_id = var_id,
              statistic_id = NA_character_,
              component = component,
              rule_id = NA_character_,
              cell_id = NA_character_,
              message = sprintf(
                paste0(
                  "Summary format %s for variable `%s` references component ",
                  "`%s`, but no assigned statistic returns it."
                ),
                format_description,
                variable_name,
                component
              )
            )
          )
        } else if (length(matching_statistic_ids) > 1L) {
          statistic_names <- plan$statistics$name[
            match(matching_statistic_ids, plan$statistics$statistic_id)
          ]
          diagnostics <- dplyr::bind_rows(
            diagnostics,
            tibble::tibble(
              severity = "error",
              code = "ambiguous_summary_component",
              var_id = var_id,
              statistic_id = NA_character_,
              component = component,
              rule_id = NA_character_,
              cell_id = NA_character_,
              message = sprintf(
                paste0(
                  "Summary format %s for variable `%s` references component ",
                  "`%s`, which is returned by multiple statistics: %s."
                ),
                format_description,
                variable_name,
                component,
                paste(statistic_names, collapse = ", ")
              )
            )
          )
        }
      }
    }
  }

  missing_rounding_rows <- which(
    plan$statistic_components$scale == "dimensionless" &
      is.na(plan$statistic_components$rounding)
  )
  for (component_row in missing_rounding_rows) {
    statistic_id <- plan$statistic_components$statistic_id[component_row]
    component <- plan$statistic_components$component[component_row]
    statistic_name <- plan$statistics$name[
      match(statistic_id, plan$statistics$statistic_id)
    ]
    diagnostics <- dplyr::bind_rows(
      diagnostics,
      tibble::tibble(
        severity = "error",
        code = "missing_component_rounding",
        var_id = NA_character_,
        statistic_id = statistic_id,
        component = component,
        rule_id = NA_character_,
        cell_id = NA_character_,
        message = sprintf(
          paste0(
            "Dimensionless component `%s` of statistic `%s` has no rounding ",
            "policy; set it with `set_component_rounding()` before adding ",
            "the statistic to a plan."
          ),
          component,
          statistic_name
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
          component = NA_character_,
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
          component = NA_character_,
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
