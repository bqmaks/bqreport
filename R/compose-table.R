#' Compose a neutral summary table specification
#'
#' Arranges already formatted summary output into flat row, column, header and
#' body registries. The result contains no renderer-specific objects and does
#' not perform numeric calculations or formatting.
#'
#' @param formatted A `bq_formatted_presentation` object.
#'
#' @return A `bq_table` object.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(value = c(1.2, 3)))
#' data <- set_type(data, value, continuous())
#' data <- set_rounding(data, value, 1)
#' statistic <- continuous_statistic(
#'   "mean",
#'   function(x) data.frame(mean = mean(x))
#' )
#' plan <- plan_summary(data, value)
#' plan <- add_statistic(plan, value, statistic)
#' formatted <- format_presentation(prepare_presentation(run_analysis(plan)))
#' compose_table(formatted)
compose_table <- function(formatted) {
  if (!inherits(formatted, "bq_formatted_presentation")) {
    bq_abort(
      "bq_error_invalid_presentation",
      sprintf(
        "`formatted` must be a bq_formatted_presentation object, not %s.",
        class(formatted)[1L]
      )
    )
  }

  UseMethod("compose_table")
}

#' Compose a neutral summary table specification
#'
#' @param formatted A `bq_formatted_presentation_summary` object.
#'
#' @return A `bq_table_summary` object.
#' @exportS3Method
#' @noRd
compose_table.bq_formatted_presentation_summary <- function(formatted) {
  result <- formatted$presentation$result
  plan <- result$plan
  registry <- attr(plan$data, "variables")
  row_records <- list()
  next_row_number <- 1L

  for (var_id in plan$variables) {
    variable_row <- match(var_id, registry$var_id)
    variable_label <- registry$label[variable_row]
    if (is.na(variable_label)) {
      variable_label <- registry$name[variable_row]
    }
    variable_displays <- formatted$display_cells[
      formatted$display_cells$var_id == var_id,
    ]
    observed_displays <- variable_displays$status == "observed"
    show_statistic_rows <- any(
      observed_displays & variable_displays$show_statistics
    ) || !any(observed_displays)

    assigned_statistics <- plan$statistics$statistic_id[
      plan$statistics$statistic_id %in%
        plan$statistic_assignments$statistic_id[
          plan$statistic_assignments$var_id == var_id
        ]
    ]

    if (show_statistic_rows) {
      for (statistic_id in assigned_statistics) {
        statistic_name <- plan$statistics$name[
          match(statistic_id, plan$statistics$statistic_id)
        ]
        components <- plan$statistic_components[
          plan$statistic_components$statistic_id == statistic_id,
          c("component", "scale", "position")
        ]
        components <- components[order(components$position), ]

        for (component_row in seq_len(nrow(components))) {
          row_records[[next_row_number]] <- tibble::tibble(
            row_id = sprintf("r%03d", next_row_number),
            var_id = var_id,
            row_kind = "statistic",
            statistic_id = statistic_id,
            statistic_name = statistic_name,
            component = components$component[component_row],
            component_scale = components$scale[component_row],
            variable_label = variable_label,
            unit = registry$unit[variable_row],
            position = next_row_number
          )
          next_row_number <- next_row_number + 1L
        }
      }
    }

    if (any(
      formatted$display_cells$var_id == var_id &
        formatted$display_cells$show_values
    )) {
      row_records[[next_row_number]] <- tibble::tibble(
        row_id = sprintf("r%03d", next_row_number),
        var_id = var_id,
        row_kind = "enumeration",
        statistic_id = NA_character_,
        statistic_name = NA_character_,
        component = NA_character_,
        component_scale = NA_character_,
        variable_label = variable_label,
        unit = registry$unit[variable_row],
        position = next_row_number
      )
      next_row_number <- next_row_number + 1L
    }
  }

  row_prototype <- tibble::tibble(
    row_id = character(),
    var_id = character(),
    row_kind = character(),
    statistic_id = character(),
    statistic_name = character(),
    component = character(),
    component_scale = character(),
    variable_label = character(),
    unit = character(),
    position = integer()
  )
  rows <- dplyr::bind_rows(c(list(row_prototype), row_records))

  columns <- result$cells[, c(
    "cell_id", "overall_group", "overall_strata", "n"
  )]
  columns$position <- seq_len(nrow(columns))
  columns <- columns[, c(
    "cell_id", "position", "overall_group", "overall_strata", "n"
  )]

  column_axes <- result$cell_axes
  if (nrow(column_axes) == 0L) {
    column_axes <- tibble::tibble(
      cell_id = character(),
      var_id = character(),
      axis = character(),
      axis_position = integer(),
      value = character(),
      is_overall = logical()
    )
  } else {
    axis_ids <- c(plan$group, plan$strata)
    column_axes$axis <- ifelse(
      column_axes$var_id %in% plan$group,
      "group",
      "strata"
    )
    column_axes$axis_position <- match(column_axes$var_id, axis_ids)
    column_axes <- column_axes[, c(
      "cell_id", "var_id", "axis", "axis_position", "value", "is_overall"
    )]
  }

  cell_displays <- formatted$display_cells[, c(
    "cell_id", "var_id", "status", "show_statistics", "show_values",
    "rule_id", "status_text"
  )]
  body_records <- list()
  next_body_record <- 1L

  for (row_number in seq_len(nrow(rows))) {
    row <- rows[row_number, ]
    displays <- cell_displays[
      cell_displays$var_id == row$var_id &
        cell_displays$status == "observed",
    ]

    if (row$row_kind == "statistic") {
      displays <- displays[displays$show_statistics, ]
      values <- formatted$formatted_estimates[
        formatted$formatted_estimates$var_id == row$var_id &
          formatted$formatted_estimates$statistic_id == row$statistic_id &
          formatted$formatted_estimates$component == row$component,
        c("cell_id", "value")
      ]
    } else {
      displays <- displays[displays$show_values, ]
      values <- formatted$enumerations[
        formatted$enumerations$var_id == row$var_id,
        c("cell_id", "value")
      ]
    }

    visible_values <- values[
      match(displays$cell_id, values$cell_id),
      c("cell_id", "value")
    ]
    if (nrow(visible_values) > 0L) {
      body_records[[next_body_record]] <- tibble::tibble(
        row_id = rep(row$row_id, nrow(visible_values)),
        cell_id = visible_values$cell_id,
        value = visible_values$value
      )
      next_body_record <- next_body_record + 1L
    }
  }

  body_prototype <- tibble::tibble(
    row_id = character(),
    cell_id = character(),
    value = character()
  )

  structure(
    list(
      analysis = "summary",
      formatted = formatted,
      rows = rows,
      columns = columns,
      column_axes = column_axes,
      cell_displays = cell_displays,
      body = dplyr::bind_rows(c(list(body_prototype), body_records))
    ),
    class = c("bq_table_summary", "bq_table")
  )
}
