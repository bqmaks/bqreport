#' Compile summary design cells
#'
#' Expands group and strata values into leaf cells and the raw Overall
#' collapses requested by a summary plan. Arbitrary numbers of strata stay
#' flat by using separate cell, axis and row registries.
#'
#' Factor axes use every declared level, including unused levels. Other atomic
#' axes use their observed values in deterministic order. Missing axis values
#' form an explicit cell rather than causing rows to disappear.
#'
#' @param plan A `bq_plan_summary` object with structurally valid design
#'   references.
#'
#' @return A named list with `cells`, `cell_axes` and `cell_rows` tibbles.
#' @noRd
compile_summary_cells <- function(plan) {
  if (!inherits(plan, "bq_plan_summary")) {
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("`plan` must be a bq_plan_summary object, not %s.", class(plan)[1L])
    )
  }

  registry <- attr(plan$data, "variables")
  axis_ids <- c(plan$group, plan$strata)
  axis_names <- registry$name[match(axis_ids, registry$var_id)]
  group_axis <- seq_along(axis_ids) <= length(plan$group)
  strata_axis <- !group_axis
  level_registry <- attr(plan$data, "levels")

  axis_values <- lapply(seq_along(axis_ids), function(axis_position) {
    axis_id <- axis_ids[[axis_position]]
    axis_name <- axis_names[[axis_position]]
    column <- plan$data[[axis_name]]
    variable_type <- registry$type[match(axis_id, registry$var_id)]

    if (!is.na(variable_type) && variable_type == "ordinal") {
      declared <- level_registry[level_registry$var_id == axis_id, ]
      declared <- declared[order(declared$position), , drop = FALSE]
      values <- declared$value
      if (anyNA(column)) {
        values <- c(values, NA_character_)
      }
      return(values)
    }

    if (is.factor(column)) {
      values <- levels(column)
      if (anyNA(column)) {
        values <- c(values, NA_character_)
      }
      return(values)
    }

    observed <- unique(column[!is.na(column)])
    if (is.character(observed)) {
      observed <- sort(enc2utf8(observed), method = "radix")
    } else if (is.atomic(observed)) {
      observed <- sort(observed)
    }

    if (anyNA(column)) {
      observed <- c(observed, column[which(is.na(column))[1L]])
    }
    observed
  })

  collapse_patterns <- list(c(group = FALSE, strata = FALSE))
  if ("group" %in% plan$overall) {
    collapse_patterns <- c(
      collapse_patterns,
      list(c(group = TRUE, strata = FALSE))
    )
  }
  if ("strata" %in% plan$overall) {
    collapse_patterns <- c(
      collapse_patterns,
      list(c(group = FALSE, strata = TRUE))
    )
  }
  if (all(c("group", "strata") %in% plan$overall)) {
    collapse_patterns <- c(
      collapse_patterns,
      list(c(group = TRUE, strata = TRUE))
    )
  }

  cell_records <- list()
  axis_records <- list()
  row_records <- list()
  next_cell_number <- 1L

  for (collapse in collapse_patterns) {
    collapsed_axes <- (group_axis & collapse[["group"]]) |
      (strata_axis & collapse[["strata"]])
    active_axes <- which(!collapsed_axes)
    active_indices <- lapply(axis_values[active_axes], seq_along)

    if (length(active_indices) == 0L) {
      grid <- data.frame(.cell = 1L)[, FALSE, drop = FALSE]
    } else {
      grid <- do.call(
        expand.grid,
        c(
          active_indices,
          list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
        )
      )
    }

    for (grid_row in seq_len(nrow(grid))) {
      included <- rep(TRUE, nrow(plan$data))
      axis_output <- rep(NA_character_, length(axis_ids))

      for (active_position in seq_along(active_axes)) {
        axis_position <- active_axes[active_position]
        value_position <- grid[[active_position]][grid_row]
        value <- axis_values[[axis_position]][value_position]
        column <- plan$data[[axis_names[axis_position]]]

        if (is.na(value)) {
          included <- included & is.na(column)
        } else {
          equal <- column == value
          equal[is.na(equal)] <- FALSE
          included <- included & equal
        }
        axis_output[axis_position] <- as.character(value)
      }

      cell_id <- sprintf("c%03d", next_cell_number)
      included_rows <- which(included)
      cell_records[[next_cell_number]] <- tibble::tibble(
        cell_id = cell_id,
        overall_group = unname(collapse[["group"]]),
        overall_strata = unname(collapse[["strata"]]),
        n = as.integer(length(included_rows))
      )

      if (length(axis_ids) > 0L) {
        axis_records[[next_cell_number]] <- tibble::tibble(
          cell_id = rep(cell_id, length(axis_ids)),
          var_id = axis_ids,
          value = axis_output,
          is_overall = collapsed_axes
        )
      }

      if (length(included_rows) > 0L) {
        row_records[[next_cell_number]] <- tibble::tibble(
          cell_id = rep(cell_id, length(included_rows)),
          row = as.integer(included_rows)
        )
      }
      next_cell_number <- next_cell_number + 1L
    }
  }

  cell_prototype <- tibble::tibble(
    cell_id = character(),
    overall_group = logical(),
    overall_strata = logical(),
    n = integer()
  )
  axis_prototype <- tibble::tibble(
    cell_id = character(),
    var_id = character(),
    value = character(),
    is_overall = logical()
  )
  row_prototype <- tibble::tibble(
    cell_id = character(),
    row = integer()
  )

  list(
    cells = dplyr::bind_rows(c(list(cell_prototype), cell_records)),
    cell_axes = dplyr::bind_rows(c(list(axis_prototype), axis_records)),
    cell_rows = dplyr::bind_rows(c(list(row_prototype), row_records))
  )
}
