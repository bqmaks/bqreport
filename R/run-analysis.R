#' Run an analysis plan
#'
#' Checks a plan and executes its analysis engine. A plan with blocking
#' preflight diagnostics is rejected before any statistic is computed.
#'
#' @param plan A `bq_plan` object.
#'
#' @return A `bq_result` object.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA)))
#' data <- set_type(data, age, continuous())
#' average <- continuous_statistic(
#'   "average",
#'   function(x) {
#'     data.frame(
#'       mean = if (length(x) == 0L) NA_real_ else mean(x, na.rm = TRUE)
#'     )
#'   }
#' )
#' plan <- plan_summary(data) |>
#'   add_statistic(age, average)
#' run_analysis(plan)
run_analysis <- function(plan) {
  if (!inherits(plan, "bq_plan")) {
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("`plan` must be a bq_plan object, not %s.", class(plan)[1L])
    )
  }

  UseMethod("run_analysis")
}

#' Run a summary analysis plan
#'
#' @param plan A `bq_plan_summary` object.
#'
#' @return A `bq_result_summary` object.
#' @exportS3Method
#' @noRd
run_analysis.bq_plan_summary <- function(plan) {
  checked <- preflight(plan)

  if (!checked$ok) {
    bq_abort(
      "bq_error_preflight",
      paste0(
        "Analysis plan is not ready; inspect `preflight(plan)$diagnostics` ",
        "and correct all errors before running it."
      ),
      preflight = checked
    )
  }

  registry <- attr(plan$data, "variables")
  model_variables <- lapply(plan$variables, function(var_id) {
    variable_name <- registry$name[match(var_id, registry$var_id)]
    as_continuous_model_vector(plan$data[[variable_name]])
  })
  names(model_variables) <- plan$variables
  sample_size_records <- list()
  estimate_records <- list()
  next_sample_size <- 1L
  next_estimate <- 1L

  for (cell_id in checked$cells$cell_id) {
    rows <- checked$cell_rows$row[checked$cell_rows$cell_id == cell_id]

    for (var_id in plan$variables) {
      variable_name <- registry$name[match(var_id, registry$var_id)]
      x <- model_variables[[var_id]][rows]
      sample_size_records[[next_sample_size]] <- tibble::tibble(
        cell_id = cell_id,
        var_id = var_id,
        n = as.integer(sum(!is.na(x))),
        n_missing = as.integer(sum(is.na(x)))
      )
      next_sample_size <- next_sample_size + 1L
    }

    for (assignment_row in seq_len(nrow(plan$statistic_assignments))) {
      statistic_id <- plan$statistic_assignments$statistic_id[assignment_row]
      var_id <- plan$statistic_assignments$var_id[assignment_row]
      variable_name <- registry$name[match(var_id, registry$var_id)]
      statistic_name <- plan$statistics$name[
        match(statistic_id, plan$statistics$statistic_id)
      ]
      x <- model_variables[[var_id]][rows]
      fun <- plan$statistic_functions[[statistic_id]]

      output <- tryCatch(
        fun(x),
        error = function(error) {
          bq_abort(
            "bq_error_statistic_runtime",
            sprintf(
              paste0(
                "Statistic `%s` failed for variable `%s` in cell `%s`: %s"
              ),
              statistic_name,
              variable_name,
              cell_id,
              conditionMessage(error)
            ),
            cell_id = cell_id,
            var_id = var_id,
            statistic_id = statistic_id,
            parent = error
          )
        }
      )

      expected_components <- plan$statistic_components$component[
        plan$statistic_components$statistic_id == statistic_id
      ]
      expected_types <- plan$statistic_components$type[
        plan$statistic_components$statistic_id == statistic_id
      ]

      if (!is.data.frame(output)) {
        bq_abort(
          "bq_error_statistic_schema",
          sprintf(
            paste0(
              "Statistic `%s` returned a non-data-frame result for variable ",
              "`%s` in cell `%s`."
            ),
            statistic_name,
            variable_name,
            cell_id
          ),
          cell_id = cell_id,
          var_id = var_id,
          statistic_id = statistic_id
        )
      }

      if (nrow(output) != 1L) {
        bq_abort(
          "bq_error_statistic_schema",
          sprintf(
            paste0(
              "Statistic `%s` returned %d rows for variable `%s` in cell ",
              "`%s`; exactly one row is required."
            ),
            statistic_name,
            nrow(output),
            variable_name,
            cell_id
          ),
          cell_id = cell_id,
          var_id = var_id,
          statistic_id = statistic_id
        )
      }

      if (!identical(names(output), expected_components)) {
        bq_abort(
          "bq_error_statistic_schema",
          sprintf(
            paste0(
              "Statistic `%s` returned different components for variable ",
              "`%s` in cell `%s`; expected: %s."
            ),
            statistic_name,
            variable_name,
            cell_id,
            paste(expected_components, collapse = ", ")
          ),
          cell_id = cell_id,
          var_id = var_id,
          statistic_id = statistic_id
        )
      }

      plain_numeric <- vapply(
        output,
        function(column) {
          is.numeric(column) && !is.object(column) && is.null(dim(column))
        },
        logical(1)
      )

      if (!all(plain_numeric)) {
        bq_abort(
          "bq_error_statistic_schema",
          sprintf(
            paste0(
              "Statistic `%s` returned a non-plain-numeric component for ",
              "variable `%s` in cell `%s`."
            ),
            statistic_name,
            variable_name,
            cell_id
          ),
          cell_id = cell_id,
          var_id = var_id,
          statistic_id = statistic_id
        )
      }

      actual_types <- unname(vapply(output, typeof, character(1)))
      if (!identical(actual_types, expected_types)) {
        bq_abort(
          "bq_error_statistic_schema",
          sprintf(
            paste0(
              "Statistic `%s` returned different component types for ",
              "variable `%s` in cell `%s`."
            ),
            statistic_name,
            variable_name,
            cell_id
          ),
          cell_id = cell_id,
          var_id = var_id,
          statistic_id = statistic_id
        )
      }

      for (component_position in seq_along(expected_components)) {
        estimate_records[[next_estimate]] <- tibble::tibble(
          cell_id = cell_id,
          var_id = var_id,
          statistic_id = statistic_id,
          component = expected_components[component_position],
          value = as.double(output[[component_position]][1L])
        )
        next_estimate <- next_estimate + 1L
      }
    }
  }

  sample_size_prototype <- tibble::tibble(
    cell_id = character(),
    var_id = character(),
    n = integer(),
    n_missing = integer()
  )
  estimate_prototype <- tibble::tibble(
    cell_id = character(),
    var_id = character(),
    statistic_id = character(),
    component = character(),
    value = double()
  )

  structure(
    list(
      analysis = "summary",
      plan = plan,
      diagnostics = checked$diagnostics,
      cells = checked$cells,
      cell_axes = checked$cell_axes,
      cell_rows = checked$cell_rows,
      sample_sizes = dplyr::bind_rows(
        c(list(sample_size_prototype), sample_size_records)
      ),
      estimates = dplyr::bind_rows(
        c(list(estimate_prototype), estimate_records)
      )
    ),
    class = c("bq_result_summary", "bq_result")
  )
}
