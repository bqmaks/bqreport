#' Prepare analysis results for presentation
#'
#' Compiles presentation decisions without formatting or changing computed
#' estimates. Enumerated values remain numeric so rounding, locale and text
#' layout can be applied later.
#'
#' @param result A `bq_result` object.
#'
#' @return A `bq_presentation` object.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA)))
#' data <- set_type(data, age, continuous())
#' plan <- plan_summary(data, age)
#' average <- continuous_statistic(
#'   "average",
#'   function(x) {
#'     data.frame(
#'       mean = if (length(x) == 0L) NA_real_ else mean(x, na.rm = TRUE)
#'     )
#'   }
#' )
#' plan <- add_statistic(plan, age, average)
#' plan <- add_display_rule(plan, age, enumerate_values())
#' prepare_presentation(run_analysis(plan))
prepare_presentation <- function(result) {
  if (!inherits(result, "bq_result")) {
    bq_abort(
      "bq_error_invalid_result",
      sprintf("`result` must be a bq_result object, not %s.", class(result)[1L])
    )
  }

  UseMethod("prepare_presentation")
}

#' Prepare summary results for presentation
#'
#' @param result A `bq_result_summary` object.
#'
#' @return A `bq_presentation_summary` object.
#' @exportS3Method
#' @noRd
prepare_presentation.bq_result_summary <- function(result) {
  plan <- result$plan
  registry <- attr(plan$data, "variables")
  display_cell_records <- list()
  display_value_records <- list()
  next_display_cell <- 1L
  next_display_value <- 1L

  for (sample_row in seq_len(nrow(result$sample_sizes))) {
    cell_id <- result$sample_sizes$cell_id[sample_row]
    var_id <- result$sample_sizes$var_id[sample_row]
    n <- result$sample_sizes$n[sample_row]
    n_missing <- result$sample_sizes$n_missing[sample_row]
    cell_n <- result$cells$n[match(cell_id, result$cells$cell_id)]
    rule_id <- plan$display_rule_assignments$rule_id[
      match(var_id, plan$display_rule_assignments$var_id)
    ]

    if (
      length(cell_n) != 1L || is.na(cell_n) || is.na(n) ||
        is.na(n_missing) || n < 0L || n_missing < 0L ||
        as.double(n) + as.double(n_missing) != as.double(cell_n)
    ) {
      bq_abort(
        "bq_error_invalid_result",
        sprintf(
          "Stored sample sizes for variable `%s` in cell `%s` are inconsistent.",
          var_id,
          cell_id
        )
      )
    }

    status <- if (cell_n == 0L) {
      "empty"
    } else if (n == 0L && n_missing > 0L) {
      "all_missing"
    } else {
      "observed"
    }

    show_statistics <- TRUE
    show_values <- FALSE
    if (!is.na(rule_id) && status == "observed") {
      rule_row <- match(rule_id, plan$display_rules$rule_id)
      max_n <- plan$display_rules$max_n[rule_row]
      if (n <= max_n) {
        show_statistics <- plan$display_rules$display_statistics[rule_row]
        show_values <- TRUE
      }
    }

    display_cell_records[[next_display_cell]] <- tibble::tibble(
      cell_id = cell_id,
      var_id = var_id,
      status = status,
      show_statistics = show_statistics,
      show_values = show_values,
      rule_id = rule_id
    )
    next_display_cell <- next_display_cell + 1L

    if (show_values) {
      rows <- result$cell_rows$row[result$cell_rows$cell_id == cell_id]
      variable_name <- registry$name[match(var_id, registry$var_id)]
      values <- plan$data[[variable_name]][rows]
      values <- values[!is.na(values)]

      if (length(values) != n) {
        bq_abort(
          "bq_error_invalid_result",
          sprintf(
            paste0(
              "Stored sample size for variable `%s` in cell `%s` does not ",
              "match its source rows."
            ),
            variable_name,
            cell_id
          )
        )
      }

      display_value_records[[next_display_value]] <- tibble::tibble(
        cell_id = rep(cell_id, length(values)),
        var_id = rep(var_id, length(values)),
        position = seq_along(values),
        value = as.double(values)
      )
      next_display_value <- next_display_value + 1L
    }
  }

  display_cell_prototype <- tibble::tibble(
    cell_id = character(),
    var_id = character(),
    status = character(),
    show_statistics = logical(),
    show_values = logical(),
    rule_id = character()
  )
  display_value_prototype <- tibble::tibble(
    cell_id = character(),
    var_id = character(),
    position = integer(),
    value = double()
  )

  structure(
    list(
      analysis = "summary",
      result = result,
      display_cells = dplyr::bind_rows(
        c(list(display_cell_prototype), display_cell_records)
      ),
      display_values = dplyr::bind_rows(
        c(list(display_value_prototype), display_value_records)
      )
    ),
    class = c("bq_presentation_summary", "bq_presentation")
  )
}
