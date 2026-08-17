#' Resolve a tidyselect expression to stable variable identifiers
#'
#' Tidyselect works with the current names and positions of columns. Analysis
#' plans must not retain either, so selection is immediately joined to the
#' variable registry and returned with stable `var_id` values.
#'
#' @param data A `bq_data` object.
#' @param selection A quosure containing a tidyselect expression.
#' @param argument Name of the calling function's selection argument, used in
#'   errors.
#' @param min Minimum number of columns required.
#' @param max Maximum number of columns allowed.
#'
#' @return A tibble with `var_id`, `name` and `position`.
#' @noRd
resolve_variables <- function(data, selection, argument, min = 1L, max = Inf) {
  selected <- tryCatch(
    tidyselect::eval_select(selection, data),
    error = function(error) {
      bq_abort(
        "bq_error_invalid_selection",
        paste0("Cannot select `", argument, "`: ", conditionMessage(error))
      )
    }
  )

  selected_n <- length(selected)

  if (selected_n < min) {
    required <- if (min == 1L) "one column" else sprintf("%d columns", min)
    bq_abort(
      "bq_error_invalid_selection",
      sprintf("`%s` must select at least %s.", argument, required)
    )
  }

  if (selected_n > max) {
    if (identical(min, max)) {
      required <- if (max == 1L) "one column" else sprintf("%d columns", max)
      bq_abort(
        "bq_error_invalid_selection",
        sprintf(
          "`%s` must select exactly %s, not %d.",
          argument,
          required,
          selected_n
        )
      )
    }

    bq_abort(
      "bq_error_invalid_selection",
      sprintf(
        "`%s` must select at most %d column%s, not %d.",
        argument,
        max,
        if (max == 1L) "" else "s",
        selected_n
      )
    )
  }

  registry <- attr(data, "variables")
  positions <- unname(selected)

  tibble::tibble(
    var_id = registry$var_id[positions],
    name = names(selected),
    position = positions
  )
}
