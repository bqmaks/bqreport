#' Compile an ordered family of group comparisons
#'
#' Pair orientation is a presentation-independent analytic decision. Keeping
#' it in one compiler prevents parametric and rank-based providers from
#' silently reversing the same declared family.
#'
#' @param group_values Unique group values in declared order.
#' @param family One of `"pairwise"`, `"reference"` or `"consecutive"`.
#' @param reference Reference value for a reference family, otherwise `NULL`.
#'
#' @return A tibble with comparison identifiers, positions and oriented group
#'   values.
#' @noRd
compile_comparison_family <- function(
  group_values,
  family,
  reference = NULL
) {
  if (
    !is.character(group_values) || length(group_values) < 2L ||
      anyNA(group_values) || any(!nzchar(group_values)) ||
      anyDuplicated(group_values)
  ) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "Comparison families require at least two unique non-empty group values."
    )
  }
  if (
    !is.character(family) || length(family) != 1L || is.na(family) ||
      !family %in% c("pairwise", "reference", "consecutive")
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`family` must be \"pairwise\", \"reference\" or \"consecutive\"."
    )
  }
  if (family == "reference") {
    if (
      !is.character(reference) || length(reference) != 1L ||
        is.na(reference) || !reference %in% group_values
    ) {
      bq_abort(
        "bq_error_invalid_analysis_input",
        "A reference family requires one declared `reference` group value."
      )
    }
  } else if (!is.null(reference)) {
    bq_abort(
      "bq_error_invalid_analysis_input",
      "`reference` must be NULL unless `family = \"reference\"`."
    )
  }

  positions <- if (family == "pairwise") {
    utils::combn(seq_along(group_values), 2L)
  } else if (family == "reference") {
    comparison_positions <- setdiff(seq_along(group_values), match(reference, group_values))
    rbind(
      rep(match(reference, group_values), length(comparison_positions)),
      comparison_positions
    )
  } else {
    rbind(
      seq_len(length(group_values) - 1L),
      seq.int(2L, length(group_values))
    )
  }
  comparison_n <- ncol(positions)

  tibble::tibble(
    comparison_id = sprintf("cmp%03d", seq_len(comparison_n)),
    position = seq_len(comparison_n),
    reference_value = group_values[positions[1L, ]],
    comparison_value = group_values[positions[2L, ]],
    direction = rep("comparison_minus_reference", comparison_n)
  )
}
