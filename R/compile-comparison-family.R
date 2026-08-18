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
    reference_position <- match(reference, group_values)
    comparison_positions <- setdiff(seq_along(group_values), reference_position)
    rbind(
      rep(reference_position, length(comparison_positions)),
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

#' Validate a declared comparison family and its reference
#'
#' The three declarable families and the rule that a reference value belongs
#' to the reference family alone are shared by every provider that lets the
#' analyst choose the family.
#'
#' @param comparisons Family name.
#' @param reference Reference group value or `NULL`.
#'
#' @return `reference` as recorded in a specification: the value, or `NA`.
#' @noRd
check_comparison_family <- function(comparisons, reference) {
  check_choice(
    comparisons, "comparisons", c("pairwise", "reference", "consecutive")
  )
  if (comparisons == "reference") {
    if (
      !is.character(reference) || length(reference) != 1L ||
        is.na(reference) || !nzchar(reference)
    ) {
      bq_abort(
        "bq_error_invalid_analysis_function",
        "`reference` must be one non-empty group value for a reference family."
      )
    }
    return(reference)
  }
  if (!is.null(reference)) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`reference` must be NULL unless `comparisons = \"reference\"`."
    )
  }
  NA_character_
}

#' Validate a p-value adjustment method
#'
#' @param p_adjust Method name.
#'
#' @return `p_adjust`, invisibly.
#' @noRd
check_p_adjust <- function(p_adjust) {
  if (
    !is.character(p_adjust) || length(p_adjust) != 1L || is.na(p_adjust) ||
      !p_adjust %in% stats::p.adjust.methods
  ) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      paste0(
        "`p_adjust` must be one method supported by `stats::p.adjust()`: ",
        paste(stats::p.adjust.methods, collapse = ", "), "."
      )
    )
  }
  invisible(p_adjust)
}
