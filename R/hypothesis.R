#' Resolve a hypothesis declaration into margins and a benefit direction
#'
#' Two-group tests share one vocabulary of hypotheses: `"two_sided"`,
#' `"equivalence"`, `"noninferiority"` and `"superiority"`. The rules that tie
#' `margin` and `benefit` to that choice are the same for every test, so they
#' are resolved here once. A test that measures its estimand on a bounded
#' scale passes `max_margin` to keep the resulting bounds inside that scale.
#'
#' Margins are recorded as `margin_lower` / `margin_upper` in the units of the
#' estimand: an equivalence margin becomes a symmetric or explicitly named
#' pair of bounds, a noninferiority margin the lower bound `-margin`, and a
#' superiority margin the lower bound `margin`. Missing bounds are `NA`.
#'
#' @param hypothesis Hypothesis type.
#' @param margin Margin as accepted by the public constructors.
#' @param benefit `"higher"`, `"lower"` or `NULL`.
#' @param max_margin Exclusive upper bound for the absolute size of any
#'   margin; `Inf` for an unbounded estimand.
#'
#' @return A list with `hypothesis`, `margin_lower`, `margin_upper` and
#'   `benefit` (`NA` when not applicable).
#' @noRd
resolve_hypothesis <- function(hypothesis, margin, benefit, max_margin = Inf) {
  class <- "bq_error_invalid_analysis_function"
  check_choice(
    hypothesis, "hypothesis",
    c("two_sided", "equivalence", "noninferiority", "superiority")
  )
  bounded <- is.finite(max_margin)
  within <- function(value) !bounded || abs(value) < max_margin
  size_note <- if (bounded) {
    paste0(" smaller than ", format(max_margin))
  } else {
    ""
  }

  margin_lower <- NA_real_
  margin_upper <- NA_real_
  if (hypothesis == "two_sided") {
    if (!is.null(margin)) {
      bq_abort(class, "`margin` must be NULL for a two-sided test.")
    }
  } else if (hypothesis == "equivalence") {
    scalar <- is.numeric(margin) && length(margin) == 1L && !is.na(margin) &&
      is.finite(margin) && margin > 0 && within(margin)
    bounds <- is.numeric(margin) && length(margin) == 2L &&
      identical(names(margin), c("lower", "upper")) &&
      !anyNA(margin) && all(is.finite(margin)) &&
      margin[["lower"]] < 0 && margin[["upper"]] > 0 &&
      all(vapply(margin, within, logical(1)))
    if (!scalar && !bounds) {
      bq_abort(
        class,
        paste0(
          "Equivalence `margin` must be one positive number", size_note,
          " or a named `c(lower, upper)` vector with `lower < 0 < upper`",
          size_note, "."
        )
      )
    }
    margin_lower <- if (scalar) -margin else margin[["lower"]]
    margin_upper <- if (scalar) margin else margin[["upper"]]
    margin_lower <- as.double(margin_lower)
    margin_upper <- as.double(margin_upper)
  } else {
    positive_required <- hypothesis == "noninferiority"
    valid_margin <- is.numeric(margin) && length(margin) == 1L &&
      !is.na(margin) && is.finite(margin) && within(margin) &&
      if (positive_required) margin > 0 else margin >= 0
    if (!valid_margin) {
      bq_abort(
        class,
        paste0(
          if (positive_required) "Noninferiority" else "Superiority",
          " `margin` must be one ",
          if (positive_required) "positive" else "non-negative",
          " finite number", size_note, "."
        )
      )
    }
    margin_lower <- as.double(if (positive_required) -margin else margin)
  }

  if (hypothesis %in% c("noninferiority", "superiority")) {
    if (
      !is.character(benefit) || length(benefit) != 1L || is.na(benefit) ||
        !benefit %in% c("higher", "lower")
    ) {
      bq_abort(
        class,
        paste0(
          "`benefit` must be \"higher\" or \"lower\" for ", hypothesis, "."
        )
      )
    }
  } else if (!is.null(benefit)) {
    bq_abort(
      class, "`benefit` must be NULL for two-sided and equivalence tests."
    )
  }

  list(
    hypothesis = hypothesis,
    margin_lower = margin_lower,
    margin_upper = margin_upper,
    benefit = if (is.null(benefit)) NA_character_ else benefit
  )
}

#' Require a confidence level compatible with the hypothesis
#'
#' An equivalence test reads its two one-sided tests off a `2 * alpha`
#' interval, which is only meaningful above 0.5.
#'
#' @param conf_level Confidence level.
#' @param hypothesis Resolved hypothesis type.
#'
#' @return `conf_level` as a double, invisibly.
#' @noRd
check_conf_level <- function(conf_level, hypothesis = "two_sided") {
  conf_level <- check_number(
    conf_level, "conf_level", lower = 0, upper = 1, inclusive = FALSE
  )
  if (hypothesis == "equivalence" && conf_level <= 0.5) {
    bq_abort(
      "bq_error_invalid_analysis_function",
      "`conf_level` must be greater than 0.5 for an equivalence test."
    )
  }
  invisible(conf_level)
}
