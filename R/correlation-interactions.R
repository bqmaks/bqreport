# Correlation interaction tests and comparator normalization.

compute_correlation_interactions <- function(correlation_output) {
  requested <- correlation_output$interaction_test
  requested[is.na(requested)] <- FALSE
  output <- correlation_output[requested, , drop = FALSE]
  if (!nrow(output)) {
    return(list(tests = tests_prototype(), contrasts = contrasts_prototype()))
  }
  families <- split(output, output$correlation_interaction_id)
  computed <- lapply(families, compute_correlation_interaction_family)
  list(
    tests = do.call(vctrs::vec_rbind, lapply(computed, `[[`, "tests")),
    contrasts = do.call(vctrs::vec_rbind, lapply(computed, `[[`, "contrasts"))
  )
}

compute_correlation_interaction_family <- function(rows) {
  if (nrow(rows) < 2L) {
    return(list(tests = tests_prototype(), contrasts = contrasts_prototype()))
  }
  comparator <- rows$correlation_comparator[[1]]
  context <- structure(list(
    estimates = rows, confidence_level = rows$confidence_level[[1]],
    adjust_method = rows$adjust_method[[1]],
    variable_x = rows$variable_x[[1]], variable_y = rows$variable_y[[1]],
    strata = rows$strata[[1]]
  ), class = "correlation_comparison_context")
  output <- comparator$compare(context)
  normalize_correlation_comparison_output(output, rows, comparator)
}

compute_fisher_z_comparison <- function(context) {
  rows <- context$estimates
  z <- atanh(rows$estimate)
  variance <- rows$std_error^2
  weights <- 1 / variance
  weighted_mean <- sum(weights * z) / sum(weights)
  statistic <- sum(weights * (z - weighted_mean)^2)
  df <- nrow(rows) - 1L
  pairs <- utils::combn(seq_len(nrow(rows)), 2L)
  critical <- stats::qnorm((1 + rows$confidence_level[[1]]) / 2)
  contrast_rows <- lapply(seq_len(ncol(pairs)), function(i) {
    numerator_i <- pairs[1L, i]
    denominator_i <- pairs[2L, i]
    estimate <- z[[numerator_i]] - z[[denominator_i]]
    std_error <- sqrt(variance[[numerator_i]] + variance[[denominator_i]])
    statistic <- estimate / std_error
    tibble::tibble(
      numerator = rows$stratum_label[[numerator_i]],
      denominator = rows$stratum_label[[denominator_i]],
      estimate = estimate,
      std_error = std_error, std_error_scale = "fisher_z",
      conf_low = estimate - critical * std_error,
      conf_high = estimate + critical * std_error,
      p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      effect_measure = "difference_in_fisher_z", scale = "fisher_z"
    )
  })
  contrast <- do.call(vctrs::vec_rbind, contrast_rows)
  correlation_comparison_output(
    statistic, df, stats::pchisq(statistic, df, lower.tail = FALSE), contrast
  )
}

normalize_correlation_comparison_output <- function(output, rows, comparator) {
  if (!inherits(output, "correlation_comparison_output")) {
    stop_invalid_correlation_output(
      "A correlation comparator must return `correlation_comparison_output()`."
    )
  }
  required <- c(
    "numerator", "denominator", "estimate", "std_error", "std_error_scale",
    "conf_low", "conf_high", "p_value", "effect_measure", "scale"
  )
  if (length(setdiff(required, names(output$contrasts)))) {
    stop_invalid_correlation_output("Correlation comparator contrasts are malformed.")
  }
  interaction_id <- rows$correlation_interaction_id[[1]]
  test <- tibble::tibble(
    analysis_id = interaction_id, outcome = rows$variable_x[[1]],
    predictor = rows$variable_y[[1]], contrast = NA_character_,
    numerator = NA_character_, denominator = NA_character_,
    test = "correlation_interaction", statistic = output$statistic,
    df = output$df, p_value = output$p_value, p_adjusted = NA_real_,
    adjust_method = "none", method = comparator$id
  )
  x <- output$contrasts
  contrasts <- tibble::tibble(
    analysis_id = interaction_id, outcome = rows$variable_x[[1]],
    predictor = rows$variable_y[[1]],
    contrast_id = vapply(seq_len(nrow(x)), function(i) bq_id(
      "contrast", interaction_id, x$numerator[[i]], x$denominator[[i]]
    ), character(1)),
    contrast = paste0(x$numerator, " - ", x$denominator),
    numerator = as.character(x$numerator), denominator = as.character(x$denominator),
    modifier = paste(rows$strata[[1]], collapse = " + "),
    modifier_level = NA_character_, inner_contrast = NA_character_,
    outer_contrast = NA_character_, estimand = "correlation_difference",
    exponentiated = FALSE, estimate = as.numeric(x$estimate),
    std_error = as.numeric(x$std_error),
    std_error_scale = as.character(x$std_error_scale),
    conf_low = as.numeric(x$conf_low), conf_high = as.numeric(x$conf_high),
    p_value = as.numeric(x$p_value), p_adjusted = NA_real_,
    adjust_method = rows$adjust_method[[1]],
    effect_measure = as.character(x$effect_measure), scale = as.character(x$scale)
  )
  contrasts$p_adjusted <- stats::p.adjust(
    contrasts$p_value, method = contrasts$adjust_method[[1]]
  )
  list(tests = test, contrasts = contrasts)
}
