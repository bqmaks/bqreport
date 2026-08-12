#' Access registered or computed contrasts
#' @param x A `bq_data` or `analysis_result`.
#' @return A tidy tibble.
#' @export
contrasts <- function(x) UseMethod("contrasts")

#' @export
contrasts.bq_data <- function(x) {
  check_bq_data(x)
  tibble::as_tibble(attr(x, "contrast_registry", exact = TRUE))
}

#' Configure model coding
#' @param .data A `bq_data` object.
#' @param .cols Categorical predictors selected with tidyselect.
#' @param coding Currently only treatment coding is supported.
#' @param reference Reference value.
#' @return Updated `bq_data`.
#' @export
set_coding <- function(.data, .cols, coding = "treatment", reference) {
  check_bq_data(.data)
  if (!identical(coding, "treatment")) {
    stop_comparison("Only `treatment` coding is supported.", "bq_error_invalid_coding")
  }
  check_scalar_setting(reference, "reference", "bq_error_invalid_coding")
  selected <- names(tidyselect::eval_select(rlang::enquo(.cols), .data))
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected, registry$name)
  if (any(!registry$type[rows] %in% c("binary", "ordinal", "nominal"))) {
    stop_comparison("Coding requires categorical variables.", "bq_error_invalid_coding")
  }
  registry$coding[rows] <- coding
  for (row in rows) registry$reference[[row]] <- reference
  attr(.data, "variable_registry") <- registry
  .data
}

#' Compare levels against a reference
#' @param reference Reference value used as denominator.
#' @return A backend-independent `contrast_spec`.
#' @export
against_reference <- function(reference) {
  check_scalar_setting(reference, "reference", "bq_error_invalid_comparison")
  structure(list(type = "against_reference", reference = reference), class = "contrast_spec")
}

#' Register target comparisons
#' @param .data A `bq_data` object.
#' @param .cols Predictor columns selected with tidyselect.
#' @param comparisons A `contrast_spec`.
#' @param adjust A method accepted by [stats::p.adjust()].
#' @return Updated `bq_data`.
#' @export
set_comparisons <- function(.data, .cols, comparisons, adjust = "none") {
  check_bq_data(.data)
  if (!inherits(comparisons, "contrast_spec")) {
    stop_comparison("`comparisons` must be a contrast_spec.", "bq_error_invalid_comparison")
  }
  if (!is.character(adjust) || length(adjust) != 1L || !adjust %in% stats::p.adjust.methods) {
    stop_comparison("`adjust` is not supported.", "bq_error_invalid_comparison")
  }
  selected <- names(tidyselect::eval_select(rlang::enquo(.cols), .data))
  registry <- variables(.data)
  rows <- match(selected, registry$name)
  additions <- lapply(rows, function(row) tibble::tibble(
    contrast_id = paste0("contrast_", uuid::UUIDgenerate()),
    predictor_id = registry$var_id[[row]], predictor = registry$name[[row]],
    comparison_type = comparisons$type, reference = list(comparisons$reference),
    adjust_method = adjust
  ))
  current <- attr(.data, "contrast_registry", exact = TRUE)
  attr(.data, "contrast_registry") <- vctrs::vec_rbind(current, !!!additions)
  .data
}

stop_comparison <- function(message, class) {
  stop(structure(list(message = message, call = sys.call(-1L)), class = c(class, "error", "condition")))
}

compute_builtin_contrasts <- function(estimate_data, spec, data) {
  registry <- contrasts(data)
  if (nrow(registry) == 0L) return(contrasts_prototype())
  requested <- spec$contrast_ids[[1]]
  if (length(requested) == 0L) return(contrasts_prototype())
  registered <- registry[registry$contrast_id %in% requested, , drop = FALSE]
  if (nrow(registered) == 0L) return(contrasts_prototype())
  rows <- estimate_data$term == spec$predictor[[1]] & !is.na(estimate_data$level)
  if (!any(rows)) return(contrasts_prototype())
  outputs <- lapply(seq_len(nrow(registered)), function(i) {
    comparison <- registered[i, , drop = FALSE]
    reference <- as.character(comparison$reference[[1]])
    tibble::tibble(
      analysis_id = spec$analysis_id[[1]], outcome = spec$outcome[[1]],
      predictor = spec$predictor[[1]], contrast_id = comparison$contrast_id[[1]],
      contrast = paste0(estimate_data$level[rows], " - ", reference),
      numerator = estimate_data$level[rows], denominator = reference,
      estimate = estimate_data$estimate[rows], conf_low = estimate_data$conf_low[rows],
      conf_high = estimate_data$conf_high[rows], p_value = estimate_data$p_value[rows],
      p_adjusted = stats::p.adjust(estimate_data$p_value[rows], method = comparison$adjust_method[[1]]),
      adjust_method = comparison$adjust_method[[1]],
      effect_measure = estimate_data$effect_measure[rows], scale = estimate_data$scale[rows]
    )
  })
  vctrs::vec_rbind(!!!outputs)
}
