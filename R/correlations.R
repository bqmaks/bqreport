#' Construct correlation methods
#' @return A concrete `correlation_method_spec`.
#' @export
pearson_correlation <- function() {
  new_correlation_method("pearson", "fisher_z")
}

#' @rdname pearson_correlation
#' @export
spearman_correlation <- function() {
  new_correlation_method("spearman", "fisher_z_approximation")
}

#' @rdname pearson_correlation
#' @export
kendall_correlation <- function() {
  new_correlation_method("kendall", "normal_approximation")
}

new_correlation_method <- function(id, ci_method) {
  structure(list(
    id = id, estimator = "correlation_coefficient", ci_method = ci_method,
    effect_measure = paste0(id, "_correlation"), scale = "minus_one_to_one",
    required_packages = "stats"
  ), class = "correlation_method_spec")
}

#' Compile a correlation analysis plan
#'
#' @param .data A `bq_data` object.
#' @param variables Numeric variables selected with tidyselect.
#' @param with Optional second variable set. If omitted, unique pairs within
#'   `variables` are compiled.
#' @param method A correlation method specification.
#' @param adjust_for Optional numeric covariates for partial correlation.
#' @param missing Pairwise or common complete-case analysis.
#' @param confidence_level Confidence level.
#' @param adjust Multiplicity adjustment accepted by [stats::p.adjust()].
#' @return An `analysis_plan` with one row per unique variable pair.
#' @export
plan_correlations <- function(
  .data, variables = where_continuous(), with = NULL,
  adjust_for = tidyselect::any_of(character()),
  method = pearson_correlation(), missing = c("pairwise", "complete"),
  confidence_level = 0.95, adjust = "none"
) {
  check_bq_data(.data)
  check_confidence_level(confidence_level)
  missing <- match.arg(missing)
  if (!inherits(method, "correlation_method_spec")) {
    stop_invalid_correlation("`method` must be a correlation method specification.")
  }
  if (!is.character(adjust) || length(adjust) != 1L ||
      !adjust %in% stats::p.adjust.methods) {
    stop_invalid_correlation("`adjust` is not supported.")
  }
  left <- names(tidyselect::eval_select(rlang::enquo(variables), .data))
  with_quo <- rlang::enquo(with)
  right <- if (rlang::quo_is_null(with_quo)) left else
    names(tidyselect::eval_select(with_quo, .data))
  adjustment_names <- names(tidyselect::eval_select(
    rlang::enquo(adjust_for), .data
  ))
  if (length(adjustment_names) && method$id == "kendall") {
    stop_invalid_correlation(
      "Partial Kendall correlation is not supported; choose Pearson or Spearman."
    )
  }
  overlap <- intersect(unique(c(left, right)), adjustment_names)
  if (length(overlap)) stop_invalid_correlation(paste0(
    "Adjustment variables must differ from correlated variables: ",
    paste(overlap, collapse = ", "), "."
  ))
  pairs <- if (identical(left, right) && rlang::quo_is_null(with_quo)) {
    if (length(left) < 2L) matrix(character(), nrow = 2L) else utils::combn(left, 2L)
  } else {
    grid <- expand.grid(x = left, y = right, stringsAsFactors = FALSE)
    grid <- grid[grid$x != grid$y, , drop = FALSE]
    keys <- vapply(seq_len(nrow(grid)), function(i) {
      paste(sort(c(grid$x[[i]], grid$y[[i]])), collapse = "\r")
    }, character(1))
    grid <- grid[!duplicated(keys), , drop = FALSE]
    rbind(grid$x, grid$y)
  }
  if (ncol(pairs) == 0L) return(empty_analysis_plan())
  registry <- tibble::as_tibble(
    attr(.data, "variable_registry", exact = TRUE)
  )
  selected_names <- unique(c(left, right, adjustment_names))
  selected_ids <- registry$var_id[match(selected_names, registry$name)]
  adjustment_ids <- registry$var_id[match(adjustment_names, registry$name)]
  family_id <- paste0("correlation_family_", uuid::UUIDgenerate())
  rows <- lapply(seq_len(ncol(pairs)), function(i) {
    x_spec <- registry[match(pairs[1, i], registry$name), , drop = FALSE]
    y_spec <- registry[match(pairs[2, i], registry$name), , drop = FALSE]
    row <- analysis_plan_row(
      x_spec, y_spec, method = NULL, status = "ready", reason = NA_character_,
      confidence_level = confidence_level
    )
    row$analysis_type <- "correlation"
    row$variable_x_id <- x_spec$var_id[[1]]
    row$variable_y_id <- y_spec$var_id[[1]]
    row$variable_x <- x_spec$name[[1]]
    row$variable_y <- y_spec$name[[1]]
    transformation_ids <- unique(c(
      x_spec$var_id[[1]], y_spec$var_id[[1]], adjustment_ids
    ))
    transformations <- lapply(transformation_ids, function(id) {
      registry$transformation[[match(id, registry$var_id)]]
    })
    names(transformations) <- transformation_ids
    row$transformation_specs <- list(transformations)
    row$correlation_family_id <- family_id
    row$correlation_variable_ids <- list(selected_ids)
    row$adjustment_ids <- list(adjustment_ids)
    row$adjustment_variables <- list(adjustment_names)
    row$estimand <- if (length(adjustment_ids)) "partial_correlation" else "correlation"
    row$method <- method$id
    row$engine <- "stats_cor_test"
    row$estimator <- method$estimator
    row$ci_method <- method$ci_method
    row$effect_measure <- method$effect_measure
    row$model_scale <- method$scale
    row$scale <- method$scale
    row$missing_policy <- missing
    row$adjust_method <- adjust
    row$required_packages <- list(method$required_packages)
    row$method_object <- list(method)
    row$formula <- list(NULL)
    row$validated <- FALSE
    row
  })
  new_analysis_plan(vctrs::vec_rbind(!!!rows))
}

validate_correlation_task <- function(plan, i, data, registry) {
  plan$validated[[i]] <- TRUE
  x_row <- match(plan$variable_x_id[[i]], registry$var_id)
  y_row <- match(plan$variable_y_id[[i]], registry$var_id)
  issues <- character()
  if (is.na(x_row) || is.na(y_row)) {
    issues <- c(issues, "A correlation variable referenced by stable id is absent.")
  } else {
    plan$variable_x[[i]] <- registry$name[[x_row]]
    plan$variable_y[[i]] <- registry$name[[y_row]]
    x <- correlation_analysis_vector(data, plan[i, , drop = FALSE],
      registry$name[[x_row]], plan$variable_x_id[[i]])
    y <- correlation_analysis_vector(data, plan[i, , drop = FALSE],
      registry$name[[y_row]], plan$variable_y_id[[i]])
    if (inherits(x, "error")) issues <- c(issues, conditionMessage(x))
    if (inherits(y, "error")) issues <- c(issues, conditionMessage(y))
    if (!inherits(x, "error") && !inherits(y, "error")) {
      adjustment_values <- lapply(plan$adjustment_ids[[i]], function(id) {
        row <- match(id, registry$var_id)
        if (is.na(row)) return(simpleError(
          "An adjustment variable referenced by stable id is absent."
        ))
        correlation_analysis_vector(
          data, plan[i, , drop = FALSE], registry$name[[row]], id
        )
      })
      adjustment_errors <- vapply(adjustment_values, inherits, logical(1), "error")
      if (any(adjustment_errors)) {
        issues <- c(issues, vapply(
          adjustment_values[adjustment_errors], conditionMessage, character(1)
        ))
      }
      mask <- correlation_complete_mask(plan[i, , drop = FALSE], data, x, y)
      n <- sum(mask)
      plan$n_total[[i]] <- nrow(data)
      plan$n_eligible[[i]] <- nrow(data)
      plan$n_analyzed[[i]] <- n
      plan$n_missing_outcome[[i]] <- sum(is.na(x))
      plan$n_missing_predictor[[i]] <- sum(is.na(y))
      minimum <- if (plan$method[[i]] %in% c("pearson", "spearman")) 4L else 3L
      if (length(plan$adjustment_ids[[i]])) {
        minimum <- max(minimum, length(plan$adjustment_ids[[i]]) + 4L)
      }
      if (n < minimum) issues <- c(issues, paste0(
        "Correlation requires at least ", minimum, " complete observations."
      ))
      if (n > 0L && (n_distinct_values(x[mask]) < 2L ||
          n_distinct_values(y[mask]) < 2L)) {
        issues <- c(issues, "A correlation variable has no variation.")
      }
      if (length(plan$adjustment_ids[[i]]) &&
          n - length(plan$adjustment_ids[[i]]) - 2L <= 0L) {
        issues <- c(issues, "Partial correlation has no residual degrees of freedom.")
      }
      if (length(adjustment_values) && !any(adjustment_errors) && n > 0L) {
        adjustment_matrix <- do.call(cbind, lapply(adjustment_values, `[`, mask))
        if (qr(cbind(1, adjustment_matrix))$rank < ncol(adjustment_matrix) + 1L) {
          issues <- c(issues, "Adjustment variables are linearly dependent.")
        }
      }
    }
  }
  if (length(issues)) {
    plan$status[[i]] <- "invalid"
    plan$reason[[i]] <- append_reasons(plan$reason[[i]], issues)
  }
  plan
}

correlation_analysis_vector <- function(data, spec, name, id) {
  tryCatch({
    original <- data[[name]]
    value <- analysis_vector(original)
    value[special_missing_mask(original)] <- NA
    if (!is.numeric(value)) stop_invalid_correlation(paste0(
      "Correlation variable `", name, "` must be numeric."
    ))
    apply_transformation_spec(
      value, spec$transformation_specs[[1]][[id]], name,
      spec$analysis_id[[1]]
    )
  }, error = function(condition) condition)
}

correlation_complete_mask <- function(spec, data, x, y) {
  if (spec$missing_policy[[1]] == "pairwise") {
    ids <- unique(c(
      spec$variable_x_id[[1]], spec$variable_y_id[[1]],
      spec$adjustment_ids[[1]]
    ))
  } else ids <- spec$correlation_variable_ids[[1]]
  registry <- variables(data)
  mask <- rep(TRUE, nrow(data))
  for (id in ids) {
    row <- match(id, registry$var_id)
    if (is.na(row)) return(rep(FALSE, nrow(data)))
    value <- correlation_analysis_vector(
      data, spec, registry$name[[row]], id
    )
    if (inherits(value, "error")) return(rep(FALSE, nrow(data)))
    mask <- mask & !is.na(value)
  }
  mask
}

execute_correlation <- function(spec, data) {
  registry <- variables(data)
  x <- correlation_analysis_vector(
    data, spec, spec$variable_x[[1]], spec$variable_x_id[[1]]
  )
  y <- correlation_analysis_vector(
    data, spec, spec$variable_y[[1]], spec$variable_y_id[[1]]
  )
  mask <- correlation_complete_mask(spec, data, x, y)
  x <- x[mask]; y <- y[mask]
  method <- spec$method[[1]]
  adjustment_ids <- spec$adjustment_ids[[1]]
  k <- length(adjustment_ids)
  if (k) {
    adjustment <- vapply(adjustment_ids, function(id) {
      row <- match(id, registry$var_id)
      correlation_analysis_vector(data, spec, registry$name[[row]], id)[mask]
    }, numeric(length(x)))
    if (is.null(dim(adjustment))) adjustment <- matrix(adjustment, ncol = 1L)
    if (method == "spearman") {
      x <- rank(x); y <- rank(y)
      adjustment <- apply(adjustment, 2L, rank)
      if (is.null(dim(adjustment))) adjustment <- matrix(adjustment, ncol = 1L)
    }
    x <- stats::residuals(stats::lm.fit(cbind(1, adjustment), x))
    y <- stats::residuals(stats::lm.fit(cbind(1, adjustment), y))
    estimate <- stats::cor(x, y)
    df <- length(x) - k - 2L
    statistic <- estimate * sqrt(df / (1 - estimate^2))
    p_value <- 2 * stats::pt(abs(statistic), df, lower.tail = FALSE)
  } else {
    test <- suppressWarnings(stats::cor.test(
      x, y, method = method, exact = FALSE,
      conf.level = spec$confidence_level[[1]]
    ))
    estimate <- unname(test$estimate)
    statistic <- as.numeric(test$statistic)
    df <- if (is.null(test$parameter)) NA_real_ else as.numeric(test$parameter)
    p_value <- test$p.value
  }
  critical <- stats::qnorm((1 + spec$confidence_level[[1]]) / 2)
  if (method == "pearson") {
    std_error <- 1 / sqrt(length(x) - k - 3)
    z <- atanh(estimate)
    ci <- tanh(z + c(-1, 1) * critical * std_error)
    se_scale <- "fisher_z"
  } else if (method == "spearman") {
    std_error <- 1 / sqrt(length(x) - k - 3)
    z <- atanh(estimate)
    ci <- tanh(z + c(-1, 1) * critical * std_error)
    se_scale <- "fisher_z_approximation"
  } else {
    std_error <- if (is.finite(statistic) && statistic != 0) {
      abs(estimate / statistic)
    } else sqrt(2 * (2 * length(x) + 5) / (9 * length(x) * (length(x) - 1)))
    ci <- pmax(-1, pmin(1, estimate + c(-1, 1) * critical * std_error))
    se_scale <- "kendall_tau"
  }
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    correlation_family_id = spec$correlation_family_id[[1]],
    variable_x_id = spec$variable_x_id[[1]], variable_y_id = spec$variable_y_id[[1]],
    variable_x = spec$variable_x[[1]], variable_y = spec$variable_y[[1]],
    transformation_x = transformation_id_for(spec, spec$variable_x_id[[1]]),
    transformation_y = transformation_id_for(spec, spec$variable_y_id[[1]]),
    adjustment_variables = list(spec$adjustment_variables[[1]]),
    n_adjustment = as.integer(k), estimand = spec$estimand[[1]],
    estimate = as.numeric(estimate), std_error = as.numeric(std_error),
    std_error_scale = se_scale, conf_low = as.numeric(ci[[1]]),
    conf_high = as.numeric(ci[[2]]), statistic = as.numeric(statistic),
    df = as.numeric(df), p_value = p_value, p_adjusted = NA_real_,
    adjust_method = spec$adjust_method[[1]], effect_measure = spec$effect_measure[[1]],
    scale = spec$scale[[1]], n = as.integer(length(x)), method = method,
    ci_method = spec$ci_method[[1]], missing_policy = spec$missing_policy[[1]]
  )
}

transformation_id_for <- function(spec, id) {
  transformation <- spec$transformation_specs[[1]][[id]]
  if (is.null(transformation)) NA_character_ else transformation$id
}

correlations_prototype <- function() {
  tibble::tibble(
    analysis_id = character(), correlation_family_id = character(),
    variable_x_id = character(), variable_y_id = character(),
    variable_x = character(), variable_y = character(),
    transformation_x = character(), transformation_y = character(),
    adjustment_variables = list(), n_adjustment = integer(), estimand = character(),
    estimate = double(), std_error = double(), std_error_scale = character(),
    conf_low = double(), conf_high = double(), statistic = double(), df = double(),
    p_value = double(), p_adjusted = double(), adjust_method = character(),
    effect_measure = character(), scale = character(), n = integer(),
    method = character(), ci_method = character(), missing_policy = character()
  )
}

stop_invalid_correlation <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_correlation", "error", "condition")))
}
