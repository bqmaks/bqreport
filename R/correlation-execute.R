# Correlation execution engines and builtin estimators.

execute_correlation <- function(spec, data) {
  data <- data[stratum_mask(
    data, spec$strata[[1]], spec$stratum_values[[1]]
  ), , drop = FALSE]
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
  adjustment <- if (k) {
    value <- vapply(adjustment_ids, function(id) {
      row <- match(id, registry$var_id)
      correlation_analysis_vector(data, spec, registry$name[[row]], id)[mask]
    }, numeric(length(x)))
    if (is.null(dim(value))) matrix(value, ncol = 1L) else value
  } else matrix(numeric(), nrow = length(x), ncol = 0L)
  context <- structure(list(
    analysis_id = spec$analysis_id[[1]], x = x, y = y,
    adjustment = adjustment, confidence_level = spec$confidence_level[[1]],
    estimand = spec$estimand[[1]], method = method,
    weights = if (is.na(spec$weight[[1]])) NULL else
      analysis_vector(data[[spec$weight[[1]]]])[mask],
    id = if (is.na(spec$correlation_subject[[1]])) NULL else
      analysis_vector(data[[spec$correlation_subject[[1]]]])[mask],
    stratum_label = spec$stratum_label[[1]], missing_policy = spec$missing_policy[[1]]
  ), class = "correlation_context")
  method_object <- spec$method_object[[1]]
  output <- method_object$compute(context)
  output <- validate_correlation_method_output(output, method_object)
  tibble::tibble(
    analysis_id = spec$analysis_id[[1]],
    correlation_family_id = spec$correlation_family_id[[1]],
    variable_x_id = spec$variable_x_id[[1]], variable_y_id = spec$variable_y_id[[1]],
    variable_x = spec$variable_x[[1]], variable_y = spec$variable_y[[1]],
    stratum_label = spec$stratum_label[[1]],
    strata = list(spec$strata[[1]]),
    correlation_interaction_id = spec$correlation_interaction_id[[1]],
    interaction_test = spec$interaction_test[[1]],
    correlation_comparator = list(spec$correlation_comparator[[1]]),
    correlation_comparator_id = spec$correlation_comparator_id[[1]],
    transformation_x = transformation_id_for(spec, spec$variable_x_id[[1]]),
    transformation_y = transformation_id_for(spec, spec$variable_y_id[[1]]),
    adjustment_variables = list(spec$adjustment_variables[[1]]),
    n_adjustment = as.integer(k), estimand = spec$estimand[[1]],
    estimate = output$estimate, std_error = output$std_error,
    std_error_scale = output$std_error_scale, conf_low = output$conf_low,
    conf_high = output$conf_high, statistic = output$statistic,
    df = output$df, p_value = output$p_value, p_adjusted = NA_real_,
    adjust_method = spec$adjust_method[[1]], effect_measure = spec$effect_measure[[1]],
    scale = spec$scale[[1]], n = as.integer(length(x)), method = method,
    ci_method = spec$ci_method[[1]], missing_policy = spec$missing_policy[[1]],
    confidence_level = spec$confidence_level[[1]],
    weight = spec$weight[[1]], id = spec$correlation_subject[[1]],
    effective_n = if (is.null(attr(output, "effective_n"))) as.numeric(length(x)) else
      attr(output, "effective_n"),
    n_subjects = if (is.null(attr(output, "n_subjects"))) NA_integer_ else
      attr(output, "n_subjects"),
    bootstrap_replicates = spec$bootstrap_replicates[[1]],
    permutation_replicates = spec$permutation_replicates[[1]],
    resampling_seed = spec$resampling_seed[[1]],
    bootstrap_successful = if (is.null(attr(output, "bootstrap_successful"))) 0L else
      attr(output, "bootstrap_successful"),
    permutation_successful = if (is.null(attr(output, "permutation_successful"))) 0L else
      attr(output, "permutation_successful")
  )
}

compute_weighted_pearson <- function(context) {
  weights <- context$weights
  total <- sum(weights)
  mean_x <- sum(weights * context$x) / total
  mean_y <- sum(weights * context$y) / total
  centered_x <- context$x - mean_x; centered_y <- context$y - mean_y
  estimate <- sum(weights * centered_x * centered_y) /
    sqrt(sum(weights * centered_x^2) * sum(weights * centered_y^2))
  effective_n <- total^2 / sum(weights^2)
  if (effective_n <= 3) {
    stop_invalid_correlation_output("Weighted correlation effective sample size must exceed 3.")
  }
  std_error <- 1 / sqrt(effective_n - 3)
  critical <- stats::qnorm((1 + context$confidence_level) / 2)
  ci <- tanh(atanh(estimate) + c(-1, 1) * critical * std_error)
  statistic <- estimate * sqrt((effective_n - 2) / (1 - estimate^2))
  output <- correlation_output(
    estimate, std_error, "fisher_z_effective_n", ci[[1]], ci[[2]], statistic,
    effective_n - 2, 2 * stats::pt(abs(statistic), effective_n - 2,
      lower.tail = FALSE)
  )
  attr(output, "effective_n") <- effective_n
  output
}

compute_repeated_measures_correlation <- function(context) {
  centered_x <- context$x - stats::ave(context$x, context$id, FUN = mean)
  centered_y <- context$y - stats::ave(context$y, context$id, FUN = mean)
  estimate <- stats::cor(centered_x, centered_y)
  n_subjects <- length(unique(context$id))
  df <- length(context$x) - n_subjects - 1L
  if (df <= 1L) stop_invalid_correlation_output(
    "Repeated-measures correlation has insufficient residual degrees of freedom."
  )
  statistic <- estimate * sqrt(df / (1 - estimate^2))
  std_error <- 1 / sqrt(df - 1)
  critical <- stats::qnorm((1 + context$confidence_level) / 2)
  ci <- tanh(atanh(estimate) + c(-1, 1) * critical * std_error)
  output <- correlation_output(
    estimate, std_error, "fisher_z_within_subject", ci[[1]], ci[[2]], statistic,
    df, 2 * stats::pt(abs(statistic), df, lower.tail = FALSE)
  )
  attr(output, "n_subjects") <- as.integer(n_subjects)
  output
}

compute_biweight_correlation <- function(context) {
  biweight_values <- function(value) {
    center <- stats::median(value)
    scale <- stats::mad(value, center = center, constant = 1)
    if (!is.finite(scale) || scale == 0) {
      stop_invalid_correlation_output("Biweight correlation requires positive MAD.")
    }
    u <- (value - center) / (9 * scale)
    weight <- (1 - u^2)^2
    weight[abs(u) >= 1] <- 0
    (value - center) * weight
  }
  x <- biweight_values(context$x); y <- biweight_values(context$y)
  estimate <- sum(x * y) / sqrt(sum(x^2) * sum(y^2))
  correlation_output(
    estimate, NA_real_, "biweight", NA_real_, NA_real_, NA_real_, NA_real_,
    NA_real_
  )
}

compute_polychoric_correlation <- function(context) {
  fit <- polycor::polychor(context$x, context$y, ML = TRUE, std.err = TRUE)
  estimate <- as.numeric(fit$rho)
  std_error <- sqrt(as.numeric(fit$var))
  critical <- stats::qnorm((1 + context$confidence_level) / 2)
  statistic <- estimate / std_error
  correlation_output(
    estimate, std_error, "latent_correlation",
    max(-1, estimate - critical * std_error),
    min(1, estimate + critical * std_error), statistic, NA_real_,
    2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  )
}

compute_builtin_correlation <- function(context, method) {
  x <- context$x; y <- context$y
  adjustment <- context$adjustment
  k <- ncol(adjustment)
  if (k) {
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
      conf.level = context$confidence_level
    ))
    estimate <- unname(test$estimate)
    statistic <- as.numeric(test$statistic)
    df <- if (is.null(test$parameter)) NA_real_ else as.numeric(test$parameter)
    p_value <- test$p.value
  }
  critical <- stats::qnorm((1 + context$confidence_level) / 2)
  if (method %in% c("pearson", "spearman")) {
    # Pearson: Fisher z. Spearman: Bonett-Wright (2000) standard error,
    # which widens the interval to account for the rank transformation.
    std_error <- if (method == "pearson") {
      1 / sqrt(length(x) - k - 3)
    } else {
      sqrt((1 + estimate^2 / 2) / (length(x) - k - 3))
    }
    z <- atanh(estimate)
    ci <- tanh(z + c(-1, 1) * critical * std_error)
    se_scale <- if (method == "pearson") "fisher_z" else
      "fisher_z_bonett_wright"
  } else {
    std_error <- if (is.finite(statistic) && statistic != 0) {
      abs(estimate / statistic)
    } else sqrt(2 * (2 * length(x) + 5) / (9 * length(x) * (length(x) - 1)))
    ci <- pmax(-1, pmin(1, estimate + c(-1, 1) * critical * std_error))
    se_scale <- "kendall_tau"
  }
  correlation_output(
    estimate, std_error, se_scale, ci[[1]], ci[[2]], statistic, df, p_value
  )
}

validate_correlation_method_output <- function(output, method) {
  if (!inherits(output, "correlation_method_output")) {
    stop_invalid_correlation_output(
      "A correlation method must return `correlation_output()`."
    )
  }
  finite_or_na <- function(value) is.na(value) || is.finite(value)
  if (!finite_or_na(output$estimate) || !finite_or_na(output$std_error) ||
      !finite_or_na(output$conf_low) || !finite_or_na(output$conf_high) ||
      !finite_or_na(output$p_value)) {
    stop_invalid_correlation_output(
      paste0("Correlation method `", method$id, "` returned non-finite output.")
    )
  }
  if (!is.na(output$p_value) && (output$p_value < 0 || output$p_value > 1)) {
    stop_invalid_correlation_output("Correlation p-value must lie in [0, 1].")
  }
  output
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
    stratum_label = character(), strata = list(),
    correlation_interaction_id = character(), interaction_test = logical(),
    correlation_comparator = list(), correlation_comparator_id = character(),
    transformation_x = character(), transformation_y = character(),
    adjustment_variables = list(), n_adjustment = integer(), estimand = character(),
    estimate = double(), std_error = double(), std_error_scale = character(),
    conf_low = double(), conf_high = double(), statistic = double(), df = double(),
    p_value = double(), p_adjusted = double(), adjust_method = character(),
    effect_measure = character(), scale = character(), n = integer(),
    method = character(), ci_method = character(), missing_policy = character(),
    confidence_level = double(), bootstrap_replicates = integer(),
    weight = character(), id = character(), effective_n = double(),
    n_subjects = integer(),
    permutation_replicates = integer(), resampling_seed = integer(),
    bootstrap_successful = integer(), permutation_successful = integer()
  )
}

