#' Construct built-in descriptive group comparisons
#'
#' These specifications declare the estimand and scale before execution. They
#' support exactly two groups in the initial descriptive comparison slice.
#'
#' @param ci_method Confidence interval implementation identifier.
#'
#' @return A `group_comparison_spec`.
#' @export
mean_difference <- function(ci_method = "welch_t") {
  new_group_comparison_spec(
    "welch_mean_difference", c("continuous", "count"),
    "mean_difference", "identity", ci_method
  )
}

#' @rdname mean_difference
#' @export
standardized_mean_difference <- function(ci_method = "large_sample") {
  new_group_comparison_spec(
    "hedges_g", c("continuous", "count"),
    "standardized_mean_difference", "standard_deviation", ci_method
  )
}

#' @rdname mean_difference
#' @export
risk_difference <- function(ci_method = "wald") {
  new_group_comparison_spec(
    "wald_risk_difference", "binary",
    "risk_difference", "probability_difference", ci_method
  )
}

#' @rdname mean_difference
#' @export
risk_ratio <- function(ci_method = "log_wald") {
  new_group_comparison_spec(
    "log_wald_risk_ratio", "binary", "risk_ratio", "ratio", ci_method,
    requires_positive_cells = TRUE
  )
}

#' @rdname mean_difference
#' @export
odds_ratio <- function(ci_method = "log_wald") {
  new_group_comparison_spec(
    "log_wald_odds_ratio", "binary", "odds_ratio", "ratio", ci_method,
    requires_positive_cells = TRUE
  )
}

#' Construct a Student or Welch two-sample t method
#'
#' @param var_equal Whether to use a pooled-variance Student t procedure.
#'   This decision must be supplied explicitly.
#' @return A `two_group_method_spec`.
#' @export
t_test <- function(var_equal) {
  if (missing(var_equal) || !is.logical(var_equal) ||
      length(var_equal) != 1L || is.na(var_equal)) {
    stop_group_comparison_contract(
      "`var_equal` must be explicitly supplied as TRUE or FALSE."
    )
  }
  structure(list(
    id = if (var_equal) "student_t" else "welch_t",
    var_equal = var_equal, required_packages = "stats"
  ), class = "two_group_method_spec")
}

#' Construct a Brunner-Munzel two-group method
#'
#' @param backend An explicit backend specification from
#'   [brunnermunzel_backend()] or [lawstat_backend()].
#' @return A `two_group_method_spec`.
#' @export
brunner_munzel <- function(backend) {
  if (missing(backend) || !inherits(backend, "brunner_munzel_backend_spec")) {
    stop_group_comparison_contract(
      "`backend` must be an explicit Brunner-Munzel backend specification."
    )
  }
  structure(list(
    id = "brunner_munzel", backend = backend,
    required_packages = backend$required_packages
  ), class = "two_group_method_spec")
}

#' Construct explicit Brunner-Munzel backends
#'
#' @param permutation Whether to use the permutation implementation. This is
#'   explicit because it changes inference and computational cost.
#' @return A `brunner_munzel_backend_spec`.
#' @export
brunnermunzel_backend <- function(permutation) {
  if (missing(permutation) || !is.logical(permutation) ||
      length(permutation) != 1L || is.na(permutation)) {
    stop_group_comparison_contract(
      "`permutation` must be explicitly supplied as TRUE or FALSE."
    )
  }
  structure(list(
    id = "brunnermunzel", permutation = permutation,
    required_packages = "brunnermunzel"
  ), class = "brunner_munzel_backend_spec")
}

#' @rdname brunnermunzel_backend
#' @export
lawstat_backend <- function() {
  structure(list(
    id = "lawstat", permutation = FALSE, required_packages = "lawstat"
  ), class = "brunner_munzel_backend_spec")
}

#' Construct Brunner-Munzel estimands
#'
#' `probability_of_superiority()` is oriented as the probability that the
#' numerator group has a larger value than the denominator group, plus half
#' the tie probability. `relative_effect()` maps it to `2 * p - 1`.
#'
#' @return A `group_comparison_spec` used as an estimand specification.
#' @export
probability_of_superiority <- function() {
  new_group_comparison_spec(
    "probability_of_superiority", c("continuous", "count", "ordinal"),
    "probability_of_superiority", "probability", "brunner_munzel"
  )
}

#' @rdname probability_of_superiority
#' @export
relative_effect <- function() {
  new_group_comparison_spec(
    "relative_effect", c("continuous", "count", "ordinal"),
    "relative_effect", "minus_one_to_one", "brunner_munzel"
  )
}

#' Construct a categorical group analysis from independent components
#'
#' The omnibus test and effect-size estimator consume the same contingency
#' table but remain statistically independent specifications.
#'
#' @param omnibus An `omnibus_spec`, such as [fisher_exact_test()].
#' @param effect_size An `effect_size_spec`, such as [cramers_v()].
#' @param pairwise An explicit post-hoc policy. Pairwise categorical tests are
#'   not yet implemented, so use [no_post_hoc()].
#' @return A `group_comparison_spec`.
#' @export
categorical_group_analysis <- function(omnibus, effect_size, pairwise) {
  if (missing(omnibus) || !inherits(omnibus, "omnibus_spec")) {
    stop_group_comparison_contract("`omnibus` must be an explicit omnibus_spec.")
  }
  if (missing(effect_size) || !inherits(effect_size, "effect_size_spec")) {
    stop_group_comparison_contract("`effect_size` must be an explicit effect_size_spec.")
  }
  if (missing(pairwise) || !inherits(pairwise, "post_hoc_spec") ||
      pairwise$type != "none") {
    stop_group_comparison_contract(
      "`pairwise` must currently be explicitly supplied as no_post_hoc()."
    )
  }
  spec <- new_group_comparison_spec(
    paste(omnibus$id, effect_size$id, sep = "_with_"),
    c("binary", "ordinal", "nominal"), effect_size$id,
    effect_size$scale, effect_size$ci_method,
    required_packages = unique(c(
      omnibus$required_packages, effect_size$required_packages
    ))
  )
  spec$omnibus <- omnibus
  spec$omnibus_method <- omnibus$id
  spec$effect_size <- effect_size
  spec$post_hoc <- pairwise
  spec$posthoc_method <- "none"
  spec
}

#' Construct a Fisher exact omnibus test
#'
#' @param simulate_p_value Whether to estimate the p-value by Monte Carlo.
#' @param replicates Number of Monte Carlo replicates. Must be explicitly
#'   supplied when `simulate_p_value = TRUE`.
#' @return An `omnibus_spec`.
#' @export
fisher_exact_test <- function(simulate_p_value, replicates = NULL) {
  if (missing(simulate_p_value) || !is.logical(simulate_p_value) ||
      length(simulate_p_value) != 1L || is.na(simulate_p_value)) {
    stop_group_comparison_contract(
      "`simulate_p_value` must be explicitly supplied as TRUE or FALSE."
    )
  }
  if (simulate_p_value && (is.null(replicates) || !is.numeric(replicates) ||
      length(replicates) != 1L || is.na(replicates) || replicates < 1 ||
      replicates != as.integer(replicates))) {
    stop_group_comparison_contract(
      "`replicates` must be an explicit positive integer for Monte Carlo Fisher inference."
    )
  }
  if (!simulate_p_value && !is.null(replicates)) {
    stop_group_comparison_contract(
      "`replicates` is only meaningful when `simulate_p_value = TRUE`."
    )
  }
  structure(list(
    type = "test", id = "fisher_exact", simulate_p_value = simulate_p_value,
    replicates = if (is.null(replicates)) NA_integer_ else as.integer(replicates),
    required_packages = "stats"
  ), class = c("omnibus_spec", "model_postprocessing_step"))
}

#' Construct Cramer's V effect size
#'
#' @param ci_method Explicit confidence-interval method. Currently
#'   `noncentral_chi_squared` is supported.
#' @return An `effect_size_spec`.
#' @export
cramers_v <- function(ci_method) {
  if (missing(ci_method) || !identical(ci_method, "noncentral_chi_squared")) {
    stop_group_comparison_contract(
      "`ci_method` must currently be explicitly `noncentral_chi_squared`."
    )
  }
  structure(list(
    id = "cramers_v", scale = "zero_to_one", ci_method = ci_method,
    statistic_source = "pearson_chi_squared", required_packages = "stats"
  ), class = c("effect_size_spec", "model_postprocessing_step"))
}

#' Construct independent inference for exactly two groups
#'
#' The method, estimand, contrast orientation, hypothesis, and multiplicity
#' policy are independent mandatory decisions. No omnibus test is computed.
#'
#' @param method A `two_group_method_spec`, such as [t_test()].
#' @param estimand A supported effect specification, currently
#'   [mean_difference()].
#' @param contrast An explicit [against_reference()] contrast.
#' @param hypothesis A `hypothesis_spec`.
#' @param multiplicity A `multiplicity_spec`.
#' @return A `group_comparison_spec`.
#' @export
two_group_comparison <- function(
  method, estimand, contrast, hypothesis, multiplicity
) {
  if (missing(method) || !inherits(method, "two_group_method_spec")) {
    stop_group_comparison_contract("`method` must be a two_group_method_spec.")
  }
  supported_estimands <- if (identical(method$id, "brunner_munzel")) {
    c("probability_of_superiority", "relative_effect")
  } else {
    "mean_difference"
  }
  if (missing(estimand) || !inherits(estimand, "group_comparison_spec") ||
      !estimand$effect_measure %in% supported_estimands) {
    stop_group_comparison_contract(
      "`estimand` must be an explicitly supported estimand specification."
    )
  }
  if (missing(contrast) || !inherits(contrast, "contrast_spec") ||
      contrast$type != "against_reference") {
    stop_group_comparison_contract(
      "`contrast` must explicitly define the denominator with against_reference()."
    )
  }
  if (missing(hypothesis) || !inherits(hypothesis, "hypothesis_spec")) {
    stop_group_comparison_contract("`hypothesis` must be a hypothesis_spec.")
  }
  if (missing(multiplicity) || !inherits(multiplicity, "multiplicity_spec")) {
    stop_group_comparison_contract("`multiplicity` must be a multiplicity_spec.")
  }
  comparison_id <- if (identical(method$id, "brunner_munzel")) {
    "two_group_brunner_munzel"
  } else "two_group_t_test"
  ci_method <- if (identical(method$id, "brunner_munzel")) {
    paste(method$backend$id, if (method$backend$permutation) "permutation" else "asymptotic", sep = "_")
  } else if (method$var_equal) "student_t" else "welch_t"
  spec <- new_group_comparison_spec(
    comparison_id, estimand$types, estimand$effect_measure,
    estimand$scale, ci_method,
    required_packages = method$required_packages
  )
  spec$method <- method
  spec$estimand <- estimand
  spec$contrast <- contrast
  spec$hypothesis <- hypothesis
  spec$multiplicity <- multiplicity
  spec$omnibus_method <- "none"
  spec$posthoc_method <- if (identical(method$id, "brunner_munzel")) {
    "two_group_brunner_munzel"
  } else "two_group_t"
  spec
}

#' Construct explicit omnibus and post-hoc group tests
#'
#' These specifications keep the omnibus hypothesis and post-hoc procedure
#' fixed in the analysis plan. They are used through the `comparisons`
#' argument of [plan_descriptives()].
#'
#' @param post_hoc Explicit post-hoc policy created by [pairwise_post_hoc()]
#'   or [no_post_hoc()].
#'
#' @return A `group_comparison_spec`.
#' @export
one_way_anova <- function(post_hoc) {
  post_hoc <- validate_omnibus_post_hoc(post_hoc, "tukey")
  spec <- new_group_comparison_spec(
    "one_way_anova", c("continuous", "count"), "mean_difference",
    "identity", "tukey_hsd"
  )
  spec$omnibus_method <- "one_way_anova"
  spec$post_hoc <- post_hoc
  spec$posthoc_method <- post_hoc_method_id(post_hoc)
  spec$required_packages <- unique(c(spec$required_packages, post_hoc_packages(post_hoc)))
  spec
}

#' @rdname one_way_anova
#' @export
welch_anova <- function(post_hoc) {
  post_hoc <- validate_omnibus_post_hoc(post_hoc, "games_howell")
  spec <- new_group_comparison_spec(
    "welch_anova", c("continuous", "count"), "mean_difference",
    "identity", "games_howell",
    required_packages = post_hoc_packages(post_hoc)
  )
  spec$omnibus_method <- "welch_anova"
  spec$post_hoc <- post_hoc
  spec$posthoc_method <- post_hoc_method_id(post_hoc)
  spec
}

#' @rdname one_way_anova
#' @export
kruskal_wallis <- function(post_hoc) {
  post_hoc <- validate_omnibus_post_hoc(post_hoc, "dunn")
  spec <- new_group_comparison_spec(
    "kruskal_wallis", c("continuous", "count", "ordinal"),
    "mean_rank_difference", "rank", "dunn",
    required_packages = post_hoc_packages(post_hoc)
  )
  spec$omnibus_method <- "kruskal_wallis"
  spec$post_hoc <- post_hoc
  spec$posthoc_method <- post_hoc_method_id(post_hoc)
  spec
}

validate_omnibus_post_hoc <- function(post_hoc, supported_method) {
  if (missing(post_hoc) || !inherits(post_hoc, "post_hoc_spec")) {
    stop_postprocessing("`post_hoc` must be explicitly supplied as a post_hoc_spec.")
  }
  if (post_hoc$type == "pairwise" && post_hoc$method$id != supported_method) {
    stop_postprocessing(paste0(
      "This omnibus method supports `", supported_method, "` post-hoc tests."
    ))
  }
  post_hoc
}

post_hoc_method_id <- function(x) {
  if (x$type == "none") "none" else x$method$id
}

post_hoc_packages <- function(x) {
  if (x$type == "none") character() else x$method$required_packages
}

#' Construct pairwise group comparisons without a reported omnibus test
#'
#' Pairwise comparisons and omnibus tests are independent decisions. This
#' constructor executes the declared pairwise procedure and returns no omnibus
#' test or omnibus effect. Some backends, such as Tukey HSD, still fit their
#' underlying model; `no_omnibus()` controls the reported inferential target.
#'
#' @param pairwise A non-empty [pairwise_post_hoc()] specification.
#' @param omnibus Must be explicitly [no_omnibus()].
#'
#' @return A `group_comparison_spec`.
#' @export
pairwise_group_comparisons <- function(pairwise, omnibus) {
  if (missing(pairwise) || !inherits(pairwise, "post_hoc_spec") ||
      pairwise$type != "pairwise") {
    stop_postprocessing("`pairwise` must be an explicit pairwise post-hoc specification.")
  }
  if (missing(omnibus) || !inherits(omnibus, "omnibus_spec") ||
      omnibus$type != "none") {
    stop_postprocessing("`omnibus` must be explicitly supplied as no_omnibus().")
  }
  method_id <- post_hoc_method_id(pairwise)
  properties <- switch(
    method_id,
    tukey = list(
      types = c("continuous", "count"), effect = "mean_difference",
      scale = "identity", ci = "tukey_hsd"
    ),
    games_howell = list(
      types = c("continuous", "count"), effect = "mean_difference",
      scale = "identity", ci = "games_howell"
    ),
    dunn = list(
      types = c("continuous", "count", "ordinal"),
      effect = "mean_rank_difference", scale = "rank", ci = "none"
    ),
    stop_postprocessing("Unsupported standalone pairwise method.")
  )
  spec <- new_group_comparison_spec(
    paste0("pairwise_", method_id), properties$types, properties$effect,
    properties$scale, properties$ci,
    required_packages = post_hoc_packages(pairwise)
  )
  spec$omnibus <- omnibus
  spec$omnibus_method <- "none"
  spec$post_hoc <- pairwise
  spec$posthoc_method <- method_id
  spec
}

new_group_comparison_spec <- function(
  id, types, effect_measure, scale, ci_method, compute = NULL,
  required_packages = character(), function_hash = NA_character_,
  requires_positive_cells = FALSE
) {
  structure(list(
    id = id, types = types, effect_measure = effect_measure, scale = scale,
    ci_method = ci_method, compute = compute,
    required_packages = required_packages, function_hash = function_hash,
    requires_positive_cells = requires_positive_cells
  ), class = "group_comparison_spec")
}

#' Construct a custom descriptive group comparison
#'
#' @param id Stable comparison identifier.
#' @param types Supported analytical variable types.
#' @param effect_measure Declared effect measure.
#' @param scale Declared result scale.
#' @param compute Function accepting a read-only `group_comparison_context` and
#'   returning a value created by `group_comparison_output()`.
#' @param ci_method Confidence interval implementation identifier.
#' @param required_packages Required packages checked during preflight.
#'
#' @return A `group_comparison_spec`.
#' @export
group_comparison_function <- function(
  id, types, effect_measure, scale, compute,
  ci_method = "custom", required_packages = character()
) {
  values <- list(id = id, effect_measure = effect_measure, scale = scale,
    ci_method = ci_method)
  if (any(!vapply(values, function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, logical(1)))) {
    stop_group_comparison_contract(
      "`id`, `effect_measure`, `scale`, and `ci_method` must be non-empty strings."
    )
  }
  valid_types <- setdiff(valid_variable_types, "unknown")
  if (!is.character(types) || length(types) == 0L || anyNA(types) ||
      any(!types %in% valid_types) || anyDuplicated(types)) {
    stop_group_comparison_contract("`types` contains invalid variable types.")
  }
  if (!is.function(compute)) {
    stop_group_comparison_contract("`compute` must be a function.")
  }
  if (!is.character(required_packages) || anyNA(required_packages)) {
    stop_group_comparison_contract(
      "`required_packages` must be a character vector."
    )
  }
  new_group_comparison_spec(
    id, types, effect_measure, scale, ci_method, compute,
    unique(required_packages), digest::digest(compute)
  )
}

#' Construct custom group comparison output
#'
#' @param estimate,conf_low,conf_high,p_value Numeric scalar results.
#' @param std_error Optional standard error on `std_error_scale`.
#' @param std_error_scale Scale on which the standard error is defined.
#' @param statistic_method Non-empty method identifier.
#' @param statistic,df Optional test statistic and degrees of freedom.
#' @param test Optional test name; omit it when no separate test is returned.
#'
#' @return A `group_comparison_output`.
#' @export
group_comparison_output <- function(
  estimate, conf_low, conf_high, p_value, statistic_method,
  statistic = NA_real_, df = NA_real_, test = NA_character_,
  std_error = NA_real_, std_error_scale = NA_character_
) {
  numeric_values <- c(estimate, conf_low, conf_high, p_value, statistic, df)
  if (!is.numeric(numeric_values) || length(numeric_values) != 6L) {
    stop_group_comparison_output("Comparison output values must be numeric scalars.")
  }
  if (!is.character(statistic_method) || length(statistic_method) != 1L ||
      is.na(statistic_method) || !nzchar(statistic_method)) {
    stop_group_comparison_output("`statistic_method` must be one non-empty string.")
  }
  if (!is.character(test) || length(test) != 1L) {
    stop_group_comparison_output("`test` must be one character value.")
  }
  structure(list(
    estimate = as.numeric(estimate), conf_low = as.numeric(conf_low),
    conf_high = as.numeric(conf_high), p_value = as.numeric(p_value),
    statistic_method = statistic_method, statistic = as.numeric(statistic),
    df = as.numeric(df), test = test, std_error = as.numeric(std_error),
    std_error_scale = std_error_scale
  ), class = "group_comparison_output")
}

default_group_comparison_spec <- function(type) {
  if (type %in% c("continuous", "count")) return(mean_difference())
  if (type == "binary") return(risk_difference())
  NULL
}

stop_group_comparison_contract <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_group_comparison", "error", "condition")
  ))
}

stop_group_comparison_output <- function(message) {
  stop(structure(
    list(message = message, call = sys.call(-1L)),
    class = c(
      "bq_error_invalid_group_comparison_output", "error", "condition"
    )
  ))
}
