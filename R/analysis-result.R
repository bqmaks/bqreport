#' Run a validated analysis plan
#'
#' Only validated tasks with status `ready` are executed. Other tasks are
#' retained in the result plan and represented in the issues component. Engine
#' failures never trigger an undeclared fallback method. Explicit
#' [analysis_method_chain()] objects retain every runtime attempt.
#'
#' @param plan A validated `analysis_plan`.
#' @param data A `bq_data` object.
#' @param error Runtime engine error handling: collect, stop, or warn.
#'
#' @return An `analysis_result`.
#' @examples
#' data <- as_bq_data(tibble::tibble(
#'   bmi = c(21.4, 27.9, 24.2, 30.1, 26.6, 23.0, 28.4, 22.1),
#'   treatment = factor(rep(c("Control", "Treatment"), 4))
#' )) |>
#'   set_outcome(bmi, type = "continuous") |>
#'   set_predictor(treatment, type = "binary", reference = "Control")
#' result <- plan_analysis(data, bmi, treatment) |>
#'   validate_plan(data) |>
#'   run_analysis(data)
#' estimates(result)
#' issues(result)
#' @export
run_analysis <- function(plan, data, error = c("collect", "stop", "warn")) {
  error <- match.arg(error)
  if (!inherits(plan, "analysis_plan")) {
    stop_plan("`plan` must be an analysis_plan.", "bq_error_invalid_plan")
  }
  check_bq_data(data)
  if (any(!plan$validated)) {
    stop_plan(
      "All plan tasks must pass `validate_plan()` before execution.",
      "bq_error_unvalidated_plan"
    )
  }

  plan <- validate_plan(plan, data)
  model_list <- list()
  estimate_rows <- list()
  test_rows <- list()
  diagnostic_rows <- list()
  issue_rows <- list()
  provenance_rows <- list()
  contrast_rows <- list()
  descriptive_rows <- list()
  survival_rows <- list()
  omnibus_effect_rows <- list()
  correlation_rows <- list()
  attempt_rows <- list()

  for (i in seq_len(nrow(plan))) {
    spec <- plan[i, , drop = FALSE]
    analysis_id <- spec$analysis_id[[1]]

    if (spec$status[[1]] != "ready") {
      severity <- if (spec$status[[1]] == "invalid") "error" else "info"
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id,
        "preflight",
        severity,
        paste0("bq_", spec$status[[1]], "_analysis"),
        if (is.na(spec$reason[[1]])) {
          paste0("Analysis status is `", spec$status[[1]], "`.")
        } else {
          spec$reason[[1]]
        }
      )
      next
    }

    if (identical(spec$analysis_type[[1]], "correlation")) {
      output <- tryCatch(
        execute_correlation(spec, data), error = function(condition) condition
      )
      if (inherits(output, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "estimate", "error", class(output)[[1]],
          conditionMessage(output)
        )
      } else {
        correlation_rows[[length(correlation_rows) + 1L]] <- output
        resampling_metrics <- c(
          bootstrap_successful = output$bootstrap_successful[[1]],
          permutation_successful = output$permutation_successful[[1]]
        )
        requested_resampling <- c(
          output$bootstrap_replicates[[1]], output$permutation_replicates[[1]]
        ) > 0L
        if (any(requested_resampling)) {
          diagnostic_rows[[length(diagnostic_rows) + 1L]] <- tibble::tibble(
            analysis_id = analysis_id,
            metric = names(resampling_metrics)[requested_resampling],
            value = as.numeric(resampling_metrics[requested_resampling]),
            status = "observed", message = NA_character_
          )
        }
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    if (identical(spec$analysis_type[[1]], "longitudinal_regression")) {
      output <- tryCatch(
        execute_longitudinal_analysis(spec, data),
        error = function(condition) condition
      )
      if (inherits(output, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "fit", "error", class(output)[[1]], conditionMessage(output)
        )
      } else {
        model_list[[analysis_id]] <- output$model
        estimate_rows[[length(estimate_rows) + 1L]] <- output$estimates
        test_rows[[length(test_rows) + 1L]] <- output$tests
        if (nrow(output$contrasts)) {
          contrast_rows[[length(contrast_rows) + 1L]] <- output$contrasts
        }
        diagnostic_rows[[length(diagnostic_rows) + 1L]] <- output$diagnostics
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    if (identical(spec$analysis_type[[1]], "descriptive")) {
      computed <- tryCatch(
        compute_observed_descriptives(spec, data),
        error = function(condition) condition
      )
      if (inherits(computed, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "compute", "error", class(computed)[[1]],
          conditionMessage(computed)
        )
      } else {
        descriptive_rows[[length(descriptive_rows) + 1L]] <- computed
        comparison <- tryCatch(
          compute_descriptive_comparison(spec, data),
          error = function(condition) condition
        )
        if (inherits(comparison, "error")) {
          issue_rows[[length(issue_rows) + 1L]] <- issue_row(
            analysis_id, "comparison", "error", class(comparison)[[1]],
            conditionMessage(comparison)
          )
        } else {
          if (nrow(comparison$contrasts)) {
            contrast_rows[[length(contrast_rows) + 1L]] <- comparison$contrasts
          }
          if (nrow(comparison$tests)) {
            test_rows[[length(test_rows) + 1L]] <- comparison$tests
          }
          if (nrow(comparison$omnibus_effects)) {
            omnibus_effect_rows[[length(omnibus_effect_rows) + 1L]] <-
              comparison$omnibus_effects
          }
        }
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    if (identical(spec$analysis_type[[1]], "survival_regression")) {
      output <- tryCatch(
        execute_cox_analysis(spec, data),
        error = function(condition) condition
      )
      if (inherits(output, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "fit", "error", class(output)[[1]],
          conditionMessage(output)
        )
      } else {
        model_list[[analysis_id]] <- output$model
        estimate_rows[[length(estimate_rows) + 1L]] <- output$estimates
        test_rows[[length(test_rows) + 1L]] <- output$tests
        if (nrow(output$contrasts)) {
          contrast_rows[[length(contrast_rows) + 1L]] <- output$contrasts
        }
        diagnostic_rows[[length(diagnostic_rows) + 1L]] <- output$diagnostics
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    if (identical(spec$analysis_type[[1]], "kaplan_meier")) {
      output <- tryCatch(
        execute_kaplan_meier(spec, data),
        error = function(condition) condition
      )
      if (inherits(output, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "estimate", "error", class(output)[[1]],
          conditionMessage(output)
        )
      } else {
        model_list[[analysis_id]] <- output$model
        survival_rows[[length(survival_rows) + 1L]] <- output$estimates
        if (nrow(output$tests)) test_rows[[length(test_rows) + 1L]] <- output$tests
        if (nrow(output$contrasts)) {
          contrast_rows[[length(contrast_rows) + 1L]] <- output$contrasts
        }
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    if (identical(spec$analysis_type[[1]], "cumulative_incidence")) {
      output <- tryCatch(execute_cumulative_incidence(spec, data), error = function(condition) condition)
      if (inherits(output, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "estimate", "error", class(output)[[1]], conditionMessage(output)
        )
      } else {
        model_list[[analysis_id]] <- output$model
        survival_rows[[length(survival_rows) + 1L]] <- output$estimates
        provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      }
      next
    }

    frame <- build_analysis_frame(spec, data)
    captured_warnings <- character()
    if (identical(spec$engine[[1]], "method_chain")) {
      chain_result <- execute_method_chain(spec, frame, data)
      attempt_rows[[length(attempt_rows) + 1L]] <- chain_result$attempts
      for (j in which(chain_result$attempts$status == "failed")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "fit",
          if (isTRUE(chain_result$success) && j < nrow(chain_result$attempts)) "warning" else "error",
          chain_result$attempts$condition_class[[j]],
          paste0(
            "Method `", chain_result$attempts$method[[j]], "` failed: ",
            chain_result$attempts$message[[j]]
          )
        )
      }
      if (!isTRUE(chain_result$success)) next
      custom_result <- chain_result$output
      spec <- chain_result$spec
      plan$executed_method[[i]] <- chain_result$method
      if (!is.null(custom_result$model)) model_list[[analysis_id]] <- custom_result$model
      if (nrow(custom_result$estimates)) estimate_rows[[length(estimate_rows) + 1L]] <- custom_result$estimates
      if (nrow(custom_result$tests)) test_rows[[length(test_rows) + 1L]] <- custom_result$tests
      if (nrow(custom_result$contrasts)) contrast_rows[[length(contrast_rows) + 1L]] <- custom_result$contrasts
      if (nrow(custom_result$diagnostics)) diagnostic_rows[[length(diagnostic_rows) + 1L]] <- custom_result$diagnostics
      if (nrow(custom_result$issues)) issue_rows[[length(issue_rows) + 1L]] <- custom_result$issues
      if (!is.null(custom_result$model)) {
        additional_comparisons <- tryCatch(
          compute_custom_comparisons(custom_result$model, spec, frame, data),
          error = function(condition) condition
        )
        if (inherits(additional_comparisons, "error")) {
          issue_rows[[length(issue_rows) + 1L]] <- issue_row(
            analysis_id, "contrasts", "error", class(additional_comparisons)[[1]],
            conditionMessage(additional_comparisons)
          )
        } else if (nrow(additional_comparisons)) {
          contrast_rows[[length(contrast_rows) + 1L]] <- additional_comparisons
        }
      }
      provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(
        spec, declared_spec = plan[i, , drop = FALSE], fallback_used = nrow(chain_result$attempts) > 1L
      )
      next
    }
    if (identical(spec$engine[[1]], "custom_function")) {
      custom_result <- tryCatch(
        execute_custom_method(spec, frame, data),
        error = function(condition) condition
      )
      if (inherits(custom_result, "error")) {
        issue_rows[[length(issue_rows) + 1L]] <- issue_row(
          analysis_id, "fit", "error", class(custom_result)[[1]],
          conditionMessage(custom_result)
        )
        next
      }
      if (!is.null(custom_result$model)) model_list[[analysis_id]] <- custom_result$model
      if (nrow(custom_result$estimates)) estimate_rows[[length(estimate_rows) + 1L]] <- custom_result$estimates
      if (nrow(custom_result$tests)) test_rows[[length(test_rows) + 1L]] <- custom_result$tests
      if (nrow(custom_result$contrasts)) contrast_rows[[length(contrast_rows) + 1L]] <- custom_result$contrasts
      if (nrow(custom_result$diagnostics)) diagnostic_rows[[length(diagnostic_rows) + 1L]] <- custom_result$diagnostics
      if (nrow(custom_result$issues)) issue_rows[[length(issue_rows) + 1L]] <- custom_result$issues
      if (!is.null(custom_result$model)) {
        additional_comparisons <- tryCatch(
          compute_custom_comparisons(custom_result$model, spec, frame, data),
          error = function(condition) condition
        )
        if (inherits(additional_comparisons, "error")) {
          issue_rows[[length(issue_rows) + 1L]] <- issue_row(
            analysis_id, "contrasts", "error", class(additional_comparisons)[[1]],
            conditionMessage(additional_comparisons)
          )
        } else if (nrow(additional_comparisons)) {
          contrast_rows[[length(contrast_rows) + 1L]] <- additional_comparisons
        }
      }
      provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)
      next
    }
    fit <- tryCatch(
      withCallingHandlers(
        fit_builtin_engine(spec, frame),
        warning = function(condition) {
          captured_warnings <<- c(captured_warnings, conditionMessage(condition))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(condition) condition
    )

    if (inherits(fit, "error")) {
      message <- paste0(
        "Engine `", spec$engine[[1]], "` failed for analysis `",
        analysis_id, "`: ", conditionMessage(fit)
      )
      if (error == "stop") {
        stop_engine(message, analysis_id)
      }
      if (error == "warn") {
        warning(engine_warning(message, analysis_id), call. = FALSE)
      }
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "fit", "error", class(fit)[[1]], message
      )
      next
    }

    if ("..bq_cluster" %in% names(frame)) {
      attr(fit, "bq_clusters") <- frame[["..bq_cluster"]]
    }

    model_list[[analysis_id]] <- fit
    post_fit <- withCallingHandlers(
      list(
        estimates = tidy_builtin_estimates(fit, spec),
        tests = tidy_builtin_test(fit, spec, frame),
        diagnostics = diagnose_builtin(fit, spec)
      ),
      warning = function(condition) {
        captured_warnings <<- c(captured_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    estimate_rows[[length(estimate_rows) + 1L]] <- post_fit$estimates
    computed_contrasts <- compute_builtin_contrasts(fit, spec, data)
    if (nrow(computed_contrasts) > 0L) {
      contrast_rows[[length(contrast_rows) + 1L]] <- computed_contrasts
    }
    conditional_contrasts <- tryCatch(
      compute_conditional_contrasts(fit, spec, data),
      error = function(condition) condition
    )
    if (inherits(conditional_contrasts, "error")) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "contrasts", "error", class(conditional_contrasts)[[1]],
        conditionMessage(conditional_contrasts)
      )
    } else if (nrow(conditional_contrasts)) {
      contrast_rows[[length(contrast_rows) + 1L]] <- conditional_contrasts
    }
    interaction_contrasts <- tryCatch(
      compute_contrasts_of_contrasts(fit, spec, data),
      error = function(condition) condition
    )
    if (inherits(interaction_contrasts, "error")) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "contrasts", "error", class(interaction_contrasts)[[1]],
        conditionMessage(interaction_contrasts)
      )
    } else if (nrow(interaction_contrasts)) {
      contrast_rows[[length(contrast_rows) + 1L]] <- interaction_contrasts
    }
    custom_comparisons <- tryCatch(
      compute_custom_comparisons(fit, spec, frame, data),
      error = function(condition) condition
    )
    if (inherits(custom_comparisons, "error")) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "contrasts", "error", class(custom_comparisons)[[1]],
        conditionMessage(custom_comparisons)
      )
    } else if (nrow(custom_comparisons)) {
      contrast_rows[[length(contrast_rows) + 1L]] <- custom_comparisons
    }
    test_rows[[length(test_rows) + 1L]] <- post_fit$tests
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <- post_fit$diagnostics
    provenance_rows[[length(provenance_rows) + 1L]] <- provenance_row(spec)

    for (message in unique(captured_warnings)) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        analysis_id, "fit", "warning", "bq_warning_engine", message
      )
    }
  }

  correlation_output <- bind_component(correlation_rows, correlations_prototype())
  if (nrow(correlation_output)) {
    families <- split(seq_len(nrow(correlation_output)),
      correlation_output$correlation_family_id)
    for (rows in families) {
      correlation_output$p_adjusted[rows] <- stats::p.adjust(
        correlation_output$p_value[rows],
        method = correlation_output$adjust_method[[rows[[1]]]]
      )
    }
    correlation_interactions <- tryCatch(
      compute_correlation_interactions(correlation_output),
      error = function(condition) condition
    )
    if (inherits(correlation_interactions, "error")) {
      issue_rows[[length(issue_rows) + 1L]] <- issue_row(
        correlation_output$correlation_interaction_id[[1]], "comparison", "error",
        class(correlation_interactions)[[1]], conditionMessage(correlation_interactions)
      )
    } else if (nrow(correlation_interactions$tests)) {
      test_rows[[length(test_rows) + 1L]] <- correlation_interactions$tests
    }
    if (!inherits(correlation_interactions, "error") &&
        nrow(correlation_interactions$contrasts)) {
      contrast_rows[[length(contrast_rows) + 1L]] <-
        correlation_interactions$contrasts
    }
  }
  structure(
    list(
      plan = plan,
      models = model_list,
      estimates = bind_component(estimate_rows, estimates_prototype()),
      contrasts = bind_component(contrast_rows, contrasts_prototype()),
      tests = bind_component(test_rows, tests_prototype()),
      descriptives = bind_component(descriptive_rows, descriptives_prototype()),
      survival_estimates = bind_component(survival_rows, survival_estimates_prototype()),
      omnibus_effects = bind_component(
        omnibus_effect_rows, omnibus_effects_prototype()
      ),
      correlations = correlation_output,
      diagnostics = bind_component(diagnostic_rows, diagnostics_prototype()),
      issues = bind_component(issue_rows, issues_prototype()),
      attempts = bind_component(attempt_rows, attempts_prototype()),
      provenance = bind_component(provenance_rows, provenance_prototype())
    ),
    class = "analysis_result"
  )
}
