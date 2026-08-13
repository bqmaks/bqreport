#' Register a longitudinal study design
#'
#' @param .data A `bq_data` object.
#' @param id Exactly one subject identifier column.
#' @param time Exactly one observation-time column for long data.
#' @param group Optional treatment or grouping column.
#' @param layout Input layout, long or wide.
#' @param baseline Optional baseline time value.
#' @param time_scale Whether time is categorical or continuous.
#' @return Updated `bq_data`.
#' @export
set_longitudinal_design <- function(
  .data, id, time = tidyselect::any_of(character()),
  group = tidyselect::any_of(character()), layout = c("long", "wide"),
  baseline = NULL, time_scale = c("categorical", "continuous")
) {
  check_bq_data(.data)
  layout <- match.arg(layout)
  time_scale <- match.arg(time_scale)
  id_selection <- tidyselect::eval_select(rlang::enquo(id), .data)
  time_selection <- tidyselect::eval_select(rlang::enquo(time), .data)
  group_selection <- tidyselect::eval_select(rlang::enquo(group), .data)
  if (length(id_selection) != 1L) {
    stop_invalid_longitudinal_design("`id` must select exactly one column.")
  }
  if ((layout == "long" && length(time_selection) != 1L) ||
      (layout == "wide" && length(time_selection) != 0L)) {
    stop_invalid_longitudinal_design(
      "Long layout requires one `time` column; wide layout must not supply one."
    )
  }
  if (length(group_selection) > 1L) {
    stop_invalid_longitudinal_design("`group` may select at most one column.")
  }
  if (!is.null(baseline) && (length(baseline) != 1L || is.na(baseline))) {
    stop_invalid_longitudinal_design("`baseline` must be one non-missing value.")
  }
  registry <- variables(.data)
  id_name <- names(id_selection)
  time_name <- names(time_selection)
  group_name <- names(group_selection)
  status <- "valid"
  reason <- NA_character_
  if (layout == "long") {
    time_values <- analysis_vector(.data[[time_name]])
    if (!is.null(baseline) && !baseline %in% time_values) {
      status <- "invalid"
      reason <- "The configured baseline is absent from the time variable."
    }
    complete <- !special_missing_mask(.data[[id_name]]) &
      !special_missing_mask(.data[[time_name]])
    keys <- paste(
      analysis_vector(.data[[id_name]])[complete],
      analysis_vector(.data[[time_name]])[complete], sep = "\r"
    )
    if (anyDuplicated(keys)) {
      status <- "invalid"
      reason <- append_reasons(reason, "Duplicate id-time observations were found.")
    }
  }
  id_var_id <- registry$var_id[match(id_name, registry$name)]
  time_var_id <- if (length(time_name)) {
    registry$var_id[match(time_name, registry$name)]
  } else {
    NA_character_
  }
  group_var_id <- if (length(group_name)) {
    registry$var_id[match(group_name, registry$name)]
  } else {
    NA_character_
  }
  row <- tibble::tibble(
    design_id = bq_id(
      "design", "longitudinal", layout, id_var_id, time_var_id,
      group_var_id, baseline, time_scale
    ),
    type = "longitudinal", layout = layout,
    id_var_id = id_var_id,
    time_var_id = time_var_id,
    group_var_id = group_var_id,
    baseline = list(baseline), time_scale = time_scale,
    status = status, reason = reason
  )
  attr(.data, "design_registry") <- row
  .data <- add_role_by_name(.data, id_name, "id")
  if (length(time_name)) .data <- add_role_by_name(.data, time_name, "time")
  if (length(group_name)) .data <- add_role_by_name(.data, group_name, "group")
  .data
}

#' Access registered study designs
#' @param x A `bq_data` object.
#' @return A tidy design registry with current column names.
#' @export
designs <- function(x) {
  check_bq_data(x)
  design <- tibble::as_tibble(attr(x, "design_registry", exact = TRUE))
  if (nrow(design) == 0L) return(design)
  registry <- variables(x)
  id_row <- match(design$id_var_id, registry$var_id)
  time_row <- match(design$time_var_id, registry$var_id)
  group_row <- match(design$group_var_id, registry$var_id)
  design$id <- registry$name[id_row]
  design$time <- registry$name[time_row]
  design$group <- registry$name[group_row]
  missing_id <- is.na(id_row)
  missing_time <- design$layout == "long" & is.na(time_row)
  missing_group <- !is.na(design$group_var_id) & is.na(group_row)
  invalid <- missing_id | missing_time | missing_group
  design$status[invalid] <- "invalid"
  design$reason[invalid] <- "A longitudinal design component is absent from the data."
  design
}

#' Register a repeated longitudinal outcome
#'
#' @param .data A `bq_data` object with a longitudinal design.
#' @param name Bare analytical outcome name.
#' @param values Source columns in explicit time order for wide data. For long
#'   data, select the single outcome column.
#' @param time Explicit time values corresponding to `values` for wide data.
#' @param baseline Optional baseline value; defaults to the design baseline.
#' @param time_scale Optional outcome-specific time scale.
#' @param type Analytical outcome type.
#' @param event_value Explicit event value for a binary repeated outcome.
#' @return Updated `bq_data`.
#' @export
add_longitudinal_outcome <- function(
  .data, name, values, time = NULL, baseline = NULL,
  time_scale = NULL, type = "continuous", event_value = NULL
) {
  check_bq_data(.data)
  design <- designs(.data)
  if (nrow(design) != 1L || design$type[[1]] != "longitudinal") {
    stop_invalid_longitudinal_outcome("Register one longitudinal design first.")
  }
  outcome_name <- rlang::as_name(rlang::enquo(name))
  selection <- tidyselect::eval_select(rlang::enquo(values), .data)
  value_names <- names(selection)
  if (design$layout[[1]] == "long") {
    if (length(selection) != 1L || !is.null(time)) {
      stop_invalid_longitudinal_outcome(
        "Long outcomes require one `values` column and no explicit `time` mapping."
      )
    }
    time_values <- NULL
  } else {
    if (!is.atomic(time) || length(time) != length(selection) || length(time) < 2L) {
      stop_invalid_longitudinal_outcome(
        "Wide `values` and `time` must have matching lengths of at least two."
      )
    }
    if (anyNA(time) || anyDuplicated(time)) {
      stop_invalid_longitudinal_outcome("Wide time values must be unique and non-missing.")
    }
    time_values <- time
  }
  baseline <- baseline %||% design$baseline[[1]]
  if (!is.null(baseline) && design$layout[[1]] == "wide" && !baseline %in% time_values) {
    stop_invalid_longitudinal_outcome("The baseline is absent from the explicit time mapping.")
  }
  time_scale <- time_scale %||% design$time_scale[[1]]
  if (!time_scale %in% c("categorical", "continuous")) {
    stop_invalid_longitudinal_outcome("`time_scale` must be categorical or continuous.")
  }
  if (!type %in% valid_variable_types || type == "unknown") {
    stop_invalid_longitudinal_outcome("`type` is not a supported analytical variable type.")
  }
  if (type == "binary" && (is.null(event_value) || length(event_value) != 1L ||
      is.na(event_value))) {
    stop_invalid_longitudinal_outcome(
      "Binary longitudinal outcomes require one explicit `event_value`."
    )
  }
  registry <- variables(.data)
  outcome_variable_type <- type
  source_rows <- match(value_names, registry$name)
  storage_types <- registry$storage_type[source_rows]
  status <- if (length(unique(storage_types)) == 1L) "valid" else "invalid"
  reason <- if (status == "valid") NA_character_ else
    "Repeated source columns have incompatible storage types."
  row <- tibble::tibble(
    outcome_id = bq_id(
      "outcome", outcome_name, "longitudinal", design$design_id[[1]],
      registry$var_id[source_rows], time_values, baseline, time_scale,
      event_value
    ),
    name = outcome_name, type = "longitudinal", variable_type = outcome_variable_type,
    design_id = design$design_id[[1]], layout = design$layout[[1]],
    value_var_ids = list(registry$var_id[source_rows]),
    time_values = list(time_values), baseline = list(baseline),
    time_scale = time_scale, event_value = list(event_value),
    status = status, reason = reason
  )
  current <- attr(.data, "outcome_registry", exact = TRUE)
  if (nrow(current) && outcome_name %in% current$name) {
    stop_invalid_longitudinal_outcome(paste0("Outcome `", outcome_name, "` is already registered."))
  }
  attr(.data, "outcome_registry") <- if (nrow(current)) {
    vctrs::vec_rbind(current, row)
  } else row
  for (source in value_names) .data <- add_role_by_name(.data, source, "outcome")
  .data
}

build_longitudinal_frame <- function(data, outcome_name) {
  design <- designs(data)
  outcome <- outcomes(data)
  outcome <- outcome[outcome$name == outcome_name & outcome$type == "longitudinal", , drop = FALSE]
  if (nrow(design) != 1L || nrow(outcome) != 1L ||
      design$status[[1]] != "valid" || outcome$status[[1]] != "valid") {
    stop_invalid_longitudinal_design("The longitudinal design or outcome is invalid.")
  }
  if (design$layout[[1]] == "long") {
    frame <- tibble::tibble(
      ..bq_id = data[[design$id[[1]]]],
      ..bq_time = data[[design$time[[1]]]],
      ..bq_outcome = data[[outcome$values[[1]][[1]]]]
    )
  } else {
    source <- outcome$values[[1]]
    rows <- lapply(seq_len(nrow(data)), function(i) tibble::tibble(
      ..bq_id = rep(data[[design$id[[1]]]][[i]], length(source)),
      ..bq_time = outcome$time_values[[1]],
      ..bq_outcome = unlist(data[i, source], use.names = FALSE)
    ))
    frame <- vctrs::vec_rbind(!!!rows)
  }
  if (!is.na(design$group[[1]])) {
    if (design$layout[[1]] == "long") {
      frame$..bq_group <- data[[design$group[[1]]]]
    } else {
      frame$..bq_group <- rep(
        data[[design$group[[1]]]], each = length(outcome$values[[1]])
      )
    }
  }
  if (outcome$time_scale[[1]] == "categorical") {
    levels <- if (design$layout[[1]] == "wide") outcome$time_values[[1]] else
      unique(analysis_vector(frame$..bq_time))
    frame$..bq_time <- factor(frame$..bq_time, levels = levels)
  }
  if (outcome$variable_type[[1]] == "binary") {
    frame$..bq_outcome <- analysis_vector(frame$..bq_outcome) ==
      outcome$event_value[[1]]
  }
  attr(frame, "reshape_spec") <- list(
    layout = design$layout[[1]], design_id = design$design_id[[1]],
    outcome_id = outcome$outcome_id[[1]],
    value_var_ids = outcome$value_var_ids[[1]],
    time_values = outcome$time_values[[1]], baseline = outcome$baseline[[1]],
    time_scale = outcome$time_scale[[1]]
  )
  frame
}

stop_invalid_longitudinal_design <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_longitudinal_design", "error", "condition")))
}

stop_invalid_longitudinal_outcome <- function(message) {
  stop(structure(list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_outcome", "error", "condition")))
}
