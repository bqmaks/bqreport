#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.bq_data <- function(data, i, ...) {
  out <- vctrs::vec_slice(vctrs::vec_data(data), i)
  dplyr::dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.bq_data <- function(data, cols) {
  out <- NextMethod()
  modified <- intersect(names(cols), names(data))

  for (name in modified) {
    out <- invalidate_modified_variable(out, data, name)
  }
  out
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.bq_data <- function(data, template) {
  data <- tibble::as_tibble(data)
  old_registry <- attr(template, "variable_registry", exact = TRUE)
  current_names <- names(data)
  retained <- match(current_names, old_registry$name)
  registry_parts <- vector("list", length(current_names))

  for (column in seq_along(current_names)) {
    old_row <- retained[[column]]
    if (!is.na(old_row)) {
      registry_parts[[column]] <- old_registry[old_row, , drop = FALSE]
    } else {
      registry_parts[[column]] <- new_variable_registry(data[column])
    }
  }

  registry <- if (length(registry_parts) == 0L) {
    old_registry[0, , drop = FALSE]
  } else {
    vctrs::vec_rbind(!!!registry_parts)
  }
  registry$name <- current_names

  attr(data, "variable_registry") <- registry
  attr(data, "outcome_registry") <- attr(template, "outcome_registry", exact = TRUE)
  attr(data, "design_registry") <- attr(template, "design_registry", exact = TRUE)
  attr(data, "contrast_registry") <- attr(template, "contrast_registry", exact = TRUE)
  class(data) <- c("bq_data", setdiff(class(data), "bq_data"))
  data
}

invalidate_modified_variable <- function(out, before, name) {
  registry <- attr(out, "variable_registry", exact = TRUE)
  row <- match(name, registry$name)
  old_column <- before[[name]]
  new_column <- out[[name]]
  metadata_changed <- !identical(labelled_signature(old_column), labelled_signature(new_column))
  storage_changed <- !identical(storage_type(old_column), storage_type(new_column))

  registry$storage_type[[row]] <- storage_type(new_column)
  registry$distribution[[row]] <- NA_character_
  registry$transformation[row] <- list(NULL)
  registry$status[[row]] <- "review"

  if (metadata_changed || storage_changed) {
    registry$reference[row] <- list(NULL)
    registry$event_value[row] <- list(NULL)
    registry$label[[row]] <- variable_label(new_column)
    registry <- sync_labelled_registry(registry, row, new_column)

    if (!registry$locked[[row]]) {
      inferred <- infer_analytical_type(new_column)
      registry$type[[row]] <- inferred
      registry$source[[row]] <- if (inferred == "unknown") "default" else "inferred"
    }
  }

  attr(out, "variable_registry") <- registry
  out
}

labelled_signature <- function(x) {
  list(
    label = labelled::var_label(x),
    value_labels = labelled::val_labels(x),
    na_values = labelled::na_values(x),
    na_range = labelled::na_range(x)
  )
}

sync_labelled_registry <- function(registry, row, column) {
  values <- list(
    value_labels = labelled::val_labels(column),
    na_values = labelled::na_values(column),
    na_range = labelled::na_range(column)
  )
  for (property in intersect(names(values), names(registry))) {
    registry[[property]][row] <- list(values[[property]])
  }
  registry
}

#' @export
`[.bq_data` <- function(x, i, j, ..., drop = FALSE) {
  out <- NextMethod()
  if (!is.data.frame(out)) {
    return(out)
  }
  dplyr::dplyr_reconstruct(out, x)
}

#' @export
`names<-.bq_data` <- function(x, value) {
  old_names <- names(x)
  out <- NextMethod()
  registry <- attr(out, "variable_registry", exact = TRUE)

  if (length(value) == length(old_names) && nrow(registry) == length(value)) {
    registry$name <- value
    attr(out, "variable_registry") <- registry
  }
  out
}
