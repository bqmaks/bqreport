#' Convert data to a bq data object
#'
#' `as_bq_data()` creates a tibble subclass and initializes the package's
#' metadata registries. Existing `bq_data` objects are returned without
#' regenerating variable identifiers.
#'
#' @param x A data frame.
#' @param metadata An optional metadata data frame. See [apply_dictionary()].
#'
#' @return A `bq_data` tibble.
#' @export
as_bq_data <- function(x, metadata = NULL) {
  if (inherits(x, "bq_data")) {
    if (is.null(metadata)) {
      return(x)
    }
    return(apply_dictionary(x, metadata))
  }

  if (!is.data.frame(x)) {
    stop_invalid_data("`x` must be a data frame.")
  }

  x <- tibble::as_tibble(x)
  registry <- new_variable_registry(x)

  attr(x, "variable_registry") <- registry
  attr(x, "outcome_registry") <- tibble::tibble()
  attr(x, "design_registry") <- tibble::tibble()
  attr(x, "contrast_registry") <- tibble::tibble()
  class(x) <- c("bq_data", class(x))
  if (is.null(metadata)) x else apply_dictionary(x, metadata)
}

#' Access the variable registry
#'
#' @param x A `bq_data` object.
#'
#' @return A tibble containing variable metadata.
#' @export
variables <- function(x) {
  if (!inherits(x, "bq_data")) {
    stop_invalid_data("`x` must be a bq_data object.")
  }

  registry <- attr(x, "variable_registry", exact = TRUE)
  tibble::as_tibble(registry)
}

new_variable_registry <- function(x) {
  n <- ncol(x)
  ids <- if (n == 0L) character() else paste0("var_", uuid::UUIDgenerate(n = n))
  analytical_type <- unname(vapply(x, infer_analytical_type, character(1)))

  tibble::tibble(
    var_id = ids,
    name = names(x),
    label = unname(vapply(x, variable_label, character(1))),
    unit = rep(NA_character_, n),
    digits = rep(NA_integer_, n),
    role = rep(list("auxiliary"), n),
    type = analytical_type,
    storage_type = unname(vapply(x, storage_type, character(1))),
    distribution = rep(NA_character_, n),
    reference = rep(list(NULL), n),
    coding = rep(NA_character_, n),
    weight_type = rep(NA_character_, n),
    cluster_type = rep(NA_character_, n),
    event_value = rep(list(NULL), n),
    transformation = rep(list(NULL), n),
    missing_policy = rep("complete_case", n),
    source = ifelse(analytical_type == "unknown", "default", "inferred"),
    locked = rep(FALSE, n),
    status = rep("review", n)
  )
}

#' Apply a variable metadata dictionary
#'
#' The dictionary uses column names as initialization-time keys. Metadata are
#' stored in the variable registry; `digits` never changes the underlying data
#' and is intended only for presentation layers.
#'
#' @param .data A `bq_data` object.
#' @param metadata A data frame with a required `name` column and optional
#'   `label`, `unit`, and `digits` columns. Labelled metadata can be supplied as
#'   list-columns named `value_labels`, `na_values`, and `na_range`. Additional,
#'   non-reserved columns are preserved in the variable registry. Missing
#'   property values leave the current metadata unchanged.
#'
#' @return `.data` with an updated variable registry.
#' @export
apply_dictionary <- function(.data, metadata) {
  check_bq_data(.data)
  metadata <- validate_dictionary(metadata, names(.data))
  registry <- attr(.data, "variable_registry", exact = TRUE)
  properties <- setdiff(names(metadata), "name")

  for (property in setdiff(properties, names(registry))) {
    registry[[property]] <- vctrs::vec_init(metadata[[property]], nrow(registry))
  }

  for (dictionary_row in seq_len(nrow(metadata))) {
    registry_row <- match(metadata$name[[dictionary_row]], registry$name)
    changed <- FALSE

    for (property in properties) {
      if (!dictionary_value_is_missing(metadata[[property]], dictionary_row)) {
        registry[[property]] <- tryCatch(
          vctrs::vec_assign(
            registry[[property]],
            registry_row,
            metadata[[property]][dictionary_row]
          ),
          error = function(error) {
            stop_invalid_dictionary(paste0(
              "Metadata column `", property,
              "` is incompatible with the existing registry column."
            ))
          }
        )
        changed <- TRUE
      }
    }

    if (changed) {
      registry$source[[registry_row]] <- "dictionary"
      registry$status[[registry_row]] <- "review"
    }

    .data <- apply_labelled_metadata(.data, metadata, dictionary_row)
  }

  attr(.data, "variable_registry") <- registry
  .data
}

apply_labelled_metadata <- function(.data, metadata, row) {
  variable <- metadata$name[[row]]
  column <- .data[[variable]]

  tryCatch(
    {
      if ("label" %in% names(metadata) && !is.na(metadata$label[[row]])) {
        labelled::var_label(column) <- metadata$label[[row]]
      }
      if (
        "value_labels" %in% names(metadata) &&
          !is.null(metadata$value_labels[[row]])
      ) {
        labelled::val_labels(column) <- metadata$value_labels[[row]]
      }
      if (
        "na_values" %in% names(metadata) &&
          !is.null(metadata$na_values[[row]])
      ) {
        labelled::na_values(column) <- metadata$na_values[[row]]
      }
      if (
        "na_range" %in% names(metadata) &&
          !is.null(metadata$na_range[[row]])
      ) {
        labelled::na_range(column) <- metadata$na_range[[row]]
      }
    },
    error = function(error) {
      stop_invalid_dictionary(paste0(
        "Labelled metadata for variable `", variable,
        "` is incompatible with its data: ", conditionMessage(error)
      ))
    }
  )

  .data[[variable]] <- column
  .data
}

dictionary_value_is_missing <- function(x, row) {
  if (is.list(x)) {
    return(is.null(x[[row]]))
  }
  is.na(x[[row]])
}

validate_dictionary <- function(metadata, data_names) {
  if (!is.data.frame(metadata)) {
    stop_invalid_dictionary("`metadata` must be a data frame.")
  }
  metadata <- tibble::as_tibble(metadata)
  protected <- c(
    "var_id", "role", "type", "storage_type", "distribution", "reference", "coding", "weight_type", "cluster_type",
    "event_value", "transformation", "missing_policy", "source", "locked",
    "status"
  )

  if (!"name" %in% names(metadata)) {
    stop_invalid_dictionary("`metadata` must contain a `name` column.")
  }
  protected_supplied <- intersect(names(metadata), protected)
  if (length(protected_supplied) > 0L) {
    stop_invalid_dictionary(paste0(
      "Protected registry columns cannot be supplied as metadata: ",
      paste(protected_supplied, collapse = ", "), "."
    ))
  }
  if (!is.character(metadata$name) || anyNA(metadata$name)) {
    stop_invalid_dictionary("`metadata$name` must be a character vector without missing values.")
  }
  if (anyDuplicated(metadata$name)) {
    stop_invalid_dictionary("Each variable may occur only once in `metadata`.")
  }
  unknown <- setdiff(metadata$name, data_names)
  if (length(unknown) > 0L) {
    stop_invalid_dictionary(paste0(
      "Unknown variables in `metadata`: ", paste(unknown, collapse = ", "), "."
    ))
  }
  for (property in intersect(c("label", "unit"), names(metadata))) {
    if (!is.character(metadata[[property]])) {
      stop_invalid_dictionary(paste0("`metadata$", property, "` must be character."))
    }
  }
  if ("digits" %in% names(metadata)) {
    digits <- metadata$digits
    valid_digits <- is.numeric(digits) && all(
      is.na(digits) | (is.finite(digits) & digits >= 0 & digits == floor(digits))
    )
    if (!valid_digits) {
      stop_invalid_dictionary("`metadata$digits` must contain non-negative whole numbers or missing values.")
    }
    metadata$digits <- as.integer(digits)
  }
  validate_labelled_dictionary(metadata)
  metadata
}

validate_labelled_dictionary <- function(metadata) {
  if ("value_labels" %in% names(metadata)) {
    if (!is.list(metadata$value_labels)) {
      stop_invalid_dictionary("`metadata$value_labels` must be a list-column.")
    }
    for (labels in metadata$value_labels) {
      if (is.null(labels)) {
        next
      }
      valid <- is.atomic(labels) && !is.null(names(labels)) &&
        all(!is.na(names(labels)) & nzchar(names(labels)))
      if (!valid) {
        stop_invalid_dictionary(
          "Each `value_labels` element must be a named atomic vector or NULL."
        )
      }
    }
  }
  if ("na_values" %in% names(metadata)) {
    if (!is.list(metadata$na_values)) {
      stop_invalid_dictionary("`metadata$na_values` must be a list-column.")
    }
    valid <- vapply(
      metadata$na_values,
      function(values) is.null(values) || is.atomic(values),
      logical(1)
    )
    if (!all(valid)) {
      stop_invalid_dictionary(
        "Each `na_values` element must be an atomic vector or NULL."
      )
    }
  }
  if ("na_range" %in% names(metadata)) {
    if (!is.list(metadata$na_range)) {
      stop_invalid_dictionary("`metadata$na_range` must be a list-column.")
    }
    valid <- vapply(metadata$na_range, function(range) {
      is.null(range) || (
        is.numeric(range) && length(range) == 2L && !anyNA(range) &&
          all(is.finite(range)) && range[[1L]] <= range[[2L]]
      )
    }, logical(1))
    if (!all(valid)) {
      stop_invalid_dictionary(
        "Each `na_range` element must be an ordered numeric vector of length two or NULL."
      )
    }
  }
  invisible(metadata)
}

stop_invalid_dictionary <- function(message) {
  condition <- structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_dictionary", "error", "condition")
  )
  stop(condition)
}

infer_analytical_type <- function(x) {
  if (inherits(x, "POSIXt")) {
    return("datetime")
  }
  if (inherits(x, "Date")) {
    return("date")
  }
  if (is.ordered(x)) {
    return("ordinal")
  }
  if (is.factor(x) || is.character(x)) {
    n_observed <- length(unique(as.character(x[!is.na(x)])))
    if (n_observed == 2L) {
      return("binary")
    }
    if (n_observed > 2L) {
      return("nominal")
    }
  }
  "unknown"
}

variable_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || length(label) != 1L || is.na(label)) {
    return(NA_character_)
  }
  as.character(label)
}

storage_type <- function(x) {
  if (is.object(x)) {
    return(class(x)[[1L]])
  }
  typeof(x)
}

stop_invalid_data <- function(message) {
  condition <- structure(
    list(message = message, call = sys.call(-1L)),
    class = c("bq_error_invalid_data", "error", "condition")
  )
  stop(condition)
}

valid_roles <- c(
  "outcome", "predictor", "group", "id", "time", "visit", "event",
  "offset", "weight", "cluster", "stratum", "auxiliary"
)

#' Add an analytical role to variables
#'
#' Roles are additive: a variable can simultaneously be, for example, a
#' `group` and a `predictor`. Assigning the same role repeatedly has no effect.
#'
#' @param .data A `bq_data` object.
#' @param .cols Columns selected using tidyselect syntax.
#' @param role A single supported role.
#'
#' @return `.data` with an updated variable registry.
#' @export
set_role <- function(.data, .cols, role) {
  check_bq_data(.data)
  role <- check_role(role)
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)

  update_roles(.data, names(selected), function(roles) {
    if (identical(role, "auxiliary")) {
      return(roles)
    }
    unique(c(setdiff(roles, "auxiliary"), role))
  })
}

#' Remove an analytical role from variables
#'
#' If the last substantive role is removed, the variable is assigned the
#' default `auxiliary` role.
#'
#' @inheritParams set_role
#'
#' @return `.data` with an updated variable registry.
#' @export
remove_role <- function(.data, .cols, role) {
  check_bq_data(.data)
  role <- check_role(role)
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data)

  update_roles(.data, names(selected), function(roles) {
    roles <- setdiff(roles, role)
    if (length(roles) == 0L) "auxiliary" else roles
  })
}

update_roles <- function(.data, selected_names, update) {
  registry <- attr(.data, "variable_registry", exact = TRUE)
  rows <- match(selected_names, registry$name)

  for (row in rows) {
    registry$role[[row]] <- update(registry$role[[row]])
  }

  attr(.data, "variable_registry") <- registry
  .data
}

check_bq_data <- function(x) {
  if (!inherits(x, "bq_data")) {
    stop_invalid_data("`.data` must be a bq_data object.")
  }
  invisible(x)
}

check_role <- function(role) {
  valid <- is.character(role) && length(role) == 1L && !is.na(role) &&
    role %in% valid_roles

  if (!valid) {
    message <- paste0(
      "`role` must be one of: ",
      paste(valid_roles, collapse = ", "),
      "."
    )
    condition <- structure(
      list(message = message, call = sys.call(-1L)),
      class = c("bq_error_invalid_role", "error", "condition")
    )
    stop(condition)
  }

  role
}
