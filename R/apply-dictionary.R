#' Apply a variable dictionary
#'
#' Applies flat, name-keyed metadata to a `bq_data` object. The dictionary may
#' contain `label`, `role`, `type`, `event` and `reference` in addition to the
#' required `name` column. Missing values mean "leave this field unchanged".
#' Ordered categories are supplied in a second flat table with one row per
#' level.
#'
#' Dictionary type decisions replace inferred or default decisions, but never
#' silently replace an explicit decision made through a setter.
#'
#' @param data A `bq_data` object.
#' @param dictionary A data frame with one row per variable and a required
#'   character column `name`. Optional columns are `label`, `role`, `type`,
#'   `event` and `reference`.
#' @param level_dictionary `NULL`, or a data frame with columns `name`, `value`
#'   and `position`. It is required for ordinal types in `dictionary`.
#'
#' @return `data` with dictionary metadata applied.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55), group = c("case", "control")))
#' dictionary <- data.frame(
#'   name = c("age", "group"),
#'   label = c("Age, years", "Study group"),
#'   role = c("predictor", "outcome"),
#'   type = c("continuous", "binary"),
#'   event = c(NA, "case")
#' )
#' apply_dictionary(data, dictionary)
apply_dictionary <- function(data, dictionary, level_dictionary = NULL) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  if (!is.data.frame(dictionary)) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf("`dictionary` must be a data frame, not %s.", class(dictionary)[1L])
    )
  }

  allowed_columns <- c("name", "label", "role", "type", "event", "reference")
  unknown_columns <- setdiff(names(dictionary), allowed_columns)

  if (length(unknown_columns) > 0L) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf(
        "Dictionary column `%s` is not supported; use only name, label, role, type, event and reference.",
        unknown_columns[1L]
      )
    )
  }

  if (!"name" %in% names(dictionary)) {
    bq_abort(
      "bq_error_invalid_dictionary",
      "`dictionary` must contain a character column named `name`."
    )
  }

  character_columns <- intersect(c("name", "label", "role", "type"), names(dictionary))
  invalid_character <- character_columns[
    !vapply(dictionary[character_columns], is.character, logical(1))
  ]

  if (length(invalid_character) > 0L) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf("Dictionary column `%s` must be character.", invalid_character[1L])
    )
  }

  scalar_columns <- intersect(c("event", "reference"), names(dictionary))
  invalid_scalar <- scalar_columns[
    !vapply(
      dictionary[scalar_columns],
      function(column) is.atomic(column) && is.null(dim(column)),
      logical(1)
    )
  ]

  if (length(invalid_scalar) > 0L) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf("Dictionary column `%s` must be an atomic vector.", invalid_scalar[1L])
    )
  }

  if (anyNA(dictionary$name) || any(dictionary$name == "")) {
    bq_abort(
      "bq_error_invalid_dictionary",
      "Dictionary `name` values must not be missing or empty."
    )
  }

  duplicated_names <- unique(dictionary$name[duplicated(dictionary$name)])

  if (length(duplicated_names) > 0L) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf("Variable `%s` occurs more than once in `dictionary`.", duplicated_names[1L])
    )
  }

  missing_names <- setdiff(dictionary$name, names(data))

  if (length(missing_names) > 0L) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf("Variable `%s` from `dictionary` is not present in `data`.", missing_names[1L])
    )
  }

  if ("role" %in% names(dictionary)) {
    allowed_roles <- c("outcome", "predictor", "group", "id")
    invalid_roles <- setdiff(stats::na.omit(dictionary$role), allowed_roles)

    if (length(invalid_roles) > 0L) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Role `%s` is not supported in `dictionary`.", invalid_roles[1L])
      )
    }
  }

  if ("type" %in% names(dictionary)) {
    allowed_types <- c(
      "continuous", "count", "binary", "ordinal", "nominal", "date",
      "datetime", "unknown"
    )
    invalid_types <- setdiff(stats::na.omit(dictionary$type), allowed_types)

    if (length(invalid_types) > 0L) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Type `%s` is not supported in `dictionary`.", invalid_types[1L])
      )
    }

  }

  level_dictionary <- validate_level_dictionary(data, dictionary, level_dictionary)

  registry <- attr(data, "variables")
  level_registry <- attr(data, "levels")

  for (dictionary_row in seq_len(nrow(dictionary))) {
    variable_name <- dictionary$name[dictionary_row]
    registry_row <- match(variable_name, registry$name)

    if ("label" %in% names(dictionary) && !is.na(dictionary$label[dictionary_row])) {
      registry$label[registry_row] <- dictionary$label[dictionary_row]
    }

    if ("role" %in% names(dictionary) && !is.na(dictionary$role[dictionary_row])) {
      registry$role[registry_row] <- dictionary$role[dictionary_row]
    }

    dictionary_type <- if ("type" %in% names(dictionary)) {
      dictionary$type[dictionary_row]
    } else {
      NA_character_
    }
    dictionary_event <- if ("event" %in% names(dictionary)) {
      dictionary$event[dictionary_row]
    } else {
      NA
    }
    dictionary_reference <- if ("reference" %in% names(dictionary)) {
      dictionary$reference[dictionary_row]
    } else {
      NA
    }

    has_type <- !is.na(dictionary_type)
    has_event <- length(dictionary_event) == 1L && !is.na(dictionary_event)
    has_reference <- length(dictionary_reference) == 1L && !is.na(dictionary_reference)
    target_type <- if (has_type) dictionary_type else registry$type[registry_row]

    if (has_type && dictionary_type == "binary" && !has_event) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Binary variable `%s` requires a non-missing `event`.", variable_name)
      )
    }

    if (has_type && dictionary_type == "nominal" && !has_reference) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Nominal variable `%s` requires a non-missing `reference`.", variable_name)
      )
    }

    if (has_event && !identical(target_type, "binary")) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Variable `%s` has `event` but is not binary.", variable_name)
      )
    }

    if (has_reference && !identical(target_type, "nominal")) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Variable `%s` has `reference` but is not nominal.", variable_name)
      )
    }

    dictionary_event <- if (has_event) as.character(dictionary_event) else NA_character_
    dictionary_reference <- if (has_reference) {
      as.character(dictionary_reference)
    } else {
      NA_character_
    }

    column <- data[[variable_name]]
    categories <- if (is.factor(column)) {
      levels(column)
    } else {
      unique(as.character(column[!is.na(column)]))
    }

    if (has_event && !dictionary_event %in% categories) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Event \"%s\" is not a value of variable `%s`.", dictionary_event, variable_name)
      )
    }

    if (has_reference && !dictionary_reference %in% categories) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf(
          "Reference \"%s\" is not a value of variable `%s`.",
          dictionary_reference,
          variable_name
        )
      )
    }

    type_is_explicit <- identical(registry$type_source[registry_row], "explicit")

    if (
      has_type && type_is_explicit &&
        !identical(registry$type[registry_row], dictionary_type)
    ) {
      bq_abort(
        "bq_error_dictionary_conflict",
        sprintf(
          "Dictionary type `%s` conflicts with explicit type `%s` for variable `%s`.",
          dictionary_type,
          registry$type[registry_row],
          variable_name
        )
      )
    }

    dictionary_levels <- level_dictionary$value[
      level_dictionary$name == variable_name
    ]

    if (has_type && dictionary_type == "ordinal" && type_is_explicit) {
      var_id <- registry$var_id[registry_row]
      existing_levels <- level_registry[level_registry$var_id == var_id, ]
      existing_levels <- existing_levels[order(existing_levels$position), ]

      if (!identical(existing_levels$value, dictionary_levels)) {
        bq_abort(
          "bq_error_dictionary_conflict",
          sprintf(
            "Dictionary levels conflict with explicit levels for variable `%s`.",
            variable_name
          )
        )
      }
    }

    event_is_explicit <- identical(registry$event_source[registry_row], "explicit")

    if (
      has_event && event_is_explicit &&
        !identical(registry$event[registry_row], dictionary_event)
    ) {
      bq_abort(
        "bq_error_dictionary_conflict",
        sprintf("Dictionary event conflicts with explicit event for variable `%s`.", variable_name)
      )
    }

    reference_is_explicit <- type_is_explicit && !is.na(registry$reference[registry_row])

    if (
      has_reference && reference_is_explicit &&
        !identical(registry$reference[registry_row], dictionary_reference)
    ) {
      bq_abort(
        "bq_error_dictionary_conflict",
        sprintf(
          "Dictionary reference conflicts with explicit reference for variable `%s`.",
          variable_name
        )
      )
    }

    if (has_type && !type_is_explicit) {
      registry$type[registry_row] <- dictionary_type
      registry$type_source[registry_row] <- "dictionary"
      registry$event[registry_row] <- NA_character_
      registry$event_source[registry_row] <- NA_character_
      registry$reference[registry_row] <- NA_character_
      var_id <- registry$var_id[registry_row]
      level_registry <- level_registry[level_registry$var_id != var_id, ]

      if (dictionary_type == "ordinal") {
        level_registry <- dplyr::bind_rows(
          level_registry,
          tibble::tibble(
            var_id = rep(var_id, length(dictionary_levels)),
            value = dictionary_levels,
            position = seq_along(dictionary_levels)
          )
        )
      }
    }

    if (has_event && !event_is_explicit) {
      registry$event[registry_row] <- dictionary_event
      registry$event_source[registry_row] <- "dictionary"
    }

    if (has_reference && !reference_is_explicit) {
      registry$reference[registry_row] <- dictionary_reference
    }
  }

  attr(data, "variables") <- registry
  attr(data, "levels") <- level_registry
  data
}

#' Validate and normalise an ordinal level dictionary
#'
#' @param data A `bq_data` object.
#' @param dictionary The already validated main dictionary.
#' @param level_dictionary A candidate level dictionary or `NULL`.
#'
#' @return A tibble with columns `name`, `value` and `position`, ordered by
#'   variable occurrence in the main dictionary and then by position.
#' @noRd
validate_level_dictionary <- function(data, dictionary, level_dictionary) {
  ordinal_names <- if ("type" %in% names(dictionary)) {
    dictionary$name[!is.na(dictionary$type) & dictionary$type == "ordinal"]
  } else {
    character()
  }

  if (is.null(level_dictionary)) {
    if (length(ordinal_names) > 0L) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Ordinal variable `%s` requires `level_dictionary`.", ordinal_names[1L])
      )
    }

    return(tibble::tibble(
      name = character(),
      value = character(),
      position = integer()
    ))
  }

  if (!is.data.frame(level_dictionary)) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf("`level_dictionary` must be a data frame, not %s.", class(level_dictionary)[1L])
    )
  }

  required_columns <- c("name", "value", "position")

  if (!setequal(names(level_dictionary), required_columns)) {
    bq_abort(
      "bq_error_invalid_dictionary",
      "`level_dictionary` must contain exactly name, value and position."
    )
  }

  if (!is.character(level_dictionary$name)) {
    bq_abort(
      "bq_error_invalid_dictionary",
      "Level dictionary column `name` must be character."
    )
  }

  if (!is.atomic(level_dictionary$value) || !is.null(dim(level_dictionary$value))) {
    bq_abort(
      "bq_error_invalid_dictionary",
      "Level dictionary column `value` must be an atomic vector."
    )
  }

  if (!is.numeric(level_dictionary$position)) {
    bq_abort(
      "bq_error_invalid_dictionary",
      "Level dictionary column `position` must be numeric."
    )
  }

  if (
    anyNA(level_dictionary$name) || any(level_dictionary$name == "") ||
      anyNA(level_dictionary$value) || any(as.character(level_dictionary$value) == "") ||
      anyNA(level_dictionary$position) || any(!is.finite(level_dictionary$position)) ||
      any(level_dictionary$position <= 0) ||
      any(level_dictionary$position != floor(level_dictionary$position))
  ) {
    bq_abort(
      "bq_error_invalid_dictionary",
      "Level dictionary names and values must be non-missing, and positions must be positive integers."
    )
  }

  level_dictionary <- tibble::tibble(
    name = level_dictionary$name,
    value = as.character(level_dictionary$value),
    position = as.integer(level_dictionary$position)
  )

  extra_names <- setdiff(unique(level_dictionary$name), ordinal_names)

  if (length(extra_names) > 0L) {
    bq_abort(
      "bq_error_invalid_dictionary",
      sprintf(
        "Variable `%s` has levels but is not ordinal in `dictionary`.",
        extra_names[1L]
      )
    )
  }

  normalised <- level_dictionary[0, ]

  for (variable_name in ordinal_names) {
    variable_levels <- level_dictionary[level_dictionary$name == variable_name, ]

    if (nrow(variable_levels) < 3L) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Ordinal variable `%s` requires at least three levels.", variable_name)
      )
    }

    if (!setequal(variable_levels$position, seq_len(nrow(variable_levels)))) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf(
          "Positions for ordinal variable `%s` must be consecutive from 1.",
          variable_name
        )
      )
    }

    variable_levels <- variable_levels[order(variable_levels$position), ]

    if (anyDuplicated(variable_levels$value)) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf("Ordinal variable `%s` has duplicate level values.", variable_name)
      )
    }

    column <- data[[variable_name]]
    categories <- if (is.factor(column)) {
      levels(column)
    } else {
      unique(as.character(column[!is.na(column)]))
    }
    undeclared <- setdiff(categories, variable_levels$value)

    if (length(undeclared) > 0L) {
      bq_abort(
        "bq_error_invalid_dictionary",
        sprintf(
          "Variable `%s` contains level \"%s\" absent from `level_dictionary`.",
          variable_name,
          undeclared[1L]
        )
      )
    }

    normalised <- dplyr::bind_rows(normalised, variable_levels)
  }

  normalised
}
