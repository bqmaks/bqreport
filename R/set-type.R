#' Set the analytic type of a variable
#'
#' Associates one column of a `bq_data` object with a type specification. The
#' column itself is left unchanged; the decision and its sources are recorded
#' in the variable registry, while an ordinal order is expanded into the
#' separate flat level registry.
#'
#' @param data A `bq_data` object.
#' @param variable A tidyselect expression selecting exactly one column.
#' @param type A `bq_type` specification created by [type_continuous()], [type_count()],
#'   [type_binary()], [type_ordinal()] or [type_nominal()].
#'
#' @return `data` with updated analytic metadata.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55), group = c("case", "control")))
#' data <- set_type(data, age, type_continuous())
#' data <- set_type(data, group, type_binary("case"))
#' variables(data)
set_type <- function(data, variable, type) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  if (missing(type)) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`type` is required; supply a specification created by a type constructor."
    )
  }

  expected_fields <- c("type", "event", "reference", "levels")
  valid_type <- inherits(type, "bq_type") &&
    identical(names(type), expected_fields) &&
    is.character(type$type) && length(type$type) == 1L && !is.na(type$type) &&
    type$type %in% c("continuous", "count", "binary", "ordinal", "nominal") &&
    is.character(type$event) && length(type$event) == 1L &&
    is.character(type$reference) && length(type$reference) == 1L &&
    is.character(type$levels)

  if (valid_type) {
    valid_type <- switch(
      type$type,
      continuous = is.na(type$event) && is.na(type$reference) &&
        length(type$levels) == 0L,
      count = is.na(type$event) && is.na(type$reference) &&
        length(type$levels) == 0L,
      binary = !is.na(type$event) && is.na(type$reference) &&
        length(type$levels) == 0L,
      ordinal = is.na(type$event) && is.na(type$reference) &&
        length(type$levels) >= 3L && !anyNA(type$levels) &&
        !anyDuplicated(type$levels),
      nominal = is.na(type$event) && !is.na(type$reference) &&
        length(type$levels) == 0L,
      FALSE
    )
  }

  if (!valid_type) {
    bq_abort(
      "bq_error_invalid_type_spec",
      "`type` must be a bq_type specification created by a type constructor."
    )
  }

  selection <- resolve_variables(
    data,
    rlang::enquo(variable),
    argument = "variable",
    min = 1L,
    max = 1L
  )

  variable_name <- selection$name
  column <- data[[selection$position]]
  categories <- if (is.factor(column)) {
    levels(column)
  } else {
    unique(as.character(column[!is.na(column)]))
  }

  if (type$type == "binary" && !type$event %in% categories) {
    bq_abort(
      "bq_error_type_mismatch",
      sprintf(
        "Event %s is not a value of variable `%s`; choose an event present in the data.",
        encodeString(type$event, quote = '"'),
        variable_name
      )
    )
  }

  if (type$type == "nominal" && !type$reference %in% categories) {
    bq_abort(
      "bq_error_type_mismatch",
      sprintf(
        paste0(
          "Reference %s is not a value of variable `%s`; choose a reference ",
          "present in the data."
        ),
        encodeString(type$reference, quote = '"'),
        variable_name
      )
    )
  }

  if (type$type == "ordinal") {
    undeclared <- setdiff(categories, type$levels)

    if (length(undeclared) > 0L) {
      bq_abort(
        "bq_error_type_mismatch",
        sprintf(
          paste0(
            "Variable `%s` contains level %s that is absent from `levels`; ",
            "declare every level in order."
          ),
          variable_name,
          encodeString(undeclared[1L], quote = '"')
        )
      )
    }
  }

  variables <- attr(data, "variables")
  row <- match(variable_name, variables$name)
  var_id <- variables$var_id[row]
  variables$type[row] <- type$type
  variables$event[row] <- type$event
  variables$event_source[row] <- if (is.na(type$event)) NA_character_ else "explicit"
  variables$reference[row] <- type$reference
  variables$type_source[row] <- "explicit"
  attr(data, "variables") <- variables

  level_registry <- attr(data, "levels")
  level_registry <- level_registry[level_registry$var_id != var_id, ]

  if (length(type$levels) > 0L) {
    level_registry <- dplyr::bind_rows(
      level_registry,
      tibble::tibble(
        var_id = rep(var_id, length(type$levels)),
        value = type$levels,
        position = seq_along(type$levels)
      )
    )
  }

  attr(data, "levels") <- level_registry
  data
}
