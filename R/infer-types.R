#' Infer analytic types for data columns
#'
#' Fills analytic metadata for selected columns whose type is still missing.
#' Existing type decisions are left untouched regardless of their source. The
#' data itself is never converted or otherwise modified.
#'
#' Inference records its provenance in `type_source`. Binary events carry their
#' own source, distinguishing events dictated by a known coding from defaults
#' chosen by category order. An inferred ordinal order is expanded into the
#' separate flat level registry.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more columns.
#'   Defaults to all columns.
#' @param max_levels A positive whole number giving the largest number of
#'   categories still read as `"nominal"`.
#'
#' @return `data` with inferred metadata added where type was missing.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55), event = c(0, 1)))
#' data <- infer_types(data)
#' variables(data)
infer_types <- function(data, variables = tidyselect::everything(), max_levels = 20L) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  selection <- resolve_variables(
    data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )

  registry <- attr(data, "variables")
  level_registry <- attr(data, "levels")

  for (selection_row in seq_len(nrow(selection))) {
    variable_name <- selection$name[selection_row]
    var_id <- selection$var_id[selection_row]
    row <- match(var_id, registry$var_id)

    if (!is.na(registry$type[row])) {
      next
    }

    metadata <- infer_type_metadata(data[[selection$position[selection_row]]], max_levels)

    registry$type[row] <- metadata$type
    registry$event[row] <- metadata$event
    registry$event_source[row] <- metadata$event_source
    registry$reference[row] <- NA_character_
    registry$type_source[row] <- "inferred"

    level_registry <- level_registry[level_registry$var_id != var_id, ]

    if (length(metadata$levels) > 0L) {
      level_registry <- dplyr::bind_rows(
        level_registry,
        tibble::tibble(
          var_id = rep(var_id, length(metadata$levels)),
          value = metadata$levels,
          position = seq_along(metadata$levels)
        )
      )
    }
  }

  attr(data, "variables") <- registry
  attr(data, "levels") <- level_registry
  data
}
