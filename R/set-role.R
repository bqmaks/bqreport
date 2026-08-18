#' Set the analytic role of variables
#'
#' Records how one or more columns will be used in analyses. This changes only
#' the variable registry: column values and all value-dependent metadata stay
#' untouched. Restrictions on how many outcomes, groups or identifiers an
#' analysis accepts belong to that analysis's preflight checks.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more columns.
#' @param role One of `"outcome"`, `"predictor"`, `"group"` or `"id"`.
#'
#' @return `data` with updated role metadata.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(y = 1:3, age = c(40, 55, 61), sex = c("f", "m", "m")))
#' data <- set_role(data, y, "outcome")
#' data <- set_role(data, c(age, sex), "predictor")
#' variables(data)
set_role <- function(data, variables, role) {
  if (!inherits(data, "bq_data")) {
    bq_abort(
      "bq_error_invalid_data",
      sprintf("`data` must be a bq_data object, not %s.", class(data)[1L])
    )
  }

  allowed_roles <- c("outcome", "predictor", "group", "id")

  if (
    missing(role) || !is.character(role) || length(role) != 1L ||
      is.na(role) || !role %in% allowed_roles
  ) {
    bq_abort(
      "bq_error_invalid_role",
      paste0(
        "`role` must be one of \"outcome\", \"predictor\", \"group\" or \"id\"; ",
        "choose the analytic use of the selected variables."
      )
    )
  }

  selection <- resolve_variables(
    data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )

  registry <- attr(data, "variables")
  registry$role[match(selection$var_id, registry$var_id)] <- role
  attr(data, "variables") <- registry

  data
}

#' Mark variables as outcomes
#'
#' A convenience wrapper around [set_role()] that assigns the `"outcome"`
#' role. Restrictions on the number of outcomes belong to each analysis's
#' preflight checks.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more columns.
#'
#' @return `data` with the selected variables marked as outcomes.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(response = 1:3, response_2 = 4:6))
#' set_outcome(data, c(response, response_2))
set_outcome <- function(data, variables) {
  set_role(data, {{ variables }}, "outcome")
}

#' Mark variables as predictors
#'
#' A convenience wrapper around [set_role()] that assigns the `"predictor"`
#' role. Restrictions on the predictors accepted by an analysis belong to that
#' analysis's preflight checks.
#'
#' @param data A `bq_data` object.
#' @param variables A tidyselect expression selecting one or more columns.
#'
#' @return `data` with the selected variables marked as predictors.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, 61), sex = c("f", "m", "m")))
#' set_predictor(data, c(age, sex))
set_predictor <- function(data, variables) {
  set_role(data, {{ variables }}, "predictor")
}
