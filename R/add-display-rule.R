#' Add a display rule to a summary plan
#'
#' Registers one display-rule specification and assigns it to selected summary
#' variables. The rule controls presentation only and does not change which
#' statistics are computed.
#'
#' @param plan A `bq_plan_summary` object.
#' @param variables A tidyselect expression selecting one or more variables
#'   already included in `plan$variables`.
#' @param rule A `bq_display_rule` specification such as one created by
#'   [enumerate_values()].
#'
#' @return A copy of `plan` with the display rule registered and assigned.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA), bmi = c(22, 31, 27)))
#' plan <- plan_summary(data, c(age, bmi))
#' add_display_rule(plan, c(age, bmi), enumerate_values(max_n = 2L))
add_display_rule <- function(plan, variables, rule) {
  if (!inherits(plan, "bq_plan_summary")) {
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("`plan` must be a bq_plan_summary object, not %s.", class(plan)[1L])
    )
  }

  valid_rule <- inherits(rule, "bq_display_rule") &&
    identical(names(rule), c("kind", "max_n", "display_statistics")) &&
    identical(rule$kind, "enumerate_values") &&
    is.integer(rule$max_n) && length(rule$max_n) == 1L &&
    !is.na(rule$max_n) && rule$max_n > 0L &&
    is.logical(rule$display_statistics) &&
    length(rule$display_statistics) == 1L &&
    !is.na(rule$display_statistics)

  if (!valid_rule) {
    bq_abort(
      "bq_error_invalid_display_rule",
      "`rule` must be a specification created by a display-rule constructor."
    )
  }

  selection <- resolve_variables(
    plan$data,
    rlang::enquo(variables),
    argument = "variables",
    min = 1L
  )
  outside_plan <- setdiff(selection$var_id, plan$variables)

  if (length(outside_plan) > 0L) {
    variable_name <- selection$name[match(outside_plan[1L], selection$var_id)]
    bq_abort(
      "bq_error_invalid_plan",
      sprintf(
        paste0(
          "Variable `%s` is not a summary variable in `plan`; select only ",
          "planned variables."
        ),
        variable_name
      )
    )
  }

  already_assigned <- intersect(
    selection$var_id,
    plan$display_rule_assignments$var_id
  )

  if (length(already_assigned) > 0L) {
    variable_name <- selection$name[
      match(already_assigned[1L], selection$var_id)
    ]
    bq_abort(
      "bq_error_invalid_plan",
      sprintf("Variable `%s` already has a display rule.", variable_name)
    )
  }

  rule_id <- sprintf("r%03d", plan$next_display_rule_number)
  plan$display_rules <- dplyr::bind_rows(
    plan$display_rules,
    tibble::tibble(
      rule_id = rule_id,
      kind = rule$kind,
      max_n = rule$max_n,
      display_statistics = rule$display_statistics
    )
  )
  plan$display_rule_assignments <- dplyr::bind_rows(
    plan$display_rule_assignments,
    tibble::tibble(
      rule_id = rep(rule_id, nrow(selection)),
      var_id = selection$var_id
    )
  )
  plan$next_display_rule_number <- plan$next_display_rule_number + 1L

  plan
}
