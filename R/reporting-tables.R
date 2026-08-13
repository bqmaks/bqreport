reporting_dictionary <- function(locale = c("en", "ru")) {
  locale <- match.arg(locale)
  labels <- list(
    en = c(outcome="Outcome", predictor="Predictor", term="Term", level="Level",
      estimate="Estimate", conf_int="95% CI", p_value="p", p_adjusted="Adjusted p",
      contrast="Contrast", group="Group", time="Time", cause="Cause", method="Method"),
    ru = c(outcome="\u0418\u0441\u0445\u043e\u0434", predictor="\u041f\u0440\u0435\u0434\u0438\u043a\u0442\u043e\u0440", term="\u0422\u0435\u0440\u043c", level="\u0423\u0440\u043e\u0432\u0435\u043d\u044c",
      estimate="\u041e\u0446\u0435\u043d\u043a\u0430", conf_int="95% \u0414\u0418", p_value="p", p_adjusted="\u0421\u043a\u043e\u0440\u0440. p",
      contrast="\u041a\u043e\u043d\u0442\u0440\u0430\u0441\u0442", group="\u0413\u0440\u0443\u043f\u043f\u0430", time="\u0412\u0440\u0435\u043c\u044f", cause="\u041f\u0440\u0438\u0447\u0438\u043d\u0430", method="\u041c\u0435\u0442\u043e\u0434")
  )
  labels[[locale]]
}

check_reporting_locale <- function(locale) {
  if (!is.character(locale) || length(locale) != 1L || !locale %in% c("en", "ru")) {
    stop_invalid_table("`locale` must be `en` or `ru`.")
  }
  locale
}

format_report_number <- function(x, digits, missing = "NA") {
  ifelse(is.na(x) | !is.finite(x), missing, formatC(x, format="f", digits=digits))
}

format_report_p <- function(x, digits, missing = "NA") {
  threshold <- 10^-digits
  ifelse(is.na(x), missing, ifelse(x < threshold,
    paste0("<", formatC(threshold, format="f", digits=digits)),
    formatC(x, format="f", digits=digits)))
}

new_reporting_table <- function(body, columns, locale, class, call) {
  dictionary <- reporting_dictionary(locale)
  header <- tibble::tibble(
    column = columns,
    label = unname(ifelse(columns %in% names(dictionary), dictionary[columns], columns))
  )
  structure(list(table_body=tibble::as_tibble(body), table_header=header,
    footnotes=tibble::tibble(), locale=locale, call=call),
    class=c(class, "bq_table"))
}

#' Build a regression results table
#' @param x An `analysis_result`.
#' @param locale Output locale, `en` or `ru`.
#' @param digits Digits for estimates and confidence limits.
#' @param p_value_digits Digits for p values.
#' @param missing Missing-value text.
#' @return A backend-independent `bq_table`.
#' @export
tbl_regression <- function(x, locale="en", digits=2L, p_value_digits=3L, missing="NA") {
  check_analysis_result(x); locale <- check_reporting_locale(locale)
  digits <- check_table_digits(digits,"digits"); p_value_digits <- check_table_digits(p_value_digits,"p_value_digits")
  ids <- x$plan$analysis_id[x$plan$analysis_type %in% c("univariable_regression","survival_regression")]
  values <- estimates(x); values <- values[values$analysis_id %in% ids, , drop=FALSE]
  if (!nrow(values)) stop_invalid_table("`x` contains no regression estimates.")
  body <- tibble::tibble(
    outcome=values$outcome, predictor=values$predictor, term=values$term,
    level=values$level, estimate=format_report_number(values$estimate,digits,missing),
    conf_int=paste0(format_report_number(values$conf_low,digits,missing), " \u2013 ", format_report_number(values$conf_high,digits,missing)),
    p_value=format_report_p(values$p_value,p_value_digits,missing), method=values$method
  )
  new_reporting_table(body,names(body),locale,"tbl_regression",match.call())
}

#' Build a contrasts/comparisons table
#' @inheritParams tbl_regression
#' @return A backend-independent `bq_table`.
#' @export
tbl_comparison <- function(x, locale="en", digits=2L, p_value_digits=3L, missing="NA") {
  check_analysis_result(x); locale <- check_reporting_locale(locale)
  digits <- check_table_digits(digits,"digits"); p_value_digits <- check_table_digits(p_value_digits,"p_value_digits")
  values <- contrasts(x)
  if (!nrow(values)) stop_invalid_table("`x` contains no contrasts.")
  body <- tibble::tibble(
    outcome=values$outcome, predictor=values$predictor, contrast=values$contrast,
    estimate=format_report_number(values$estimate,digits,missing),
    conf_int=paste0(format_report_number(values$conf_low,digits,missing), " \u2013 ", format_report_number(values$conf_high,digits,missing)),
    p_value=format_report_p(values$p_value,p_value_digits,missing),
    p_adjusted=format_report_p(values$p_adjusted,p_value_digits,missing)
  )
  new_reporting_table(body,names(body),locale,"tbl_comparison",match.call())
}

#' Build a correlation results table
#' @inheritParams tbl_regression
#' @return A backend-independent `bq_table`.
#' @export
tbl_correlation <- function(x, locale="en", digits=2L, p_value_digits=3L, missing="NA") {
  check_analysis_result(x); locale <- check_reporting_locale(locale)
  digits <- check_table_digits(digits,"digits"); p_value_digits <- check_table_digits(p_value_digits,"p_value_digits")
  values <- correlations(x)
  if (!nrow(values)) stop_invalid_table("`x` contains no correlation estimates.")
  body <- tibble::tibble(
    predictor=paste(values$variable_x, values$variable_y, sep=" \u00d7 "), group=values$stratum_label,
    estimate=format_report_number(values$estimate,digits,missing),
    conf_int=paste0(format_report_number(values$conf_low,digits,missing), " \u2013 ", format_report_number(values$conf_high,digits,missing)),
    p_value=format_report_p(values$p_value,p_value_digits,missing), method=values$method
  )
  new_reporting_table(body,names(body),locale,"tbl_correlation",match.call())
}

#' Build a survival estimands table
#' @inheritParams tbl_regression
#' @return A backend-independent `bq_table`.
#' @export
tbl_survival <- function(x, locale="en", digits=2L, p_value_digits=3L, missing="NA") {
  check_analysis_result(x); locale <- check_reporting_locale(locale)
  digits <- check_table_digits(digits,"digits")
  values <- survival_estimates(x)
  if (!nrow(values)) stop_invalid_table("`x` contains no survival estimates.")
  body <- tibble::tibble(
    outcome=values$outcome, group=values$group_level, cause=values$cause,
    time=format_report_number(values$time,digits,missing), term=values$estimate_type,
    estimate=format_report_number(values$estimate,digits,missing),
    conf_int=paste0(format_report_number(values$conf_low,digits,missing), " \u2013 ", format_report_number(values$conf_high,digits,missing)),
    method=values$method
  )
  new_reporting_table(body,names(body),locale,"tbl_survival",match.call())
}

#' Build a longitudinal fixed-effects table
#' @inheritParams tbl_regression
#' @return A backend-independent `bq_table`.
#' @export
tbl_longitudinal <- function(x, locale="en", digits=2L, p_value_digits=3L, missing="NA") {
  check_analysis_result(x)
  ids <- x$plan$analysis_id[x$plan$analysis_type == "longitudinal_regression"]
  copy <- x; copy$plan <- x$plan[x$plan$analysis_id %in% ids, , drop=FALSE]
  copy$plan$analysis_type <- "univariable_regression"
  copy$estimates <- x$estimates[x$estimates$analysis_id %in% ids, , drop=FALSE]
  table <- tbl_regression(copy, locale, digits, p_value_digits, missing)
  class(table) <- c("tbl_longitudinal", "bq_table")
  table
}
