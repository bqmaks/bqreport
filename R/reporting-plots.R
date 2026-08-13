require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop_missing_reporting_backend("ggplot2")
}

#' Plot model estimates and confidence intervals
#' @param x An `analysis_result`.
#' @param component Either model `estimates` or `contrasts`.
#' @return A `ggplot` object.
#' @export
plot_forest <- function(x, component=c("estimates","contrasts")) {
  check_analysis_result(x); require_ggplot2(); component <- match.arg(component)
  values <- if (component == "estimates") estimates(x) else contrasts(x)
  if (!nrow(values)) stop_invalid_table(paste0("`x` contains no ",component,"."))
  label <- if (component == "estimates") {
    ifelse(is.na(values$level), values$term, paste(values$term, values$level, sep=": "))
  } else values$contrast
  data <- tibble::tibble(label=label, estimate=values$estimate,
    conf_low=values$conf_low, conf_high=values$conf_high,
    effect_measure=values$effect_measure, scale=values$scale)
  reference <- if (all(data$scale == "ratio")) 1 else 0
  plot <- ggplot2::ggplot(data, ggplot2::aes(x=rlang::.data$estimate, y=stats::reorder(rlang::.data$label, rlang::.data$estimate))) +
    ggplot2::geom_vline(xintercept=reference, linetype=2, colour="grey60") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin=rlang::.data$conf_low,xmax=rlang::.data$conf_high), orientation="y", width=.2
    ) +
    ggplot2::geom_point() + ggplot2::labs(x=NULL,y=NULL) + ggplot2::theme_minimal()
  if (reference == 1 && all(data$estimate > 0, na.rm=TRUE)) plot <- plot + ggplot2::scale_x_log10()
  plot
}

#' Plot survival or cumulative-incidence curves
#' @param x An `analysis_result`.
#' @param estimand Curve estimand to display.
#' @param locale Output locale, `en` or `ru`.
#' @return A `ggplot` object.
#' @export
plot_survival <- function(x, estimand=c("survival","cumulative_risk","cumulative_incidence"), locale="en") {
  check_analysis_result(x); require_ggplot2(); estimand <- match.arg(estimand); locale <- check_reporting_locale(locale)
  types <- switch(estimand, survival=c("survival_curve","survival_probability"),
    cumulative_risk=c("cumulative_risk_curve","cumulative_risk"),
    cumulative_incidence="cumulative_incidence")
  data <- survival_estimates(x); data <- data[data$estimate_type %in% types & !is.na(data$time),,drop=FALSE]
  if (!nrow(data)) stop_invalid_table(paste0("`x` contains no ",estimand," curve estimates."))
  data$series <- interaction(data$group_level,data$cause,drop=TRUE,lex.order=TRUE)
  y_label <- list(
    en=c(survival="Survival probability",cumulative_risk="Cumulative risk",cumulative_incidence="Cumulative incidence"),
    ru=c(survival="\u0412\u0435\u0440\u043e\u044f\u0442\u043d\u043e\u0441\u0442\u044c \u0432\u044b\u0436\u0438\u0432\u0430\u043d\u0438\u044f",cumulative_risk="\u041a\u0443\u043c\u0443\u043b\u044f\u0442\u0438\u0432\u043d\u044b\u0439 \u0440\u0438\u0441\u043a",cumulative_incidence="\u041a\u0443\u043c\u0443\u043b\u044f\u0442\u0438\u0432\u043d\u0430\u044f \u0438\u043d\u0446\u0438\u0434\u0435\u043d\u0442\u043d\u043e\u0441\u0442\u044c")
  )[[locale]][[estimand]]
  ggplot2::ggplot(data,ggplot2::aes(x=rlang::.data$time,y=rlang::.data$estimate,colour=rlang::.data$series,group=rlang::.data$series)) +
    ggplot2::geom_step() + ggplot2::geom_ribbon(ggplot2::aes(ymin=rlang::.data$conf_low,ymax=rlang::.data$conf_high,fill=rlang::.data$series),alpha=.15,colour=NA) +
    ggplot2::labs(x=data$time_unit[[1]],y=y_label,colour=NULL,fill=NULL) +
    ggplot2::coord_cartesian(ylim=c(0,1)) + ggplot2::theme_minimal()
}

#' Plot longitudinal change contrasts
#' @param x An `analysis_result`.
#' @param estimand Longitudinal contrast estimand.
#' @param locale Output locale, `en` or `ru`.
#' @return A `ggplot` object.
#' @export
plot_longitudinal <- function(x, estimand=c("change_from_baseline","difference_in_changes"), locale="en") {
  check_analysis_result(x); require_ggplot2(); estimand <- match.arg(estimand); locale <- check_reporting_locale(locale)
  data <- contrasts(x); data <- data[data$estimand == estimand,,drop=FALSE]
  if (!nrow(data)) stop_invalid_table(paste0("`x` contains no `",estimand,"` contrasts."))
  data$series <- ifelse(is.na(data$outer_contrast),data$numerator,data$outer_contrast)
  ggplot2::ggplot(data,ggplot2::aes(x=rlang::.data$modifier_level,y=rlang::.data$estimate,group=rlang::.data$series,colour=rlang::.data$series)) +
    ggplot2::geom_hline(yintercept=if (all(data$scale=="ratio")) 1 else 0,linetype=2,colour="grey60") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=rlang::.data$conf_low,ymax=rlang::.data$conf_high),width=.15) +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::labs(x=if (locale=="ru") "\u0412\u0440\u0435\u043c\u044f" else "Time",y=estimand,colour=NULL) + ggplot2::theme_minimal()
}

#' Plot a correlation heatmap
#' @param x An `analysis_result`.
#' @return A `ggplot` object.
#' @export
plot_correlation <- function(x) {
  check_analysis_result(x); require_ggplot2(); values <- correlations(x)
  if (!nrow(values)) stop_invalid_table("`x` contains no correlation estimates.")
  forward <- tibble::tibble(x=values$variable_x,y=values$variable_y,estimate=values$estimate)
  reverse <- tibble::tibble(x=values$variable_y,y=values$variable_x,estimate=values$estimate)
  data <- vctrs::vec_rbind(forward,reverse)
  ggplot2::ggplot(data,ggplot2::aes(x=rlang::.data$x,y=rlang::.data$y,fill=rlang::.data$estimate)) +
    ggplot2::geom_tile() + ggplot2::scale_fill_gradient2(limits=c(-1,1),low="#2166AC",mid="white",high="#B2182B") +
    ggplot2::coord_equal() + ggplot2::labs(x=NULL,y=NULL,fill="r") + ggplot2::theme_minimal()
}
