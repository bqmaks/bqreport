require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop_missing_reporting_backend("ggplot2")
}

utils::globalVariables(".data")

result_color_mapping <- function(x, data, key) {
  series <- unique(as.character(data[[key]]))
  series <- series[!is.na(series)]
  defaults <- if (length(series)) {
    stats::setNames(grDevices::hcl.colors(length(series), "Dark 3"), series)
  } else character()
  for (analysis_id in unique(data$analysis_id)) {
    row <- match(analysis_id, x$plan$analysis_id)
    if (is.na(row) || !"predictor_color_spec" %in% names(x$plan)) next
    spec <- x$plan$predictor_color_spec[[row]]
    if (is.null(spec)) next
    applicable <- intersect(names(spec$resolved), series)
    defaults[applicable] <- spec$resolved[applicable]
  }
  defaults
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
  color_level <- if (component == "estimates") values$level else values$numerator
  data <- tibble::tibble(analysis_id=values$analysis_id,label=label, level=color_level,
    estimate=values$estimate,
    conf_low=values$conf_low, conf_high=values$conf_high,
    effect_measure=values$effect_measure, scale=values$scale)
  reference <- if (all(data$scale == "ratio")) 1 else 0
  palette <- result_color_mapping(x,data,"level")
  plot <- ggplot2::ggplot(data, ggplot2::aes(x=.data[["estimate"]], y=stats::reorder(.data[["label"]], .data[["estimate"]]),colour=.data[["level"]])) +
    ggplot2::geom_vline(xintercept=reference, linetype=2, colour="grey60") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin=.data[["conf_low"]],xmax=.data[["conf_high"]]), orientation="y", width=.2
    ) +
    ggplot2::geom_point() + ggplot2::labs(x=NULL,y=NULL,colour=NULL) + ggplot2::theme_minimal()
  if (length(palette)) plot <- plot + ggplot2::scale_colour_manual(values=palette,na.value="black")
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
  data$colour_group <- ifelse(is.na(data$group_level), data$cause, data$group_level)
  data$colour_group[is.na(data$colour_group)] <- ".all"
  data$cause_line <- ifelse(is.na(data$cause), ".event", data$cause)
  data$series <- interaction(data$colour_group,data$cause_line,drop=TRUE,lex.order=TRUE)
  palette <- result_color_mapping(x,data,"colour_group")
  y_label <- list(
    en=c(survival="Survival probability",cumulative_risk="Cumulative risk",cumulative_incidence="Cumulative incidence"),
    ru=c(survival="\u0412\u0435\u0440\u043e\u044f\u0442\u043d\u043e\u0441\u0442\u044c \u0432\u044b\u0436\u0438\u0432\u0430\u043d\u0438\u044f",cumulative_risk="\u041a\u0443\u043c\u0443\u043b\u044f\u0442\u0438\u0432\u043d\u044b\u0439 \u0440\u0438\u0441\u043a",cumulative_incidence="\u041a\u0443\u043c\u0443\u043b\u044f\u0442\u0438\u0432\u043d\u0430\u044f \u0438\u043d\u0446\u0438\u0434\u0435\u043d\u0442\u043d\u043e\u0441\u0442\u044c")
  )[[locale]][[estimand]]
  ggplot2::ggplot(data,ggplot2::aes(x=.data[["time"]],y=.data[["estimate"]],colour=.data[["colour_group"]],group=.data[["series"]],linetype=.data[["cause_line"]])) +
    ggplot2::geom_step() + ggplot2::geom_ribbon(ggplot2::aes(ymin=.data[["conf_low"]],ymax=.data[["conf_high"]],fill=.data[["colour_group"]]),alpha=.15,colour=NA) +
    ggplot2::labs(x=data$time_unit[[1]],y=y_label,colour=NULL,fill=NULL,linetype=NULL) +
    ggplot2::scale_colour_manual(values=palette) + ggplot2::scale_fill_manual(values=palette) +
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
  data$colour_group <- sub("@.*$", "", data$numerator)
  palette <- result_color_mapping(x,data,"colour_group")
  ggplot2::ggplot(data,ggplot2::aes(x=.data[["modifier_level"]],y=.data[["estimate"]],group=.data[["series"]],colour=.data[["colour_group"]])) +
    ggplot2::geom_hline(yintercept=if (all(data$scale=="ratio")) 1 else 0,linetype=2,colour="grey60") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[["conf_low"]],ymax=.data[["conf_high"]]),width=.15) +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::labs(x=if (locale=="ru") "\u0412\u0440\u0435\u043c\u044f" else "Time",y=estimand,colour=NULL) +
    ggplot2::scale_colour_manual(values=palette) + ggplot2::theme_minimal()
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
  ggplot2::ggplot(data,ggplot2::aes(x=.data[["x"]],y=.data[["y"]],fill=.data[["estimate"]])) +
    ggplot2::geom_tile() + ggplot2::scale_fill_gradient2(limits=c(-1,1),low="#2166AC",mid="white",high="#B2182B") +
    ggplot2::coord_equal() + ggplot2::labs(x=NULL,y=NULL,fill="r") + ggplot2::theme_minimal()
}
