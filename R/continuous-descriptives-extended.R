#' Declare extended continuous descriptive statistics
#'
#' Creates a built-in continuous statistic containing the basic components
#' from [continuous_descriptives()] followed by `iqr`, `mad`, `skewness` and
#' `excess_kurtosis`. Missing values are omitted before calculation.
#'
#' IQR is the difference between the type-7 quartiles used by the basic set.
#' MAD uses the default consistency constant from [stats::mad()]. Skewness and
#' excess kurtosis use the adjusted type-2 estimators from `datawizard`, which
#' correspond to the estimators commonly reported by SAS and SPSS. They return
#' missing values below three and four observations respectively, or when the
#' observed values have zero dispersion.
#'
#' The two moment components are dimensionless and default to two decimal
#' places. This policy can be replaced with [set_component_rounding()].
#'
#' @return A `bq_continuous_statistic` specification.
#' @export
#' @examples
#' if (requireNamespace("datawizard", quietly = TRUE)) {
#'   data <- as_bq_data(data.frame(age = c(40, 55, 61, NA)))
#'   data <- set_type(data, age, continuous())
#'   data <- set_rounding(data, age, 1)
#'   plan <- plan_summary(data) |>
#'     add_statistic(age, continuous_descriptives_extended())
#'   run_analysis(plan)
#' }
continuous_descriptives_extended <- function() {
  if (!requireNamespace("datawizard", quietly = TRUE)) {
    bq_abort(
      "bq_error_missing_dependency",
      paste0(
        "`continuous_descriptives_extended()` requires the suggested ",
        "package `datawizard`; install it with ",
        "`install.packages(\"datawizard\")`."
      )
    )
  }

  basic_fun <- continuous_descriptives()$fun
  statistic <- continuous_statistic(
    "descriptives_extended",
    function(x) {
      basic <- basic_fun(x)
      observed <- x[!is.na(x)]
      n <- length(observed)
      dispersion <- if (n < 2L) NA_real_ else stats::sd(observed)
      has_dispersion <- !is.na(dispersion) && dispersion > 0

      skewness <- NA_real_
      if (n >= 3L && has_dispersion) {
        skewness <- as.double(datawizard::skewness(
          observed,
          remove_na = FALSE,
          type = "2",
          verbose = FALSE
        )$Skewness[1L])
      }

      excess_kurtosis <- NA_real_
      if (n >= 4L && has_dispersion) {
        excess_kurtosis <- as.double(datawizard::kurtosis(
          observed,
          remove_na = FALSE,
          type = "2",
          verbose = FALSE
        )$Kurtosis[1L])
      }

      data.frame(
        basic,
        iqr = as.double(basic$q3 - basic$q1),
        mad = if (n == 0L) {
          NA_real_
        } else {
          as.double(stats::mad(observed))
        },
        skewness = skewness,
        excess_kurtosis = excess_kurtosis
      )
    },
    scale = c(
      mean = "variable",
      sd = "variable",
      median = "variable",
      q1 = "variable",
      q3 = "variable",
      min = "variable",
      max = "variable",
      iqr = "variable",
      mad = "variable",
      skewness = "dimensionless",
      excess_kurtosis = "dimensionless"
    )
  )
  statistic$source <- "built_in_datawizard"
  statistic$missing <- "omit"
  set_component_rounding(
    statistic,
    c("skewness", "excess_kurtosis"),
    digits = 2,
    method = "decimal"
  )
}
