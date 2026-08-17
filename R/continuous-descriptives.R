#' Declare basic continuous descriptive statistics
#'
#' Creates the built-in continuous statistic used by [add_statistic()] unless
#' another specification is supplied through its `statistic` argument.
#' Missing values are omitted before calculation. The returned components are
#' `mean`, `sd`, `median`, `q1`, `q3`, `min` and `max`; all use the source
#' variable's measurement scale and rounding policy.
#'
#' Quartiles use `stats::quantile()` with `type = 7`. A cell without observed
#' values returns missing values for every component. A cell with one observed
#' value returns a missing standard deviation.
#'
#' @return A `bq_continuous_statistic` specification.
#' @export
#' @examples
#' data <- as_bq_data(data.frame(age = c(40, 55, NA)))
#' data <- set_type(data, age, continuous())
#' data <- set_rounding(data, age, 1)
#' plan <- plan_summary(data) |>
#'   add_statistic(age)
#' run_analysis(plan)
continuous_descriptives <- function() {
  statistic <- continuous_statistic(
    "descriptives",
    function(x) {
      observed <- x[!is.na(x)]
      n <- length(observed)

      if (n == 0L) {
        return(data.frame(
          mean = NA_real_,
          sd = NA_real_,
          median = NA_real_,
          q1 = NA_real_,
          q3 = NA_real_,
          min = NA_real_,
          max = NA_real_
        ))
      }

      quartiles <- stats::quantile(
        observed,
        probs = c(0.25, 0.75),
        names = FALSE,
        type = 7
      )
      data.frame(
        mean = as.double(mean(observed)),
        sd = if (n < 2L) NA_real_ else as.double(stats::sd(observed)),
        median = as.double(stats::median(observed)),
        q1 = as.double(quartiles[1L]),
        q3 = as.double(quartiles[2L]),
        min = as.double(min(observed)),
        max = as.double(max(observed))
      )
    }
  )
  statistic$source <- "built_in_raw"
  statistic$missing <- "omit"
  statistic
}
