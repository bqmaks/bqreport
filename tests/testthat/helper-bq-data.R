# Shared fixtures for the tests below.

# The registry of a bq_data object.
variables_of <- function(x) attr(x, "variables")

# A three-column object whose registry is already filled in, so that tests can
# tell "metadata was carried over" apart from "metadata was blank anyway".
labelled_data <- function() {
  data <- as_bq_data(data.frame(
    age = c(40, 55, 61),
    sex = c("f", "m", "m"),
    bmi = c(22, 31, 27)
  ))

  variables <- variables_of(data)
  variables$label <- c("Age, years", "Sex", "Body mass index")
  variables$role <- c("predictor", "group", "outcome")
  variables$type <- c("continuous", "binary", "continuous")
  attr(data, "variables") <- variables

  data
}

# The invariant every dplyr method has to keep: one registry row per column,
# in column order.
expect_registry_aligned <- function(x) {
  testthat::expect_identical(names(x), variables_of(x)$name)
  testthat::expect_identical(nrow(variables_of(x)), ncol(x))
}
