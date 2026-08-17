# Shared fixtures for the tests below.

# The registry of a bq_data object.
variables_of <- function(x) attr(x, "variables")

# The ordered levels attached to variables of a bq_data object.
levels_of <- function(x) attr(x, "levels")

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
  variables$event <- c(NA, "m", NA)
  variables$reference <- c(NA, "f", NA)
  variables$type_source <- rep("explicit", 3)
  attr(data, "variables") <- variables
  attr(data, "levels") <- tibble::tibble(
    var_id = "v002",
    value = c("f", "m"),
    position = 1:2
  )

  data
}

# The invariants every dplyr method has to keep: the object is still a tibble
# subclass, and the registry holds one row per column, in column order.
expect_registry_aligned <- function(x) {
  testthat::expect_s3_class(
    x,
    c("bq_data", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  testthat::expect_identical(names(x), variables_of(x)$name)
  testthat::expect_identical(nrow(variables_of(x)), ncol(x))
  testthat::expect_true(all(levels_of(x)$var_id %in% variables_of(x)$var_id))
}
