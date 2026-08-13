test_that("as_bq_data creates a tibble subclass and variable registry", {
  x <- tibble::tibble(age = c(40, 50), group = c("A", "B"))

  out <- as_bq_data(x)
  registry <- variables(out)

  expect_s3_class(out, "bq_data")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), names(x))
  expect_s3_class(registry, "tbl_df")
  expect_identical(registry$name, names(x))
  expect_identical(registry$var_id, paste0("var_", names(x)))
  expect_length(unique(registry$var_id), ncol(x))
  expect_identical(registry$role, list("auxiliary", "auxiliary"))
})

test_that("variable identifiers are stable when bq_data is reconstructed", {
  x <- as_bq_data(tibble::tibble(age = 1:3))

  out <- as_bq_data(x)

  expect_identical(variables(out)$var_id, variables(x)$var_id)
})

test_that("variables returns a copy of the registry", {
  x <- as_bq_data(tibble::tibble(age = 1:3))
  registry <- variables(x)
  registry$name <- "changed"

  expect_identical(variables(x)$name, "age")
})

test_that("as_bq_data rejects inputs that are not data frames", {
  expect_error(as_bq_data(1:3), class = "bq_error_invalid_data")
})

test_that("as_bq_data applies variable metadata without changing data", {
  x <- tibble::tibble(age = c(40.123, 50.456), group = c("A", "B"))
  metadata <- tibble::tibble(
    name = c("age", "group"),
    label = c("Age", "Treatment group"),
    unit = c("years", NA_character_),
    digits = c(1L, 0L)
  )

  out <- as_bq_data(x, metadata = metadata)
  registry <- variables(out)

  expect_identical(names(out), names(x))
  expect_identical(lapply(out, as.vector), lapply(x, as.vector))
  expect_identical(registry$label, metadata$label)
  expect_identical(registry$unit, metadata$unit)
  expect_identical(registry$digits, metadata$digits)
  expect_identical(registry$source, c("dictionary", "dictionary"))
})

test_that("dictionary accepts descriptive statistic template vectors", {
  x <- tibble::tibble(age = c(40, 50), response = c(0, 1))
  metadata <- tibble::tibble(
    name = c("age", "response"),
    descriptive_templates = list(
      c("{mean} ({sd})", "{median} ({q1}; {q3})"),
      "{n} ({p}%)"
    )
  )

  out <- as_bq_data(x, metadata)

  expect_identical(
    variables(out)$descriptive_templates,
    metadata$descriptive_templates
  )
})

test_that("dictionary accepts categorical color specifications", {
  x <- tibble::tibble(arm = factor(c("A", "B")))
  metadata <- tibble::tibble(
    name = "arm", colors = list(c(B = "orange", A = "navy"))
  )
  out <- as_bq_data(x, metadata)
  expect_identical(variable_colors(out, arm), c(A = "navy", B = "orange"))
})

test_that("dictionary validates descriptive statistic templates", {
  x <- as_bq_data(tibble::tibble(age = 1:3))

  expect_error(
    apply_dictionary(
      x,
      tibble::tibble(name = "age", descriptive_templates = "{mean}")
    ),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(
      x,
      tibble::tibble(name = "age", descriptive_templates = list(character()))
    ),
    class = "bq_error_invalid_dictionary"
  )
})

test_that("apply_dictionary updates only supplied non-missing properties", {
  x <- as_bq_data(tibble::tibble(age = 1:3, group = letters[1:3]))
  metadata <- tibble::tibble(
    name = "age",
    label = "Age",
    unit = NA_character_,
    digits = 2L
  )

  out <- apply_dictionary(x, metadata)

  expect_identical(variables(out)$label, c("Age", NA_character_))
  expect_identical(variables(out)$unit, c(NA_character_, NA_character_))
  expect_identical(variables(out)$digits, c(2L, NA_integer_))
  expect_identical(variables(x)$label, c(NA_character_, NA_character_))
})

test_that("dictionary validation catches ambiguous or unknown variables", {
  x <- as_bq_data(tibble::tibble(age = 1:3))

  expect_error(
    apply_dictionary(x, tibble::tibble(name = c("age", "age"))),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(x, tibble::tibble(name = "height")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(x, tibble::tibble(name = "age", digits = -1L)),
    class = "bq_error_invalid_dictionary"
  )
})

test_that("dictionary preserves extensible metadata columns and their types", {
  x <- as_bq_data(tibble::tibble(age = 1:3, group = letters[1:3]))
  metadata <- tibble::tibble(
    name = "age",
    domain = factor("demographics"),
    display_order = 1L,
    format_options = list(list(prefix = "~"))
  )

  out <- apply_dictionary(x, metadata)
  registry <- variables(out)

  expect_s3_class(registry$domain, "factor")
  expect_identical(as.character(registry$domain), c("demographics", NA_character_))
  expect_identical(registry$display_order, c(1L, NA_integer_))
  expect_identical(registry$format_options[[1]], list(prefix = "~"))
  expect_null(registry$format_options[[2]])
})

test_that("dictionary cannot overwrite protected registry columns", {
  x <- as_bq_data(tibble::tibble(age = 1:3))

  expect_error(
    apply_dictionary(x, tibble::tibble(name = "age", var_id = "replacement")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(x, tibble::tibble(name = "age", role = "outcome")),
    class = "bq_error_invalid_dictionary"
  )
})

test_that("dictionary applies labelled metadata to data columns", {
  x <- tibble::tibble(response = c(0, 1, 97))
  metadata <- tibble::tibble(
    name = "response",
    label = "Clinical response",
    value_labels = list(c("No" = 0, "Yes" = 1, "Unknown" = 97)),
    na_values = list(97),
    na_range = list(NULL)
  )

  out <- as_bq_data(x, metadata = metadata)

  expect_identical(as.vector(out$response), x$response)
  expect_identical(labelled::var_label(out$response), "Clinical response")
  expect_identical(
    labelled::val_labels(out$response),
    c("No" = 0, "Yes" = 1, "Unknown" = 97)
  )
  expect_identical(labelled::na_values(out$response), 97)
  expect_null(labelled::na_range(out$response))
  expect_identical(variables(out)$value_labels[[1]], metadata$value_labels[[1]])
})

test_that("dictionary supports ranges of special missing values", {
  x <- tibble::tibble(score = c(1, 95, 99))
  metadata <- tibble::tibble(name = "score", na_range = list(c(90, 99)))

  out <- apply_dictionary(as_bq_data(x), metadata)

  expect_identical(labelled::na_range(out$score), c(90, 99))
})

test_that("dictionary validates labelled metadata", {
  x <- as_bq_data(tibble::tibble(response = c(0, 1)))

  expect_error(
    apply_dictionary(
      x,
      tibble::tibble(name = "response", value_labels = list(c(0, 1)))
    ),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(
      x,
      tibble::tibble(name = "response", na_range = list(c(90, 95, 99)))
    ),
    class = "bq_error_invalid_dictionary"
  )
})

test_that("as_bq_data conservatively infers analytical types", {
  x <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02", NA)),
    datetime = as.POSIXct(
      c("2024-01-01 10:00:00", "2024-01-02 10:00:00", NA),
      tz = "UTC"
    ),
    ordered = ordered(c("low", "high", NA), levels = c("low", "high")),
    binary_factor = factor(c("no", "yes", NA)),
    binary_character = c("no", "yes", NA),
    nominal = c("a", "b", "c"),
    numeric = c(0, 1, 0),
    one_level = c("same", "same", NA)
  )

  registry <- variables(as_bq_data(x))

  expect_identical(
    registry$type,
    c(
      "date", "datetime", "ordinal", "binary", "binary", "nominal",
      "unknown", "unknown"
    )
  )
  expect_identical(
    registry$source,
    c(rep("inferred", 6), "default", "default")
  )
  expect_true(all(registry$status == "review"))
})

test_that("type inference ignores missing values and unused factor levels", {
  x <- tibble::tibble(
    observed_binary = factor(c("a", "b", NA), levels = c("a", "b", "unused")),
    all_missing = c(NA_character_, NA_character_, NA_character_)
  )

  registry <- variables(as_bq_data(x))

  expect_identical(registry$type, c("binary", "unknown"))
})

test_that("set_role adds multiple roles using tidyselect", {
  x <- as_bq_data(tibble::tibble(age = 1:3, treatment = letters[1:3]))

  out <- x |>
    set_role(treatment, "group") |>
    set_role(treatment, "predictor") |>
    set_role(treatment, "predictor")

  expect_identical(variables(out)$role, list("auxiliary", c("group", "predictor")))
  expect_identical(variables(x)$role, list("auxiliary", "auxiliary"))
})

test_that("set_role supports composed tidyselect expressions", {
  x <- as_bq_data(tibble::tibble(age = 1:3, bmi = 2:4, group = letters[1:3]))

  out <- set_role(x, where(is.numeric) & !age, "outcome")

  expect_identical(variables(out)$role, list("auxiliary", "outcome", "auxiliary"))
})

test_that("remove_role removes only the requested role", {
  x <- as_bq_data(tibble::tibble(treatment = letters[1:3])) |>
    set_role(treatment, "group") |>
    set_role(treatment, "predictor")

  one_role <- remove_role(x, treatment, "group")
  no_roles <- remove_role(one_role, treatment, "predictor")

  expect_identical(variables(one_role)$role, list("predictor"))
  expect_identical(variables(no_roles)$role, list("auxiliary"))
})

test_that("role functions validate role values", {
  x <- as_bq_data(tibble::tibble(age = 1:3))

  expect_error(
    set_role(x, age, "exposure"),
    class = "bq_error_invalid_role"
  )
  expect_error(
    remove_role(x, age, "exposure"),
    class = "bq_error_invalid_role"
  )
})
