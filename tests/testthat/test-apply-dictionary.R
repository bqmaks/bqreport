test_that("apply_dictionary() applies flat metadata without changing data", {
  data <- as_bq_data(tibble::tibble(
    age = c(40, 55),
    group = c("case", "control"),
    site = factor("A", levels = c("A", "B"))
  ))
  dictionary <- tibble::tibble(
    name = c("age", "group", "site"),
    label = c("Age, years", "Study group", "Centre"),
    role = c("predictor", "outcome", "group"),
    type = c("continuous", "binary", "nominal"),
    event = c(NA, "case", NA),
    reference = c(NA, NA, "B")
  )
  result <- apply_dictionary(data, dictionary)
  registry <- variables_of(result)

  expect_registry_aligned(result)
  expect_identical(tibble::as_tibble(result), tibble::as_tibble(data))
  expect_identical(registry$label, dictionary$label)
  expect_identical(registry$role, dictionary$role)
  expect_identical(registry$type, dictionary$type)
  expect_identical(registry$type_source, rep("dictionary", 3))
  expect_identical(registry$event, c(NA, "case", NA))
  expect_identical(registry$event_source, c(NA, "dictionary", NA))
  expect_identical(registry$reference, c(NA, NA, "B"))
})

test_that("missing dictionary fields leave existing metadata unchanged", {
  data <- labelled_data()
  result <- apply_dictionary(
    data,
    tibble::tibble(name = "age", label = NA_character_, role = "id")
  )

  expect_identical(variables_of(result)$label, variables_of(data)$label)
  expect_identical(variables_of(result)$role, c("id", "group", "outcome"))
  expect_identical(variables_of(result)$type, variables_of(data)$type)
  expect_identical(levels_of(result), levels_of(data))
})

test_that("apply_dictionary() records units and rounding policies", {
  data <- labelled_data()
  dictionary <- tibble::tibble(
    name = c("age", "bmi"),
    unit = c("years", "kg/m^2"),
    rounding = c("decimal", "significant"),
    digits = c(0, 3)
  )

  result <- apply_dictionary(data, dictionary)

  expect_identical(variables_of(result)$unit, c("years", NA, "kg/m^2"))
  expect_identical(
    variables_of(result)$rounding,
    c("decimal", NA, "significant")
  )
  expect_identical(variables_of(result)$digits, c(0L, NA, 3L))
})

test_that("apply_dictionary() validates units and rounding policies", {
  data <- labelled_data()

  expect_error(
    apply_dictionary(data, tibble::tibble(name = "age", unit = "")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(
      data,
      tibble::tibble(name = "age", rounding = "decimal")
    ),
    "both `rounding` and `digits`"
  )
  expect_error(
    apply_dictionary(
      data,
      tibble::tibble(name = "age", rounding = "significant", digits = 0)
    ),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(
      data,
      tibble::tibble(name = "age", rounding = "fixed", digits = 1)
    ),
    class = "bq_error_invalid_dictionary"
  )
})

test_that("dictionary decisions replace inferred decisions", {
  data <- infer_types(as_bq_data(tibble::tibble(group = c("alpha", "zeta"))))
  result <- apply_dictionary(
    data,
    tibble::tibble(name = "group", type = "binary", event = "alpha")
  )

  expect_identical(variables_of(result)$type, "binary")
  expect_identical(variables_of(result)$type_source, "dictionary")
  expect_identical(variables_of(result)$event, "alpha")
  expect_identical(variables_of(result)$event_source, "dictionary")
})

test_that("matching dictionary values preserve explicit provenance", {
  data <- as_bq_data(tibble::tibble(group = c("case", "control")))
  data <- set_type(data, group, type_binary("case"))
  result <- apply_dictionary(
    data,
    tibble::tibble(name = "group", type = "binary", event = "case")
  )

  expect_identical(variables_of(result)$type_source, "explicit")
  expect_identical(variables_of(result)$event_source, "explicit")
})

test_that("dictionary values cannot silently replace explicit decisions", {
  binary_data <- as_bq_data(tibble::tibble(group = c("case", "control")))
  binary_data <- set_type(binary_data, group, type_binary("case"))
  nominal_data <- set_type(binary_data, group, type_nominal("control"))

  expect_error(
    apply_dictionary(
      binary_data,
      tibble::tibble(name = "group", type = "nominal", reference = "control")
    ),
    class = "bq_error_dictionary_conflict"
  )
  expect_error(
    apply_dictionary(
      binary_data,
      tibble::tibble(name = "group", type = "binary", event = "control")
    ),
    class = "bq_error_dictionary_conflict"
  )
  expect_error(
    apply_dictionary(
      nominal_data,
      tibble::tibble(name = "group", type = "nominal", reference = "case")
    ),
    class = "bq_error_dictionary_conflict"
  )
})

test_that("apply_dictionary() validates its flat schema", {
  data <- as_bq_data(tibble::tibble(x = c("a", "b")))

  expect_error(apply_dictionary(data, 1:3), class = "bq_error_invalid_dictionary")
  expect_error(
    apply_dictionary(data, tibble::tibble(label = "X")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", extra = "value")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = 1)),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = c("x", "x"))),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "absent")),
    class = "bq_error_invalid_dictionary"
  )
})

test_that("apply_dictionary() validates roles, types and categorical decisions", {
  data <- as_bq_data(tibble::tibble(x = c("a", "b")))

  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", role = "exposure")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", type = "numeric")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", type = "ordinal")),
    "level_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", type = "binary")),
    "requires a non-missing `event`"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", type = "nominal")),
    "requires a non-missing `reference`"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", type = "binary", event = "c")),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, tibble::tibble(name = "x", event = "a")),
    "has `event` but is not binary"
  )
})

test_that("apply_dictionary() requires bq_data", {
  expect_error(
    apply_dictionary(tibble::tibble(x = 1), tibble::tibble(name = "x")),
    class = "bq_error_invalid_data"
  )
})

test_that("apply_dictionary() expands ordinal levels from a flat dictionary", {
  data <- as_bq_data(tibble::tibble(severity = c("low", "high")))
  dictionary <- tibble::tibble(name = "severity", type = "ordinal")
  level_dictionary <- tibble::tibble(
    name = rep("severity", 3),
    value = c("high", "low", "medium"),
    position = c(3, 1, 2)
  )
  result <- apply_dictionary(data, dictionary, level_dictionary)

  expect_identical(variables_of(result)$type, "ordinal")
  expect_identical(variables_of(result)$type_source, "dictionary")
  expect_identical(
    variable_levels(result),
    tibble::tibble(
      var_id = rep("v001", 3),
      value = c("low", "medium", "high"),
      position = 1:3
    )
  )
})

test_that("dictionary ordinal levels replace inferred levels", {
  data <- as_bq_data(tibble::tibble(
    severity = ordered("medium", levels = c("low", "medium", "high"))
  ))
  data <- infer_types(data)
  result <- apply_dictionary(
    data,
    tibble::tibble(name = "severity", type = "ordinal"),
    tibble::tibble(
      name = rep("severity", 3),
      value = c("high", "medium", "low"),
      position = 1:3
    )
  )

  expect_identical(variables_of(result)$type_source, "dictionary")
  expect_identical(variable_levels(result)$value, c("high", "medium", "low"))
})

test_that("matching ordinal levels preserve explicit provenance", {
  data <- as_bq_data(tibble::tibble(severity = c("low", "high")))
  data <- set_type(data, severity, type_ordinal(c("low", "medium", "high")))
  dictionary <- tibble::tibble(name = "severity", type = "ordinal")
  matching_levels <- tibble::tibble(
    name = rep("severity", 3),
    value = c("low", "medium", "high"),
    position = 1:3
  )

  result <- apply_dictionary(data, dictionary, matching_levels)

  expect_identical(variables_of(result)$type_source, "explicit")
  expect_identical(variable_levels(result), variable_levels(data))
})

test_that("dictionary levels cannot replace an explicit ordinal order", {
  data <- as_bq_data(tibble::tibble(severity = c("low", "high")))
  data <- set_type(data, severity, type_ordinal(c("low", "medium", "high")))
  dictionary <- tibble::tibble(name = "severity", type = "ordinal")
  conflicting_levels <- tibble::tibble(
    name = rep("severity", 3),
    value = c("high", "medium", "low"),
    position = 1:3
  )

  expect_error(
    apply_dictionary(data, dictionary, conflicting_levels),
    class = "bq_error_dictionary_conflict"
  )
})

test_that("level dictionaries require a strict flat schema", {
  data <- as_bq_data(tibble::tibble(severity = c("low", "high")))
  dictionary <- tibble::tibble(name = "severity", type = "ordinal")
  valid <- tibble::tibble(
    name = rep("severity", 3),
    value = c("low", "medium", "high"),
    position = 1:3
  )

  expect_error(apply_dictionary(data, dictionary), "requires `level_dictionary`")
  expect_error(
    apply_dictionary(data, dictionary, valid[, c("name", "value")]),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, dictionary, dplyr::mutate(valid, extra = 1)),
    class = "bq_error_invalid_dictionary"
  )
  expect_error(
    apply_dictionary(data, dictionary, dplyr::mutate(valid, position = c(1, 2, 4))),
    "consecutive from 1"
  )
  expect_error(
    apply_dictionary(data, dictionary, dplyr::mutate(valid, value = c("low", "low", "high"))),
    "duplicate level"
  )
  expect_error(
    apply_dictionary(data, dictionary, dplyr::slice(valid, 1:2)),
    "at least three levels"
  )
})

test_that("level dictionary rows belong only to declared ordinal variables", {
  data <- as_bq_data(tibble::tibble(
    severity = c("low", "high"),
    group = c("case", "control")
  ))
  levels <- tibble::tibble(
    name = rep("group", 3),
    value = c("case", "control", "other"),
    position = 1:3
  )

  expect_error(
    apply_dictionary(
      data,
      tibble::tibble(name = "group", type = "nominal", reference = "control"),
      levels
    ),
    "is not ordinal"
  )
})

test_that("ordinal dictionaries must declare every data category", {
  data <- as_bq_data(tibble::tibble(
    severity = factor("low", levels = c("low", "medium", "high"))
  ))
  dictionary <- tibble::tibble(name = "severity", type = "ordinal")
  levels <- tibble::tibble(
    name = rep("severity", 3),
    value = c("low", "medium", "critical"),
    position = 1:3
  )

  expect_error(
    apply_dictionary(data, dictionary, levels),
    'level "high" absent'
  )
})
