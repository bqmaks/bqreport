test_that("numbers are continuous unless every value is 0 or 1", {
  expect_identical(infer_type(c(40, 55, 61)), "continuous")
  expect_identical(infer_type(c(1L, 2L, 3L)), "continuous")
  expect_identical(infer_type(c(0, 1, 1, 0)), "binary")
  expect_identical(infer_type(c(0L, 1L, NA)), "binary")
})

test_that("count is never inferred", {
  # Whole non-negative numbers are exactly the ambiguous case: age in years
  # looks the same as a number of events.
  expect_identical(infer_type(c(0, 1, 2, 3, 4)), "continuous")
})

test_that("numbers coded 1/2 are left continuous", {
  # A known limitation: the 1/2 coding common in SPSS exports is not detected,
  # because it cannot be told apart from a genuine measurement.
  expect_identical(infer_type(c(1, 2, 1, 2)), "continuous")
})

test_that("logicals are binary", {
  expect_identical(infer_type(c(TRUE, FALSE, TRUE)), "binary")
  expect_identical(infer_type(c(TRUE, TRUE)), "binary")
})

test_that("a 0/1 or TRUE/FALSE coding is binary even with one value observed", {
  expect_identical(infer_type(c("0", "1", "1")), "binary")
  expect_identical(infer_type(factor(c("0", "1"))), "binary")
  expect_identical(infer_type(c("TRUE", "FALSE")), "binary")
  # Everyone had the event: still a binary column, not a one-level category.
  expect_identical(infer_type(c("1", "1")), "binary")
  expect_identical(infer_type(c(1, 1, 1)), "binary")
})

test_that("two categories are binary and more are nominal", {
  expect_identical(infer_type(c("f", "m")), "binary")
  expect_identical(infer_type(factor(c("f", "m"))), "binary")
  expect_identical(infer_type(c("A", "B", "C")), "nominal")
})

test_that("a single category is nominal, not binary", {
  expect_identical(infer_type(c("only", "only")), "nominal")
})

test_that("ordered factors are ordinal from three categories on", {
  expect_identical(
    infer_type(factor(c("low", "mid", "high"), levels = c("low", "mid", "high"), ordered = TRUE)),
    "ordinal"
  )
  # An order over two categories adds nothing to the two categories.
  expect_identical(
    infer_type(factor(c("mild", "severe"), levels = c("mild", "severe"), ordered = TRUE)),
    "binary"
  )
})

test_that("factors are classified by declared levels, not observed values", {
  x <- factor("a", levels = c("a", "b", "c"))

  expect_identical(infer_type(x), "nominal")
  expect_identical(infer_type(droplevels(x)), "nominal")
})

test_that("too many categories stay unknown", {
  expect_identical(infer_type(letters), "unknown")
  expect_identical(infer_type(letters, max_levels = 26L), "nominal")
  expect_identical(infer_type(letters[1:5], max_levels = 3L), "unknown")
})

test_that("dates are recognised before their numeric payload", {
  expect_identical(infer_type(as.Date(c("2026-01-01", "2026-02-01"))), "date")
  expect_identical(
    infer_type(as.POSIXct(c("2026-01-01 10:00", "2026-02-01 11:00"), tz = "UTC")),
    "datetime"
  )
})

test_that("columns without observations are unknown", {
  expect_identical(infer_type(c(NA, NA)), "unknown")
  expect_identical(infer_type(c(NA_real_, NA_real_)), "unknown")
  expect_identical(infer_type(character(0)), "unknown")
  expect_identical(infer_type(as.Date(NA)), "unknown")
})

test_that("anything else is unknown", {
  expect_identical(infer_type(list(1, "a")), "unknown")
  expect_identical(infer_type(complex(real = 1, imaginary = 1)), "unknown")
})

test_that("known binary codings infer their event", {
  for (x in list(
    c(TRUE, FALSE),
    c(0, 1),
    c("0", "1"),
    factor(c("0", "1"))
  )) {
    metadata <- infer_type_metadata(x)
    expect_identical(metadata$event, if (is.logical(x)) "TRUE" else "1")
    expect_identical(metadata$event_source, "inferred")
  }

  for (x in list(c("FALSE", "TRUE"), factor(c("FALSE", "TRUE")))) {
    metadata <- infer_type_metadata(x)
    expect_identical(metadata$event, "TRUE")
    expect_identical(metadata$event_source, "inferred")
  }
})

test_that("known binary codings infer the potential event when it is unobserved", {
  expect_identical(infer_type_metadata(FALSE)$event, "TRUE")
  expect_identical(infer_type_metadata(0)$event, "1")
  expect_identical(infer_type_metadata("0")$event, "1")
  expect_identical(infer_type_metadata("FALSE")$event, "TRUE")
})

test_that("an arbitrary factor uses its second declared level as the default event", {
  x <- factor("control", levels = c("case", "control"))
  metadata <- infer_type_metadata(x)

  expect_identical(metadata$type, "binary")
  expect_identical(metadata$event, "control")
  expect_identical(metadata$event_source, "default")
})

test_that("an arbitrary character binary uses lexical order for its default event", {
  forward <- infer_type_metadata(c("alpha", "zeta"))
  reversed <- infer_type_metadata(c("zeta", "alpha"))

  expect_identical(forward$event, "zeta")
  expect_identical(reversed$event, "zeta")
  expect_identical(forward$event_source, "default")
  expect_identical(reversed$event_source, "default")
})

test_that("ordinal inference carries the declared level order", {
  x <- ordered("medium", levels = c("low", "medium", "high"))
  metadata <- infer_type_metadata(x)

  expect_identical(
    metadata,
    list(
      type = "ordinal",
      event = NA_character_,
      event_source = NA_character_,
      levels = c("low", "medium", "high")
    )
  )
})

test_that("non-binary inference does not propose an event", {
  for (x in list(c(1, 2, 3), c("a", "b", "c"), as.Date("2026-01-01"))) {
    metadata <- infer_type_metadata(x)
    expect_identical(metadata$event, NA_character_)
    expect_identical(metadata$event_source, NA_character_)
  }
})
