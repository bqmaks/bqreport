test_that("row-only dplyr verbs preserve registries", {
  x <- as_bq_data(tibble::tibble(id = 3:1, value = c(10, 20, 30))) |>
    set_role(id, "id") |>
    set_outcome(value, type = "continuous")
  original_registry <- variables(x)

  filtered <- dplyr::filter(x, value >= 20)
  sliced <- dplyr::slice(x, 1:2)
  arranged <- dplyr::arrange(x, id)

  expect_s3_class(filtered, "bq_data")
  expect_identical(variables(filtered), original_registry)
  expect_identical(variables(sliced), original_registry)
  expect_identical(variables(arranged), original_registry)
})

test_that("select removes metadata for excluded variables", {
  x <- as_bq_data(tibble::tibble(a = 1:3, b = 4:6, c = 7:9))

  out <- dplyr::select(x, c, a)

  expect_s3_class(out, "bq_data")
  expect_identical(names(out), c("c", "a"))
  expect_identical(variables(out)$name, c("c", "a"))
  expect_identical(
    variables(out)$var_id,
    variables(x)$var_id[match(c("c", "a"), variables(x)$name)]
  )
})

test_that("rename changes names but preserves variable identifiers", {
  x <- as_bq_data(tibble::tibble(old_name = 1:3, other = 4:6)) |>
    set_outcome(old_name, type = "continuous")
  old_id <- variables(x)$var_id[[1]]

  out <- dplyr::rename(x, new_name = old_name)

  expect_identical(names(out), c("new_name", "other"))
  expect_identical(variables(out)$name, c("new_name", "other"))
  expect_identical(variables(out)$var_id[[1]], old_id)
  expect_identical(variables(out)$role[[1]], "outcome")
})

test_that("mutate creates review metadata for new variables", {
  x <- as_bq_data(tibble::tibble(a = 1:3))

  out <- dplyr::mutate(x, doubled = a * 2)
  registry <- variables(out)

  expect_s3_class(out, "bq_data")
  expect_identical(registry$name, c("a", "doubled"))
  expect_identical(registry$role[[2]], "auxiliary")
  expect_identical(registry$type[[2]], "unknown")
  expect_identical(registry$status[[2]], "review")
  expect_false(registry$var_id[[2]] %in% variables(x)$var_id)
})

test_that("mutate invalidates stale metadata for an unlocked variable", {
  x <- as_bq_data(tibble::tibble(group = c("a", "b", "a")))
  registry <- attr(x, "variable_registry")
  registry$distribution <- "gaussian"
  registry$transformation[[1]] <- bqreport::log2_transform()
  registry$reference[[1]] <- "a"
  attr(x, "variable_registry") <- registry

  out <- dplyr::mutate(x, group = as.numeric(factor(group)))
  updated <- variables(out)

  expect_identical(updated$type, "unknown")
  expect_identical(updated$source, "default")
  expect_identical(updated$status, "review")
  expect_true(is.na(updated$distribution))
  expect_null(updated$transformation[[1]])
  expect_null(updated$reference[[1]])
})

test_that("mutate preserves locked type but requires review", {
  x <- as_bq_data(tibble::tibble(value = 1:3)) |>
    set_outcome(value, type = "continuous")

  out <- dplyr::mutate(x, value = as.character(value))
  updated <- variables(out)

  expect_identical(updated$type, "continuous")
  expect_identical(updated$source, "explicit")
  expect_true(updated$locked)
  expect_identical(updated$status, "review")
  expect_identical(updated$storage_type, "character")
})

test_that("base column operations reconstruct variable metadata", {
  x <- as_bq_data(tibble::tibble(a = 1:3, b = 4:6))
  ids <- variables(x)$var_id

  selected <- x[, "b", drop = FALSE]
  names(x)[1] <- "renamed"

  expect_s3_class(selected, "bq_data")
  expect_identical(variables(selected)$name, "b")
  expect_identical(variables(selected)$var_id, ids[[2]])
  expect_identical(variables(x)$name, c("renamed", "b"))
  expect_identical(variables(x)$var_id, ids)
})

test_that("summarise and reframe return ordinary tibbles", {
  x <- as_bq_data(tibble::tibble(value = 1:3))

  summary <- dplyr::summarise(x, mean = mean(value))
  reframed <- dplyr::reframe(x, value = range(value))

  expect_s3_class(summary, "tbl_df")
  expect_false(inherits(summary, "bq_data"))
  expect_s3_class(reframed, "tbl_df")
  expect_false(inherits(reframed, "bq_data"))
})
