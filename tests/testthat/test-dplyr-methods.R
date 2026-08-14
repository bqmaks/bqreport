test_that("row verbs leave the registry alone", {
  data <- labelled_data()

  for (result in list(
    dplyr::filter(data, age > 45),
    dplyr::arrange(data, age),
    dplyr::slice(data, 1:2),
    utils::head(data, 2)
  )) {
    expect_s3_class(result, "bq_data")
    expect_identical(variables_of(result), variables_of(data))
  }
})

test_that("select() keeps the rows of the columns it kept, in their new order", {
  data <- labelled_data()

  kept <- dplyr::select(data, bmi, age)
  expect_registry_aligned(kept)
  expect_identical(variables_of(kept)$var_id, c("v003", "v001"))
  expect_identical(variables_of(kept)$type, c("continuous", "continuous"))

  dropped <- dplyr::select(data, -sex)
  expect_identical(variables_of(dropped)$var_id, c("v001", "v003"))
})

test_that("relocate() reorders the registry with the columns", {
  moved <- dplyr::relocate(labelled_data(), bmi)

  expect_registry_aligned(moved)
  expect_identical(variables_of(moved)$var_id, c("v003", "v001", "v002"))
})

test_that("rename() renames a registry row instead of replacing it", {
  renamed <- dplyr::rename(labelled_data(), years = age)

  expect_registry_aligned(renamed)
  expect_identical(variables_of(renamed)$name, c("years", "sex", "bmi"))
  # The identifier and everything attached to it survive the new name.
  expect_identical(variables_of(renamed)$var_id, c("v001", "v002", "v003"))
  expect_identical(variables_of(renamed)$label[1], "Age, years")
  expect_identical(variables_of(renamed)$role[1], "predictor")
})

test_that("rename() survives a preceding select()", {
  result <- dplyr::rename(dplyr::select(labelled_data(), bmi, age), index = bmi)

  expect_registry_aligned(result)
  expect_identical(variables_of(result)$var_id, c("v003", "v001"))
  expect_identical(variables_of(result)$name, c("index", "age"))
})

test_that("mutate() gives a new column a blank row with a fresh identifier", {
  added <- dplyr::mutate(labelled_data(), waist = c(80, 95, 88))

  expect_registry_aligned(added)
  expect_identical(variables_of(added)$var_id[4], "v004")
  expect_identical(variables_of(added)$type[4], NA_character_)
  expect_identical(variables_of(added)$role[4], NA_character_)
})

test_that("mutate() over an existing column invalidates its type only", {
  rewritten <- dplyr::mutate(labelled_data(), bmi = round(bmi))
  variables <- variables_of(rewritten)

  expect_registry_aligned(rewritten)
  # type describes the values, which have just changed.
  expect_identical(variables$type, c("continuous", "binary", NA_character_))
  # label and role state intent and are independent of the values.
  expect_identical(variables$label[3], "Body mass index")
  expect_identical(variables$role[3], "outcome")
  expect_identical(variables$var_id[3], "v003")
})

test_that("mutate() dropping a column drops its registry row", {
  dropped <- dplyr::mutate(labelled_data(), sex = NULL)

  expect_registry_aligned(dropped)
  expect_identical(variables_of(dropped)$var_id, c("v001", "v003"))
})

test_that("columns arriving from joins and binds get blank rows", {
  data <- labelled_data()

  joined <- dplyr::left_join(
    data,
    tibble::tibble(sex = c("f", "m"), centre = c("A", "B")),
    by = "sex"
  )
  expect_registry_aligned(joined)
  expect_identical(variables_of(joined)$var_id[4], "v004")
  expect_identical(variables_of(joined)$type[4], NA_character_)

  bound <- dplyr::bind_cols(data, tibble::tibble(centre = c("A", "B", "A")))
  expect_registry_aligned(bound)
  expect_identical(variables_of(bound)$var_id[4], "v004")
})

test_that("bind_rows() keeps the registry of the first argument", {
  data <- labelled_data()

  expect_identical(variables_of(dplyr::bind_rows(data, data)), variables_of(data))
})

test_that("an identifier is never reused by a later column of the same name", {
  data <- labelled_data()

  restored <- dplyr::mutate(dplyr::select(data, -sex), sex = c("m", "f", "m"))

  # sex is a different column now, so it must not inherit v002.
  expect_identical(variables_of(restored)$var_id, c("v001", "v003", "v004"))
  expect_identical(variables_of(restored)$role[3], NA_character_)
})

test_that("[ follows the columns and steps aside when it drops to a vector", {
  data <- labelled_data()

  expect_identical(variables_of(data[2])$var_id, "v002")
  expect_identical(variables_of(data[1, 3])$var_id, "v003")
  expect_type(data[, 1, drop = TRUE], "double")
})

test_that("as_tibble() removes the class and the registry", {
  plain <- tibble::as_tibble(labelled_data())

  expect_false(inherits(plain, "bq_data"))
  expect_null(attr(plain, "variables"))
  expect_s3_class(plain, "tbl_df")
})

test_that("group_by() refuses to silently drop the registry", {
  expect_error(
    dplyr::group_by(labelled_data(), sex),
    class = "bq_error_unsupported_operation"
  )
})
