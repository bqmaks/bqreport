test_that("regression reporting formats without mutating numeric estimates", {
  result <- reporting_model_result()
  original <- estimates(result)$estimate
  table <- tbl_regression(result, locale = "ru", digits = 3)

  expect_s3_class(table, "bq_table")
  expect_type(table_body(table)$estimate, "character")
  expect_true(all(c("Оценка", "95% ДИ", "p") %in% table_header(table)$label))
  expect_identical(estimates(result)$estimate, original)
})

test_that("comparison table exposes adjusted p values", {
  data <- as_bq_data(tibble::tibble(y = c(1,2,3,4,5,7), g = factor(rep(c("A","B","C"), each=2)))) |>
    set_outcome(y, type="continuous") |> set_predictor(g, type="nominal", reference="A") |>
    set_comparisons(g, all_pairwise(), adjust="holm")
  result <- data |> plan_analysis(y, g) |> validate_plan(data) |> run_analysis(data)
  table <- tbl_comparison(result)
  expect_true(all(c("contrast", "estimate", "conf_int", "p_adjusted") %in% names(table_body(table))))
})

test_that("reporting rejects unsupported locales", {
  expect_error(tbl_regression(reporting_model_result(), locale = "de"),
    class = "bq_error_invalid_table")
})

test_that("descriptive overall label follows locale", {
  data <- as_bq_data(tibble::tibble(y=1:4)) |>
    set_outcome(y,type="continuous") |> set_descriptive_statistics(y,"{mean}")
  result <- data |> plan_descriptives(y, overall=TRUE) |> validate_plan(data) |> run_analysis(data)
  table <- tbl_descriptive(result, locale="ru")
  expect_true("Все пациенты" %in% table_header(table)$label)
})
