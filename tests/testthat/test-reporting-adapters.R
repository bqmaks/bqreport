test_that("gt adapter preserves bq table labels", {
  skip_if_not_installed("gt")
  table <- tbl_regression(reporting_model_result())
  expect_s3_class(as_gt(table), "gt_tbl")
})

test_that("flextable adapter returns a flextable", {
  skip_if_not_installed("flextable")
  table <- tbl_regression(reporting_model_result())
  expect_s3_class(as_flextable(table), "flextable")
})
