test_that("analytic functions print their decisions instead of their closure", {
  analysis <- t_test(
    var_equal = TRUE,
    effect_size = "cohens_d",
    bootstrap = bootstrap_control(
      iterations = 49L,
      conf_type = "percentile",
      seed = 12L
    )
  )

  output <- capture.output(visibility <- withVisible(print(analysis)))

  expect_false(visibility$visible)
  expect_identical(visibility$value, analysis)
  expect_identical(output[1L], "<bq analysis function>")
  expect_true("Kind: t_test" %in% output)
  expect_true("var_equal: TRUE" %in% output)
  expect_true(any(grepl("bootstrap: method=", output, fixed = TRUE)))
  expect_true("Dependencies: effectsize, boot" %in% output)
  expect_lt(length(output), 20L)
  expect_false(any(grepl("function (data, context)", output, fixed = TRUE)))
})
