test_that("compile_comparison_family() compiles all supported families", {
  group_values <- c("control", "dose-low", "dose-high")

  pairwise <- compile_comparison_family(group_values, "pairwise")
  reference <- compile_comparison_family(
    group_values, "reference", reference = "control"
  )
  consecutive <- compile_comparison_family(group_values, "consecutive")

  expect_identical(
    pairwise[c("reference_value", "comparison_value")],
    tibble::tibble(
      reference_value = c("control", "control", "dose-low"),
      comparison_value = c("dose-low", "dose-high", "dose-high")
    )
  )
  expect_identical(
    reference[c("reference_value", "comparison_value")],
    tibble::tibble(
      reference_value = c("control", "control"),
      comparison_value = c("dose-low", "dose-high")
    )
  )
  expect_identical(
    consecutive[c("reference_value", "comparison_value")],
    tibble::tibble(
      reference_value = c("control", "dose-low"),
      comparison_value = c("dose-low", "dose-high")
    )
  )
  for (result in list(pairwise, reference, consecutive)) {
    expect_identical(result$comparison_id, sprintf("cmp%03d", seq_len(nrow(result))))
    expect_identical(result$position, seq_len(nrow(result)))
    expect_identical(
      result$direction,
      rep("comparison_minus_reference", nrow(result))
    )
  }
})

test_that("compile_comparison_family() validates family-specific reference", {
  expect_error(
    compile_comparison_family(c("a", "b"), "reference"),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    compile_comparison_family(c("a", "b"), "pairwise", reference = "a"),
    class = "bq_error_invalid_analysis_input"
  )
  expect_error(
    compile_comparison_family(c("a", "b"), "unknown"),
    class = "bq_error_invalid_analysis_function"
  )
})
