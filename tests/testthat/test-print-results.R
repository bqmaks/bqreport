# End-to-end fixture: two continuous variables, one binary group, one small
# and one all-missing cell, a summary format and an enumeration rule.
print_example <- function() {
  data <- as_bq_data(data.frame(
    age = c(40, 55, NA, 48),
    bmi = c(22.4, 31.2, 27.8, 24.9),
    arm = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  ))
  data <- set_type(data, age, type_continuous())
  data <- set_type(data, bmi, type_continuous())
  data <- set_type(data, arm, type_binary("B"))
  data <- set_unit(data, age, "years")
  data <- set_rounding(data, c(age, bmi), 1)
  data <- apply_dictionary(
    data,
    data.frame(name = c("age", "bmi"), label = c("Age", "Body mass index"))
  )
  data <- set_summary_format(data, age, c("Mean (SD)" = "{mean} ({sd})"))
  plan <- plan_summary(data, group = arm, overall = "group")
  plan <- add_statistic(plan, c(age, bmi))
  plan <- add_display_rule(plan, bmi, enumerate_values(max_n = 2L))
  plan
}

test_that("preflight, result and table objects print compact summaries", {
  plan <- print_example()
  checked <- preflight(plan)
  result <- run_analysis(plan)
  table <- compose_table(format_presentation(prepare_presentation(result)))

  expect_snapshot(print(checked))
  expect_snapshot(print(result))
  expect_snapshot(print(table))
  capture.output(visibility <- withVisible(print(table)))
  expect_false(visibility$visible)
  expect_identical(visibility$value, table)
})

test_that("as_tibble() lays a composed table out with one column per cell", {
  plan <- print_example()
  table <- compose_table(
    format_presentation(prepare_presentation(run_analysis(plan)))
  )
  wide <- tibble::as_tibble(table)

  expect_s3_class(wide, "tbl_df")
  expect_identical(
    names(wide),
    c("variable", "row", "A (n = 2)", "B (n = 2)", "Overall (n = 4)")
  )
  expect_identical(wide$variable, c("Age, years", rep("Body mass index", 8L)))
  expect_identical(wide$row[1:2], c("Mean (SD)", "mean"))
  expect_identical(wide$row[9L], "values")
  # Age in arm A: 40 and 55; the format row substitutes formatted components.
  expect_identical(wide[["A (n = 2)"]][1L], "47.5 (10.6)")
  # Both bmi cells have n <= 2, so enumeration rows list the raw values.
  expect_identical(wide[["A (n = 2)"]][9L], "22.4, 31.2")
  expect_identical(wide[["Overall (n = 4)"]][9L], NA_character_)
})

test_that("comparison results print their analysis and table sizes", {
  result <- run_comparison(
    t_test(),
    outcome = c(8, 10, 12, 1, 2, 6),
    group = rep(c("new", "control"), each = 3L),
    reference = "control"
  )
  expect_snapshot(print(result))
  expect_snapshot(print(t_test(var_equal = TRUE, effect_size = "hedges_g")))
})
