# Descriptive analysis and group comparisons

Descriptive templates are variable metadata. They control display rows,
not the numerical computation or rounding of an `analysis_result`.

``` r

trial <- tibble::tibble(
  arm = factor(rep(c("Control", "Treatment"), each = 8)),
  age = c(42, 51, 63, 48, 55, 60, 46, 58, 44, 49, 54, 57, 61, 52, 47, 59),
  response = factor(
    c("No", "No", "Yes", "No", "Yes", "No", "No", "Yes",
      "Yes", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes")
  )
) |>
  as_bq_data() |>
  set_role(arm, "group") |>
  set_predictor(arm, type = "binary", reference = "Control") |>
  set_outcome(age, type = "continuous") |>
  set_outcome(response, type = "binary", event = "Yes") |>
  set_descriptive_statistics(age,
    c("{mean} ({sd})", "{median} ({q1}; {q3})", "MAD: {mad}")) |>
  set_descriptive_statistics(response, "{n}/{N} ({p}%)")

plan <- trial |>
  plan_descriptives(
    variables = c(age, response), groups = arm, overall = TRUE,
    comparisons = TRUE
  ) |>
  validate_plan(trial)

result <- run_analysis(plan, trial)
descriptives(result)
#> # A tibble: 60 × 18
#>    analysis_id     variable_id variable variable_type group_id group group_level
#>    <chr>           <chr>       <chr>    <chr>         <chr>    <chr> <chr>      
#>  1 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  2 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  3 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  4 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  5 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  6 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  7 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  8 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#>  9 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#> 10 analysis_2dbe1… var_dc95be… age      continuous    var_ad2… arm   NA         
#> # ℹ 50 more rows
#> # ℹ 11 more variables: overall <lgl>, level <chr>, statistic <chr>,
#> #   value <dbl>, numerator <int>, denominator <int>, statistic_method <chr>,
#> #   source <chr>, method <chr>, status <chr>, message <chr>
table_body(tbl_descriptive(result, locale = "en"))
#> # A tibble: 5 × 11
#>   analysis_id        variable_id variable variable_label unit  level template_id
#>   <chr>              <chr>       <chr>    <chr>          <chr> <chr>       <int>
#> 1 analysis_2dbe11d0… var_dc95be… age      age            NA    NA              1
#> 2 analysis_2dbe11d0… var_dc95be… age      age            NA    NA              2
#> 3 analysis_2dbe11d0… var_dc95be… age      age            NA    NA              3
#> 4 analysis_e1aa7ad8… var_771926… response response       NA    No              1
#> 5 analysis_e1aa7ad8… var_771926… response response       NA    Yes             1
#> # ℹ 4 more variables: template <chr>, stat_1 <chr>, stat_2 <chr>, stat_3 <chr>
```

The overall column and grouped populations are generated in the same
plan. For categorical outcomes, `n`, `N`, and `p` retain distinct
numerator, denominator, and proportion semantics.

## Custom descriptive function

Custom fields are declared before compilation and returned through a
validated tidy contract.

``` r

coefficient_of_variation <- descriptive_function(
  id = "coefficient_of_variation",
  fields = "cv",
  types = "continuous",
  compute = function(context) {
    tibble::tibble(
      statistic = "cv",
      value = stats::sd(context$values) / mean(context$values),
      statistic_method = "sd_over_mean"
    )
  }
)

custom_data <- trial |>
  set_descriptive_statistics(age, "CV: {cv}")

custom_result <- custom_data |>
  plan_descriptives(age, groups = arm,
    functions = list(coefficient_of_variation)) |>
  validate_plan(custom_data) |>
  run_analysis(custom_data)

table_body(tbl_descriptive(custom_result))
#> # A tibble: 1 × 11
#>   analysis_id        variable_id variable variable_label unit  level template_id
#>   <chr>              <chr>       <chr>    <chr>          <chr> <chr>       <int>
#> 1 analysis_40e76b5d… var_dc95be… age      age            NA    NA              1
#> # ℹ 4 more variables: template <chr>, stat_1 <chr>, stat_2 <chr>, stat_3 <chr>
```

[`shapiro_wilk()`](https://bqmaks.github.io/bqreport/reference/shapiro_wilk.md)
uses the same extension contract. Its result is diagnostic and does not
silently select a downstream model.
