# preflight, result and table objects print compact summaries

    Code
      print(checked)
    Output
      <bq preflight: summary>
      Ready to run: yes
      Diagnostics: 0 blocking, 0 warning
      Cells: 3

---

    Code
      print(result)
    Output
      <bq result: summary>
      diagnostics: 0 rows
      cells: 3 rows
      cell_axes: 3 rows
      cell_rows: 8 rows
      sample_sizes: 6 rows
      estimates: 42 rows

---

    Code
      print(table)
    Output
      <bq table: summary>
      # A tibble: 9 x 5
        variable        row       `A (n = 2)` `B (n = 2)` `Overall (n = 4)`
        <chr>           <chr>     <chr>       <chr>       <chr>            
      1 Age, years      Mean (SD) 47.5 (10.6) 48.0 (NA)   47.7 (7.5)       
      2 Body mass index mean      <NA>        <NA>        26.6             
      3 Body mass index sd        <NA>        <NA>        3.8              
      4 Body mass index median    <NA>        <NA>        26.4             
      5 Body mass index q1        <NA>        <NA>        24.3             
      6 Body mass index q3        <NA>        <NA>        28.7             
      7 Body mass index min       <NA>        <NA>        22.4             
      8 Body mass index max       <NA>        <NA>        31.2             
      9 Body mass index values    22.4, 31.2  27.8, 24.9  <NA>             

# comparison results print their analysis and table sizes

    Code
      print(result)
    Output
      <bq result: comparison>
      Specification: t_test
      tests: 1 row
      estimates: 0 rows
      sample_flow: 2 rows

---

    Code
      print(t_test(var_equal = TRUE, effect_size = "hedges_g"))
    Output
      <bq analysis function>
      Kind: t_test
      var_equal: TRUE
      hypothesis: "two_sided"
      margin_lower: none
      margin_upper: none
      benefit: none
      effect_size: "hedges_g"
      inference: "analytical"
      permutation: none
      bootstrap: none
      conf_level: 0.95
      Dependencies: effectsize

