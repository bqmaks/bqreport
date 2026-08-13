test_that("forest plot uses tidy estimates", {
  skip_if_not_installed("ggplot2")
  result <- reporting_model_result()
  plot <- plot_forest(result)
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$estimate, estimates(result)$estimate)
})

test_that("survival plot uses survival curves", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(time=1:5,event=c(1,0,1,0,1))) |>
    add_survival_outcome(os,time,event,1,"months")
  result <- data |> plan_kaplan_meier(os) |> validate_plan(data) |> run_analysis(data)
  plot <- plot_survival(result)
  expect_s3_class(plot,"ggplot")
  expect_true(all(plot$data$estimate_type == "survival_curve"))
})

test_that("survival plot uses categorical colors snapshotted in the plan", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("survival")
  data <- as_bq_data(tibble::tibble(
    time=1:6,event=c(1,0,1,0,1,1),arm=factor(rep(c("A","B"),3))
  )) |>
    set_predictor(arm,type="binary",reference="A") |>
    set_colors(arm,c(A="#112233",B="#CC5500")) |>
    add_survival_outcome(os,time,event,1,"months")
  plan <- plan_kaplan_meier(data,os,groups=arm)
  expect_identical(plan$predictor_color_spec[[1]]$resolved,
    c(A="#112233",B="#CC5500"))
  result <- plan |> validate_plan(data) |> run_analysis(data)
  built <- ggplot2::ggplot_build(plot_survival(result))
  expect_setequal(unique(built$data[[1]]$colour),c("#112233","#CC5500"))
})

test_that("correlation plot builds a symmetric plotting frame", {
  skip_if_not_installed("ggplot2")
  data <- as_bq_data(tibble::tibble(a=1:6,b=c(2,1,4,3,6,5)))
  result <- data |> plan_correlations(a,b) |> validate_plan(data) |> run_analysis(data)
  plot <- plot_correlation(result)
  expect_s3_class(plot,"ggplot")
  expect_equal(nrow(plot$data), 2)
})
