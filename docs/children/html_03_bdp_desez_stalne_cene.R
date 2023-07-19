# get data
df <- read.csv2(here::here("data/003.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))
# dygraph_plotter_mixed(prep_l)
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

prep_l2 <- invisible(prep_multi_line(spl[[2]], con))
# dygraph_plotter_mixed(prep_l)
prep_l2$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  left_join(data, by = "period") |>
  as_tibble() -> data
updated <- max(prep_l$updated, prep_l2$updated)
data |>
  plot_ly(x = ~period,  width = 1000) |>
  add_lines_q(y = ~`value.y`, name = "BDP - stalne cene (2010)",  color = I(umar_cols()[1])) |>
  add_lines_q(y = ~`value.x`, name = "BDP - teko\u010de cene",  color = I(umar_cols()[2])) |>
  umar_layout(
    yaxis = umar_yaxis("Znesek, v mio EUR"),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle()) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


