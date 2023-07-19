# get data
df <- read.csv2(here::here("data/006.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))
# dygraph_plotter_mixed(prep_l)

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

data |>
  plot_ly(x = ~period,  width = 1000) |>
  add_lines_qp(y = ~`value`, name = "Predelovalne dejavnostni - stopnja rasti obsega",  color = I(umar_cols()[1])) |>
  umar_layout(
    yaxis = umar_yaxis("Medletna rast, v %"),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle(),
    annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100)


