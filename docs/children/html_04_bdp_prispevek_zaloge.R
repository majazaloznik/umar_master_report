# get data
df <- read.csv2(here::here("data/004.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

data |>
  plot_ly(x = ~period,  width = 1000) |>
  add_bars_q(y = ~`value`, name = "Prispevek k rasti obsega BDP",  color = I(umar_cols()[1])) |>
  umar_layout(
    yaxis = umar_yaxis("Prispevek, v o.t."),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle(),
    annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100)



