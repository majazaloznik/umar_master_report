# inflacija
df <- read.csv2(here::here("data/015.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))
updated <- prep_l$updated

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  as_tibble() |>
  mutate(value = value - 100) -> data
updated <- prep_l$updated
data |>
  plot_ly(x = ~period, hovertemplate="%{x|%b %Y} %{y:.2f}%", width = 1000) |>
  add_bars_mp(y = ~`value`, name = "Medletna rast cen \u017eivljenskih potreb\u0161\u010din",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
         yaxis = umar_yaxis("Medletna sprememba, v %"),
         xaxis = umar_xaxis("M"),
         title = umar_subtitle(updated)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))



