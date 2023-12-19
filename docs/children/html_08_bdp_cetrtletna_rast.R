# get data
df <- read.csv2(here::here("data/009.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))
# dygraph_plotter_mixed(prep_l)

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

data |>
  plot_ly(x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000) |>
  add_bars_qp(y = ~`value.y`, name = "Desezonirani podatki",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              barmode = "relative",
    yaxis = umar_yaxis('\u010cetrtletna, v %'),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle(updated),
    annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)

