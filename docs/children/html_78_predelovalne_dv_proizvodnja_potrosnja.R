# get data
df <- read.csv2(here::here("data/065.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

updated <- max(prep_l$updated)

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_bars_qp(y = ~`value.x`, name = "Dodana vrednost",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~`value.y`, name = "Proizvodnja",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~`value`, name = "Vmesna potro\u0161nja",  color = I(umar_cols()[4])) |>
  umar_layout(
    yaxis = umar_yaxis('Letna rast, v %'),
    xaxis = umar_xaxis("A"),
    title = umar_subtitle("UMAR"),
   annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)



