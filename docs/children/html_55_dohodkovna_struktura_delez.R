# get data
df <- read.csv2(here::here("data/041.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[2]], con))

updated <- prep_l$updated
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data



data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_qp(y = ~`value.x`, name = "Bruto poslovni prese\u017eek in razn. doh.",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~`value.y`, name = "Sredstva za zaposlene",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~`value.x.x`, name = "Davki na proizvodnjo in uvoz",  color = I(umar_cols()[4])) |>
  add_lines_qp(y = ~`value.y.y`, name = "Subvencije na proizvodnjo",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              barmode = "relative",
              yaxis = umar_yaxis("Dele\u017e, v %"),
              xaxis = umar_xaxis("A"),
              title = umar_subtitle(updated),
              annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)
