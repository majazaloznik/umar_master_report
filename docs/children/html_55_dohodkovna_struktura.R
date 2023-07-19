# get data
df <- read.csv2(here::here("data/041.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_ap(y = ~`value.x`, name = "Nominalni BDP",  color = I("black")) |>
  add_lines_ap(y = ~`value.y`, name = "Bruto poslovni prese\u017eek in razn. doh.",  color = I(umar_cols()[1])) |>
  add_lines_ap(y = ~`value.x.x`, name = "Sredstva za zaposlene",  color = I(umar_cols()[2])) |>
  add_lines_ap(y = ~`value.y.y`, name = "Davki na proizvodnjo in uvoz",  color = I(umar_cols()[3]))

fig1 <- add_empty_lines(fig1, 8)

fig2 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_ap(y = ~`value.x`, name = "Nominalni BDP",  color = I("black")) |>
  add_lines_ap(y = ~`value`, name = "Subvencije na proizvodnjo",  color = I(umar_cols()[4]))

subplot(fig1,  fig2, nrows = 2, shareX = TRUE) |>
  umar_layout(barmode = "relative",
              yaxis = umar_yaxis("Medletna rast, v %"),
              yaxis2 = umar_yaxis("Medletna rast, v %"),
              xaxis = umar_xaxis("A"),
              title = umar_subtitle("UMAR"),
              annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)
