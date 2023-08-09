# get data
df <- read.csv2(here::here("data/064.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

prep_l2 <- prep_multi_line(spl[[2]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

prep_l2$data_points[[1]] |>
  as_tibble()  |>
  select(-period_id) |>
  relocate(period) |>
  mutate(quarterly_growth = value/lag(value, n=1)*100-100) |>
  select(-value) |>
  left_join(data, by = "period") |>
  as_tibble() -> data

updated <- max(prep_l$updated, prep_l2$updated)

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_bars_qp(y = ~`value.x`, name = "Medletna rast (originalni podatki)",  color = I(umar_cols()[1])) |>
  add_bars_qp(y = ~`value.y`, name = "Medletna rast (desezonirani podatki)",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~`quarterly_growth`, name = "\u010cetrtletna rast (desezonirani podatki)",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Medletna sprememba, v %'),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)



