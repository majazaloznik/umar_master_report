# get data
df <- read.csv2(here::here("data/066.csv"), encoding = "UTF-8")
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
  as_tibble() |>
  left_join(data, by = "period") |>
  mutate(dv = value.x/value.y.y * 100,
         bdp = value.x/value.x.x * 100)  -> data


updated <- max(prep_l$updated, prep_l2$updated)

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_qp(y = ~`dv`, name = "Dele\u17e v dodani vrednosti",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~`bdp`, name = "Dele\u017e v BDP-ju",  color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Dele\u017e, v %'),
    xaxis = umar_xaxis("A"),
    title = umar_subtitle(updated, "UMAR", "Transf.: izračun deležev"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)



