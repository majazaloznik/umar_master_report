# get data
df <- read.csv(here::here("data/112.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

updated <- max(prep_l$updated)

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_bars_m(y = ~`value.x`, name = "Kazalnik zaupanja potrošnikov",  color = I(umar_cols()[3])) |>
  add_lines_m(y = ~`value.y`, name = "Fin. stanje v gospodinj. v zadnjih 12 m",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~`value.x.x`, name = "Fin. stanje v gospodinj. v prihodnjih 12 m",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~`value.y.y`, name = "Gosp. stanje v Sloveniji v prihodnjih 12 m",  color = I(umar_cols()[5])) |>
  add_lines_m(y = ~`value`, name = "Večji nakupi v prihodnjih 12 m",  color = I(umar_cols()[6])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Ravnotežje, v o.t.'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MoKo")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)


