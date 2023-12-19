# get data
df <- read.csv2(here::here("data/002.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))
updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_qp(y = ~`value.x`, name = "Originalni podatki",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~`value.y`, name = "Desezonirani podatki",  color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Medletna rast, v %'),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle(updated),
    annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)



