# get data
df <- read.csv(here::here("data/109.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data



plot_ly(data, x = ~period, width = 1000) |>
  add_lines_mp(y = ~value.x,  name = "Javni - staro",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.y.y,  name = "Javni - novo",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.x.x,  name = "Skupaj - staro",  color = I("#c0c9d1")) |>
  add_lines_mp(y = ~value.y.y.y,  name = "Skupaj - novo",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value.y,  name = "Zasebni - staro",  color = I("#326da8")) |>
  add_lines_mp(y = ~value.x.x.x,  name = "Zasebni - novo",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Bruto plača, v EUR'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, NULL, prep_l$transf_txt),
              annotations = initials("MZ")) |>
  rangeslider(as.Date("2022-01-01"), max(data$period))


prep_l <- prep_multi_line(spl[[2]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data
data <- data |>
  mutate(across(starts_with("value"),
                ~ . - 100))


plot_ly(data, x = ~period, width = 1000) |>
  add_lines_mp(y = ~value.x,  name = "Javni - staro",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.y.y,  name = "Javni - novo",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.x.x,  name = "Skupaj - staro",  color = I("#c0c9d1")) |>
  add_lines_mp(y = ~value.y.y.y,  name = "Skupaj - novo",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value.y,  name = "Zasebni - staro",  color = I("#326da8")) |>
  add_lines_mp(y = ~value.x.x.x,  name = "Zasebni - novo",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Medletna realna rast, v %'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, NULL, prep_l$transf_txt),
              annotations = initials("MZ")) |>
  rangeslider(as.Date("2022-01-01"), max(data$period))
