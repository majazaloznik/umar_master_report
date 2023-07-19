# get data
df <- read.csv2(here::here("data/055.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

updated <- max(prep_l$updated)

data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_mp(y = ~`value.x`, name = "Skupaj", color = I(umar_cols()[3]), fill = "tozeroy") |>
  add_lines_mp(y = ~`value.y`, name = "Doma\u010di trg", color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~`value`,  name = "Tuji trg", color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+10) |>
  umar_layout(
    yaxis = umar_yaxis('Medletna rast, v %'),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle(),
    annotations = initials("MaHa"))|>
  config(modeBarButtonsToAdd = list(dl_button))

