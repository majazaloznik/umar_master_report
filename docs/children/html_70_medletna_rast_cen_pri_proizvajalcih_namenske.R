# get data
df <- read.csv2(here::here("data/056.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

updated <- max(prep_l$updated)

data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_mp(y = ~`value.x`, name = "Proizvodi za investicije", color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~`value.y`,  name = "Proizvodi za \u0161iroko porabo", color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~`value.x.x`,name = "Surovine", color = I(umar_cols()[4])) |>
  add_lines_mp(y = ~`value.y.y`, name = "Energenti", color = I(umar_cols()[5])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+10) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Medletna sprememba, v %'),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle(updated),
    annotations = initials("MaHa"))

