# get data
df <- read.csv2(here::here("data/024.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data


updated <- prep_l$updated

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_qp(y = ~`value.x`,
            name = "Izvoz blaga in storitev", color = I(umar_cols()[2])) |>
  add_lines(y = ~`value.y`,
            name = "Izvoz blaga", color = I(umar_cols()[1])) |>
  add_lines(y = ~`value`,
            name = "Izvoz storitev", color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+10) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis("\u010cetrtletna rast, v %"),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle(updated),
    annotations = initials("NaTJ"))
