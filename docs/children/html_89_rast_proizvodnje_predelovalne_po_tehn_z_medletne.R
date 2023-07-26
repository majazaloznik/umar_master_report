# get data
df <- read.csv2(here::here("data/075.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

updated <- max(prep_l$updated)

plot_ly(data, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~value.x,  name = "Predelovalne dejavnosti",  color = I(umar_cols()[3])) |>
  add_lines_mp(data = data, y = ~value.y,  name = "Nizko teh. zaht. dejavnosti",  color = I(umar_cols()[1])) |>
  add_lines_mp(data = data, y = ~value.x.x,  name = "Srednje nizko teh. zaht. dejavnosti",  color = I(umar_cols()[2])) |>
  add_lines_mp(data = data, y = ~value.y.y,  name = "Srednje visoko teh. zaht. dejavnosti",  color = I(umar_cols()[4])) |>
  add_lines_mp(data = data, y = ~value,  name = "Visoko teh. zaht. dejavnost",  color = I(umar_cols()[5])) |>
  umar_layout(
    yaxis = umar_yaxis('Medletna rast, v %'),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle("UMAR"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


