# get data
df <- read.csv2(here::here("data/031.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)

# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))
prep_l2 <- invisible(prep_multi_line(spl[[2]], con))

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename( stalne = value) |>
  as_tibble() -> data

updated <- max(prep_l$updated, prep_l2$updated)


purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  rename(tekoce = value) |>
  select(-raw) |>
  left_join(data, by = "period") |>
  as_tibble() -> data2

data2 |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_ap(y = ~`stalne`, name = "BDP - stalne cene",  color = I(umar_cols()[1])) |>
  add_lines_ap(y = ~`tekoce`, name = "BDP - teko\u010de cene",  color = I(umar_cols()[2])) |>
  umar_layout(
    yaxis = umar_yaxis("Medletna rast, v %"),
    xaxis = umar_xaxis("A"),
    title = umar_subtitle("UMAR"),
    annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)


