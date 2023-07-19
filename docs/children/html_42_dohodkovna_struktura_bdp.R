# get data
df <- read.csv2(here::here("data/028.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

fig1 <- plot_ly(data, x = ~period,  width = 1000, height=600) |>
  add_lines_qp(y = ~value.x,  name = "Nominalni BDP",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~value.y,   name = "Bruto poslovni prese\u017eek/raznovrstni dohodek",color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~value.x.x,  name = "Sredstva za zaposlene",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~value.y.y, name = "Davki na proizvodnjo in uvoz", color = I(umar_cols()[4]))

fig1 <- add_empty_lines(fig1, 9)


fig2 <- plot_ly(data, x = ~period, width = 1000, height=600) |>
  add_lines_qp(y = ~value.x,  name = "Nominalni BDP",  color = I(umar_cols()[1]) ) |>
  add_lines_qp(y = ~value,  name = "Subvencije na proizvodnjo",  color = I(umar_cols()[5]))

  subplot(fig1,  fig2, nrows = 2, shareX = TRUE) |>
  umar_layout(
    yaxis = umar_yaxis("Medletna rast, v %"),
    yaxis2 = umar_yaxis("Medletna rast, v %"),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle("UMAR"),
    annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))

