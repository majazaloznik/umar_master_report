# get data
df <- read.csv2(here::here("data/053.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2
prep_l3<- prep_multi_line(spl[[3]], con)
purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data3





fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 800) |>
  add_lines(y = ~value.x,  name = "Skupaj",  color = I(umar_cols()[3]), fill = "tozeroy",
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y,   name = "Nove stanovanjske nepremi\u010dnine ",color = I(umar_cols()[1]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value,  name = "Rabljene stanovanjske nepremi\u010dnine",  color = I(umar_cols()[2]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  umar_layout(annotations = list(x = 0 , y = 1, text = "Vrednost prodaj stanovanjskih nepremi\u010dnin", showarrow = F,
                            xref='paper', yref='paper'))


for(i in 1:10) {
  fig1 <- fig1 |>
    add_lines(y = ~value.x,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}

fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 800) |>
  add_lines(y = ~value.x,  name = "Skupaj",  color = I(umar_cols()[3]), fill = "tozeroy",
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y,   name = "Nova stanovanja",color = I(umar_cols()[4]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value,   name = "Nove dru\u017einske hiše",color = I(umar_cols()[5]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  umar_layout(annotations = list(x = 0. , y = 1, text = "Nove stanovanjske nepremičnine ", showarrow = F,
                            xref='paper', yref='paper'))


for(i in 1:8) {
  fig2 <- fig2 |>
    add_lines(y = ~value.x,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}

fig3 <- plot_ly(data3, x = ~period, width = 1000,
                height = 800) |>
  add_lines_qp(y = ~value.x,  name = "Skupaj",  color = I(umar_cols()[3]), fill = "tozeroy") |>
  add_lines_qp(y = ~value.y,   name = "Rabljena stanovanja",color = I(umar_cols()[6])) |>
  add_lines_qp(y = ~value,  name = "Rabljene dru\u017einske hi\u0161e",  color = I(umar_cols()[7])) |>
  umar_layout(annotations = list(x = 0 , y = 1, text = "Rabljene stanovanjske nepremi\u010dnine", showarrow = F,
                            xref='paper', yref='paper'))



subplot(fig1, fig2, fig3,   nrows = 3, shareX = TRUE) |>
  umar_layout(
    yaxis = umar_yaxis('Medletna rast, v '),
    yaxis2 = umar_yaxis('Medletna rast, v '),
    yaxis3 = umar_yaxis('Medletna rast, v '),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle("UMAR"),
    annotations = initials("MoKo")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))





