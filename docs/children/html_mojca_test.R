# get data
df <- read.csv2(here::here("data/mojca_test.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
prep_l3 <- prep_multi_line(spl[[3]], con)
prep_l4 <- prep_multi_line(spl[[4]], con)
prep_l5 <- prep_multi_line(spl[[5]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_lines_mp(y = ~value.x,  name = "Domače prenočitve",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y,  name = "Tuje prenočitve",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value,  name = "Skupaj prenočitve",  color = I(umar_cols()[3])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Index'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


updated <- max(prep_l2$updated)

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

plot_ly(data2, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~value.x,  name = "Lastnik fizična oseba",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y,  name = "Lastnik pravna oseba",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value,  name = "Skupaj prodaja avtov",  color = I(umar_cols()[3])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Index'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))



updated <- max(prep_l3$updated)

purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data3

plot_ly(data3, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~value,  name = "Masa neto plač",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Index'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


updated <- max(prep_l4$updated)

purrr::reduce(prep_l4$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data4

plot_ly(data4, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~value.x,  name = "Trgovina na debelo z živili",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y,  name = "Trgovina na debelo z neživili",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.x.x,  name = "Posredništvo",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value.y.y,  name = "Skupaj trgovina na debelo",  color = I(umar_cols()[3])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Index'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))




updated <- max(prep_l5$updated)

purrr::reduce(prep_l5$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data5

plot_ly(data3, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~value,  name = "Socialni transferji",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Index'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))

