# get data
df <- read.csv2(here::here("data/018.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l2 <- prep_multi_line(spl[[15]], con)
updated <- prep_l2$updated

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

plot_ly(data2, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~value.x,  name = "Kazalnik zaupanja",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Poslovni polo\u017eaj",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Povpra\u0161evanje",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y,   name = "Pričakovano povpra\u0161evanje",color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Ravnote\u017eje, v o.t."),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, prep_l2$transf_txt, "UMAR"),
              annotations = initials("MaHr"))|>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))


