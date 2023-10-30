# get data
df <- read.csv2(here::here("data/081.csv"), encoding = "UTF-8")
df <- df |>  arrange(sub_chart)
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)



purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

updated <- max(prep_l$updated, prep_l2$updated)

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_lines_m(y = ~value.x,  name = "Izvoz storitev",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value.y,  name = "Uvoz storitev",  color = I(umar_cols()[2])) |>
  layout(yaxis = list( range = c(0, 1100))) |>
  my_panel_subtitle("desezonirano")


fig1 <- add_empty_lines(fig1, 10)


fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_lines_m(y = ~value.x,  name = "Izvoz storitev",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value.y,  name = "Uvoz storitev",  color = I(umar_cols()[2])) |>
  layout(yaxis = list( range = c(0, 1100))) |>
  my_panel_subtitle("3-mesečne drseče sredine - desezonirano")


subplot(fig1, fig2,  nrows = 2, shareX = TRUE) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis( 'mio EUR'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l2$transf_txt),
              annotations = initials("MaHr")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


