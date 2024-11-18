# get data
df <- read.csv2(here::here("data/108.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data


plot_ly(data, x = ~period, width = 1000,
        height = 600) |>
  add_lines_qp(y = ~value.x,  name = "Pomanjkanje delavcev na splošno",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~value.y,  name = "Pomnanjkanje usposobljenih delavcev",  color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Delež podjetij, v %'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MiPe")) |>
  rangeslider(as.Date("2011-01-01"), max(data$period))

