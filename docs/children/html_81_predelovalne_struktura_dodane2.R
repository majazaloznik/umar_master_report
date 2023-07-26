# get data
df <- read.csv2(here::here("data/068.csv"), encoding = "UTF-8")
df <- df |>  arrange(sub_chart)
spl <- split(df, df$sub_chart)
# prepare data
prep_l0 <- prep_multi_line(spl[[1]], con)
prep_l <- prep_multi_line(spl[[4]], con)

prep_l2 <- prep_multi_line(spl[[5]], con)

prep_l3 <- prep_multi_line(spl[[6]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data3

prep_l0$data_points[[1]] |>
  as_tibble()  |>
  select(-period_id) |>
  relocate(period) |>
  rename(dv = value) -> dv

data |>
  left_join(dv) |>
  mutate(across(starts_with("value"), ~ . / dv * 100, .names = "ratio_{.col}")) -> data
data2 |>
  left_join(dv) |>
  mutate(across(starts_with("value"), ~ . / dv * 100, .names = "ratio_{.col}")) -> data2
data3 |>
  left_join(dv) |>
  mutate(across(starts_with("value"), ~ . / dv * 100, .names = "ratio_{.col}")) -> data3

updated <- max(prep_l$updated)

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 800) |>
  add_bars_ap(y = ~ratio_value.x,  name = "Gumarska industrija",  color = I(umar_cols()[1])) |>
  add_bars_ap(y = ~ratio_value.y,  name = "Proizvodnja nekovin. mineral. izd.",  color = I(umar_cols()[2])) |>
  add_bars_ap(y = ~ratio_value.x.x,  name = "Proizvodnja kovin",  color = I(umar_cols()[3])) |>
  add_bars_ap(y = ~ratio_value.y.y,  name = "Proizvodnja kovin. izd.",  color = I(umar_cols()[4])) |>
  add_bars_ap(y = ~ratio_value,  name = "Popravila in monta\u017ea",  color = I(umar_cols()[5])) |>
  umar_layout(annotations = list(x = 0 , y = 1,
                                 text = "Dele\u017e srednje nizko tehnološko zahtevnih dejavnosti v dodani vrednosti C", showarrow = F,
                                 xref='paper', yref='paper'))

fig1 <- add_empty_lines(fig1, 7)

fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 800) |>
  add_bars_ap(y = ~ratio_value.x,  name = "Kemi\u010d industrija",  color = I(umar_cols()[6])) |>
  add_bars_ap(y = ~ratio_value.y,  name = "Proiz. el. naprav",  color = I(umar_cols()[7])) |>
  add_bars_ap(y = ~ratio_value.x.x,  name = "Proiz. dr. strojev in naprav",  color = I(umar_cols()[8])) |>
  add_bars_ap(y = ~ratio_value.y.y,  name = "Proiz. mot. vozil in plovil",  color = I(umar_cols()[1])) |>
  add_bars_ap(y = ~ratio_value,  name = "Proiz. dr. vozil in plovil.",  color = I(umar_cols()[2])) |>
  umar_layout(annotations = list(x = 0 , y = 1,
                                 text = "Dele\u017e srednje visoko tehnolo\u0161ko zahtevnih dejavnosti v dodani vrednosti C", showarrow = F,
                                 xref='paper', yref='paper'))

fig2 <- add_empty_lines(fig2, 7)

fig3 <- plot_ly(data3, x = ~period, width = 1000,
                height = 800) |>
  add_bars_ap(y = ~ratio_value.x,  name = "Farmacevtska industrija",  color = I(umar_cols()[3])) |>
  add_bars_ap(y = ~ratio_value.y,  name = "Proiz. IKT opreme",  color = I(umar_cols()[4]))  |>
  umar_layout(annotations = list(x = 0 , y = 1,
                                 text = "Dele\u017e visoko tehnolo\u0161ko zahtevnih dejavnosti v dodani vrednosti C", showarrow = F,
                                 xref='paper', yref='paper'))


subplot(fig1, fig2, fig3,  nrows = 3, shareX = TRUE) |>
  umar_layout(
    yaxis = umar_yaxis('Dele\u017e, v %'),
    yaxis2 = umar_yaxis('Dele\u017e, v %'),
    yaxis3 = umar_yaxis('Dele\u017e, v %'),
    xaxis = umar_xaxis("A"),
    title = umar_subtitle("UMAR"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


