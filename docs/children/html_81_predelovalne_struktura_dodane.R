# get data
df <- read.csv2(here::here("data/068.csv"), encoding = "UTF-8")
df <- df |>  arrange(sub_chart)
spl <- split(df, df$sub_chart)
# prepare data
prep_l0 <- prep_multi_line(spl[[1]], con)
prep_l1 <- prep_multi_line(spl[[2]], con)
prep_l2 <- prep_multi_line(spl[[3]], con)

purrr::reduce(prep_l1$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

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

updated <- max(prep_l0$updated, prep_l1$updated, prep_l2$updated)

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_bars_ap(y = ~ratio_value.x,  name = "Proizvodnja \u017eivil",  color = I(umar_cols()[1])) |>
  add_bars_ap(y = ~ratio_value.y,  name = "Proizvodnja pija\u010d",  color = I(umar_cols()[2])) |>
  add_bars_ap(y = ~ratio_value.x.x,  name = "Proizvodnja tekstilij",  color = I(umar_cols()[3])) |>
  add_bars_ap(y = ~ratio_value.y.y,  name = "Proizvodnja obla\u010dil",  color = I(umar_cols()[4])) |>
  add_bars_ap(y = ~ratio_value,  name = "Usnjarstvo",  color = I(umar_cols()[5])) |>
  my_panel_subtitle("Dele\u017e nizko tehnolo\u0161ko zahtevnih dejavnosti v dodani vrednosti C")

fig1 <- add_empty_lines(fig1, 9)


fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_bars_ap(y = ~ratio_value.x,  name = "Lesna industrija",  color = I(umar_cols()[6])) |>
  add_bars_ap(y = ~ratio_value.y,  name = "Papirna industrija",  color = I(umar_cols()[7])) |>
  add_bars_ap(y = ~ratio_value.x.x,  name = "Tiskarstvo",  color = I(umar_cols()[8])) |>
  add_bars_ap(y = ~ratio_value.y.y,  name = "Pohi\u0161tvena industrija",  color = I(umar_cols()[1])) |>
  add_bars_ap(y = ~ratio_value,  name = "Dr. razno. pred. dej.",  color = I(umar_cols()[2]))


subplot(fig1, fig2,   nrows = 2, shareX = TRUE) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Dele\u017e, v %'),
    yaxis2 = umar_yaxis('Dele\u017e, v %'),
    xaxis = umar_xaxis("A"),
    title = umar_subtitle(updated, "UMAR", "Transf: izračun deleža"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


